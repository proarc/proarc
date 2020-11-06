/*
 * Copyright (C) 2017 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cas.lib.proarc.common.imports.audio;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.fedora.AesEditor;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.PageView.PageViewHandler;
import cz.cas.lib.proarc.common.fedora.PageView.PageViewItem;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.FileSet;
import cz.cas.lib.proarc.common.imports.FileSet.FileEntry;
import cz.cas.lib.proarc.common.imports.ImageImporter;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportProcess;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.imports.ImportProfile;
import cz.cas.lib.proarc.common.imports.InputUtils;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.incad.imgsupport.ImageMimeType;
import cz.incad.imgsupport.ImageSupport;
import javax.imageio.stream.FileImageOutputStream;
import javax.ws.rs.core.MediaType;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_NEW;


/**
 * @author Lukas Sykora
 */
public class WaveImporter implements ImageImporter {


    private static final Logger LOG = Logger.getLogger(WaveImporter.class.getName());
    private final ImportBatchManager ibm;

    public WaveImporter(ImportBatchManager ibm) {
        this.ibm = ibm;
    }

    public boolean accept(FileSet fileSet) {
        return isWave(fileSet);
    }

    public BatchItemObject consume(FileSet fileSet, ImportProcess.ImportOptions ctx) {
        FileEntry waveEntry = findWave(fileSet);
        // check wave file
        if (waveEntry == null) {
            return null;
        }
        ImportProfile config = ctx.getConfig();

        File f = waveEntry.getFile();
        String originalFilename = fileSet.getName();

        // creates FOXML and metadata
        LocalObject localObj = createObject(originalFilename, ctx);
        BatchItemObject batchLocalObject = ibm.addLocalObject(ctx.getBatch(), localObj);
        try {
            if (!InputUtils.isWave(f)) {
                throw new IllegalStateException("Not a WAVE content: " + f);
            }
            DigitalObjectHandler dobjHandler = DigitalObjectManager.getDefault().createHandler(localObj);
            createRelsExt(dobjHandler, f, ctx);
            createMetadata(dobjHandler, ctx);
            // create Audio Image Icon
          //  createImages(ctx.getTargetFolder(), new File("src/main/resources/cz/cas/lib/proarc/common/import/audio/music_icon.tiff"), originalFilename, localObj, config);
            createAudio(fileSet, ctx.getTargetFolder(), f, originalFilename, localObj, config);
            importArchivalCopy(fileSet, f, localObj, ctx);
            importUserCopy(fileSet, f, localObj, ctx);
            createTechnicalMetadata(localObj, ctx);
            // writes FOXML
            dobjHandler.commit();
            ibm.addChildRelation(ctx.getBatch(), null, localObj.getPid());
            batchLocalObject.setState(ObjectState.LOADED);
        } catch (Throwable ex) {
            LOG.log(Level.SEVERE, f.toString(), ex);
            batchLocalObject.setState(ObjectState.LOADING_FAILED);
            batchLocalObject.setLog(ImportBatchManager.toString(ex));
        }
        ibm.update(batchLocalObject);

        return batchLocalObject;
    }

    private void createAudio(FileSet fileSet, File tempBatchFolder, File original,
            String originalFilename, LocalObject foxml, ImportProfile config)
            throws DigitalObjectException, IOException {
        BinaryEditor.dissemination(foxml, BinaryEditor.RAW_AUDIO_ID, BinaryEditor.AUDIO_WAVE)
                .write(original, 0, null);
        long start;
        long end = 0;


        start = System.nanoTime();
        FileEntry entry = findSibling(fileSet, config.getNdkUserAudioFileSuffix());
        AudioMimeType audioType = AudioMimeType.MP3;
        MediaType mediaType = MediaType.valueOf(audioType.getMimeType());
        long endEntry = System.nanoTime() - start;
        if (entry == null) {
            throw new FileNotFoundException("Missing audio user copy: " + new File(tempBatchFolder.getParent(),
                    originalFilename + config.getNdkUserAudioFileSuffix()).toString());
        } else {
            File f = entry.getFile();
            end = System.nanoTime() - start;
            if (!InputUtils.isMp3(f)) {
                throw new IllegalStateException("Invalid user audio copy in fileset: " + fileSet.getName());
            } else {
                BinaryEditor.dissemination(foxml, BinaryEditor.FULL_ID, mediaType).write(f, 0, null);
            }
        }
        LOG.fine(String.format("file: %s, endEntry: %s, end: %s",
                originalFilename, endEntry / 1000000, end / 1000000));
    }

    private LocalObject createObject(String originalFilename, ImportProcess.ImportOptions ctx) {
        File tempBatchFolder = ctx.getTargetFolder();
        LocalStorage storage = new LocalStorage();
        File foxml = new File(tempBatchFolder, originalFilename + ".foxml");
        LocalObject localObj = storage.create(foxml);
        localObj.setOwner(ctx.getUsername());
        return localObj;
    }

    private void createMetadata(DigitalObjectHandler objHandler, ImportProcess.ImportOptions ctx) throws DigitalObjectException {
        MetadataHandler<Object> mHandler = objHandler.metadata();
        if (mHandler instanceof PageViewHandler) {
            // requires RELS-EXT model in place
            // creates MODS + DC + LABEL
            PageViewHandler pvHandler = (PageViewHandler) mHandler;
            String pageIndex = ctx.isGenerateIndices() ? String.valueOf(ctx.getConsumedFileCounter() + 1) : null;
            PageViewItem page = new PageViewItem();
            page.setPageIndex(pageIndex);
            pvHandler.setPage(page, null);
        } else {
            throw new IllegalStateException("Unsupported metadata handler: " + mHandler);
        }
    }

    private void createRelsExt(DigitalObjectHandler objHandler, File f, ImportOptions ctx) throws DigitalObjectException {
        String fedoraModel = ctx.getAudioModel();
        RelationEditor relEditor = objHandler.relations();
        relEditor.setModel(fedoraModel);
        relEditor.setDevice(ctx.getDevice());
        if (ctx.isPagePath()) {
            relEditor.setImportFile(f.getAbsolutePath());
        } else {
            relEditor.setImportFile(f.getName());
        }
        relEditor.setOrganization(ctx.getOrganization());
        relEditor.setUser("all");
        relEditor.setStatus(STATUS_NEW);
        relEditor.write(0, null);
        // XXX use fedora-model:downloadFilename in RELS-INT or label of datastream to specify filename
    }

    private boolean isWave(FileSet fileSet) {
        return findWave(fileSet) != null;
    }

    private FileEntry findWave(FileSet fileSet) {
        for (FileEntry entry : fileSet.getFiles()) {
            String mimetype = entry.getMimetype();
            if (AudioMimeType.WAVE.getMimeType().equals(mimetype)) {
                return entry;
            }
        }
        return null;
    }

    private FileEntry findSibling(FileSet fileSet, String filenameSuffix) {
        for (FileEntry entry : fileSet.getFiles()) {
            String filename = entry.getFile().getName().toLowerCase();
            if (filename.endsWith(filenameSuffix)) {
                return entry;
            }
        }
        return null;
    }

    private void importArchivalCopy(FileSet fileSet, File wave, FedoraObject fo, ImportOptions options) throws DigitalObjectException, IOException {
        ImportProfile config = options.getConfig();
        FileEntry entry = findSibling(fileSet, config.getNdkArchivalAudioFileSuffix());
        String dsId = BinaryEditor.NDK_AUDIO_ARCHIVAL_ID;
        if (entry == null) {
            throw new FileNotFoundException("Missing audio archival copy: " + new File(
                    wave.getParentFile(), fileSet.getName() + config.getNdkArchivalAudioFileSuffix()).toString());
        }
        if (entry != null) {
            File entryFile = entry.getFile();
            if (!InputUtils.isWave(entryFile)) {
                throw new IllegalStateException("Not a WAVE content: " + entryFile);
            }
            BinaryEditor binaryEditor = BinaryEditor.dissemination(fo, dsId, BinaryEditor.AUDIO_WAVE);
            binaryEditor.write(entryFile, 0, null);
        } else if (config.getRequiredDatastreamId().contains(dsId)) {
            throw new FileNotFoundException("Missing archival WAVE: " + new File(
                    wave.getParentFile(), fileSet.getName() + config.getNdkArchivalAudioFileSuffix()).toString());
        }
    }

    private void importUserCopy(FileSet fileSet, File wave, FedoraObject fo, ImportOptions options) throws DigitalObjectException, IOException {
        ImportProfile config = options.getConfig();
        FileSet.FileEntry entry = findSibling(fileSet, config.getNdkUserAudioFileSuffix());
        String dsId = BinaryEditor.NDK_AUDIO_USER_ID;
        if (entry == null) {
            throw new FileNotFoundException("Missing audio user copy: " + new File(
                    wave.getParentFile(), fileSet.getName() + config.getNdkUserAudioFileSuffix()).toString());
        }
        if (entry != null) {
            File entryFile = entry.getFile();
            // do not use entry.getMimeType. JDK 1.6 does not recognize JPEG2000
            if (!InputUtils.isMp3(entryFile)) {
                throw new IllegalStateException("Not a MP3 content: " + entryFile);
            }
            BinaryEditor binaryEditor = BinaryEditor.dissemination(fo, dsId, BinaryEditor.AUDIO_MP3);
            binaryEditor.write(entryFile, 0, null);
        } else if (config.getRequiredDatastreamId().contains(dsId)) {
            throw new FileNotFoundException("Missing user MP3: " + new File(
                    wave.getParentFile(), fileSet.getName() + config.getNdkUserAudioFileSuffix()).toString());
        }
    }

    private void createImages(File tempBatchFolder, File original,
                              String originalFilename, LocalStorage.LocalObject foxml, ImportProfile config)
            throws IOException, DigitalObjectException, AppConfigurationException {

        long start = System.nanoTime();
        BufferedImage tiff = ImageSupport.readImage(original.toURI().toURL(), ImageMimeType.TIFF);
        long endRead = System.nanoTime() - start;
        ImageMimeType imageType = ImageMimeType.JPEG;
        MediaType mediaType = MediaType.valueOf(imageType.getMimeType());

        start = System.nanoTime();
        String targetName = String.format("%s.full.%s", originalFilename, imageType.getDefaultFileExtension());
        File f = writeImage(tiff, tempBatchFolder, targetName, imageType);
        if (!InputUtils.isJpeg(f)) {
            throw new IllegalStateException("Not a JPEG content: " + f);
        }
        long endFull = System.nanoTime() - start;

        start = System.nanoTime();
        Integer previewMaxHeight = config.getPreviewMaxHeight();
        Integer previewMaxWidth = config.getPreviewMaxWidth();
        config.checkPreviewScaleParams();
        targetName = String.format("%s.preview.%s", originalFilename, imageType.getDefaultFileExtension());
        f = writeImage(
                scale(tiff, config.getPreviewScaling(), previewMaxWidth, previewMaxHeight),
                tempBatchFolder, targetName, imageType);
        if (!InputUtils.isJpeg(f)) {
            throw new IllegalStateException("Not a JPEG content: " + f);
        }
        long endPreview = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, BinaryEditor.PREVIEW_ID, mediaType).write(f, 0, null);

        start = System.nanoTime();
        f = createThumbnail(tempBatchFolder, originalFilename, original, tiff, config);
        long endThumb = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, BinaryEditor.THUMB_ID, mediaType).write(f, 0, null);

        LOG.fine(String.format("file: %s, read: %s, full: %s, preview: %s, thumb: %s",
                originalFilename, endRead / 1000000, endFull / 1000000, endPreview / 1000000, endThumb / 1000000));
    }

    private File createThumbnail(File tempBatchFolder, String originalFilename, File original, BufferedImage tiff, ImportProfile config)
            throws AppConfigurationException, IOException {
        ImageMimeType imageType = ImageMimeType.JPEG;
        String targetName = String.format("%s.thumb.%s", originalFilename, imageType.getDefaultFileExtension());
        return createJavaThumbnail(tempBatchFolder, targetName, imageType, tiff, config);

    }

    private File createJavaThumbnail(File tempBatchFolder, String targetName, ImageMimeType imageType, BufferedImage tiff, ImportProfile config)
            throws AppConfigurationException, IOException {
        Integer thumbMaxHeight = config.getThumbnailMaxHeight();
        Integer thumbMaxWidth = config.getThumbnailMaxWidth();
        config.checkThumbnailScaleParams();
        File f = writeImage(
                scale(tiff, config.getThumbnailScaling(), thumbMaxWidth, thumbMaxHeight),
                tempBatchFolder, targetName, imageType);
        if (!InputUtils.isJpeg(f)) {
            throw new IllegalStateException("Not a JPEG content: " + f);
        }
        return f;
    }

    private static File writeImage(BufferedImage image, File folder, String filename, ImageMimeType imageType) throws IOException {
        File imgFile = new File(folder, filename);
        FileImageOutputStream fos = new FileImageOutputStream(imgFile);
        try {
            ImageSupport.writeImageToStream(image, imageType.getDefaultFileExtension(), fos, 1.0f);
            return imgFile;
        } finally {
            fos.close();
        }
    }

    private static BufferedImage scale(BufferedImage tiff, ImageSupport.ScalingMethod method,
                                       Integer maxWidth, Integer maxHeight) {

        long start = System.nanoTime();
        int height = tiff.getHeight();
        int width = tiff.getWidth();
        int targetWidth = width;
        int targetHeight = height;
        double scale = Double.MAX_VALUE;
        if (maxHeight != null && height > maxHeight) {
            scale = (double) maxHeight / height;
        }
        if (maxWidth != null && width > maxWidth) {
            double scalew = (double) maxWidth / width;
            scale = Math.min(scale, scalew);
        }
        if (scale != Double.MAX_VALUE) {
            targetHeight = (int) (height * scale);
            targetWidth = (int) (width * scale);
        }
        BufferedImage scaled = ImageSupport.scale(tiff, targetWidth, targetHeight, method, true);
        LOG.fine(String.format("scaled [%s, %s] to [%s, %s], boundary [%s, %s] [w, h], time: %s ms",
                width, height, targetWidth, targetHeight, maxWidth, maxHeight, (System.nanoTime() - start) / 1000000));
        return scaled;
    }

    private void createTechnicalMetadata(LocalStorage.LocalObject localObj, ImportProcess.ImportOptions ctx)
            throws DigitalObjectException {

        JhoveContext jhoveCtx = ctx.getJhoveContext();
        File file = BinaryEditor.dissemination(localObj, BinaryEditor.RAW_AUDIO_ID, BinaryEditor.AUDIO_WAVE).read();
        AesEditor aes57Editor = AesEditor.raw(localObj);
        aes57Editor.write(file, jhoveCtx, aes57Editor.getLastModified(), null);

        // NDK version
        file = BinaryEditor.dissemination(localObj, BinaryEditor.NDK_AUDIO_ARCHIVAL_ID, BinaryEditor.AUDIO_WAVE).read();
        if (file != null) {
            aes57Editor = AesEditor.ndkArchival(localObj);
            aes57Editor.write(file, jhoveCtx, aes57Editor.getLastModified(), null);
        }
    }
}
