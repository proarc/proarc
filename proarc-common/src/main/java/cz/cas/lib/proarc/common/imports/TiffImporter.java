/*
 * Copyright (C) 2011 Jan Pokorsky
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
package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.MixEditor;
import cz.cas.lib.proarc.common.fedora.PageView.PageViewHandler;
import cz.cas.lib.proarc.common.fedora.PageView.PageViewItem;
import cz.cas.lib.proarc.common.fedora.StringEditor;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.FileSet.FileEntry;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.process.ExternalProcess;
import cz.cas.lib.proarc.common.process.KakaduCompress;
import cz.cas.lib.proarc.common.process.TiffToJpgConvert;
import cz.incad.imgsupport.ImageMimeType;
import cz.incad.imgsupport.ImageSupport;
import cz.incad.imgsupport.ImageSupport.ScalingMethod;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.stream.FileImageOutputStream;
import javax.ws.rs.core.MediaType;
import org.apache.commons.configuration.Configuration;

/**
 * Requires Java Advanced Imaging support.
 * See http://www.oracle.com/technetwork/java/current-142188.html and
 * http://download.java.net/media/jai/builds/release/1_1_3/
 * jai-1_1_3-lib.zip is a platform independent version
 *
 * http://download.java.net/media/jai-imageio/builds/release/1.1/ fo jai_imageio-1.1.jar
 *
 * For maven, try to depend just on com.sun.media.jai_imageio.1.1 as kramerius common.
 * How to properly depend in pom see http://sahits.ch/blog/?p=1038
 *
 * @author Jan Pokorsky
 */
public class TiffImporter implements ImageImporter {

    private static final Logger LOG = Logger.getLogger(TiffImporter.class.getName());
    private final ImportBatchManager ibm;

    public TiffImporter(ImportBatchManager ibm) {
        this.ibm = ibm;
    }

    public boolean accept(FileSet fileSet) {
        return isTiff(fileSet);
    }

    public BatchItemObject consume(FileSet fileSet, ImportOptions ctx) {
        FileEntry tiffEntry = findTiff(fileSet);
        // check tiff file
        if (tiffEntry == null) {
            return null;
        }
        ImportProfile config = ctx.getConfig();

        File f = tiffEntry.getFile();
        String originalFilename = fileSet.getName();

        // creates FOXML and metadata
        LocalObject localObj = createObject(originalFilename, ctx);
        BatchItemObject batchLocalObject = ibm.addLocalObject(ctx.getBatch(), localObj);
        try {
            if (!InputUtils.isTiff(f)) {
                throw new IllegalStateException("Not a TIFF content: " + f);
            }
            DigitalObjectHandler dobjHandler = DigitalObjectManager.getDefault().createHandler(localObj);
            createRelsExt(dobjHandler, f, ctx);
            createMetadata(dobjHandler, ctx);
            createImages(ctx.getTargetFolder(), f, originalFilename, localObj, config);
            importArchivalCopy(fileSet, f, localObj, ctx);
            importUserCopy(fileSet, f, localObj, ctx);
            importOcr(fileSet, localObj, ctx);
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

    private LocalObject createObject(String originalFilename, ImportOptions ctx) {
        File tempBatchFolder = ctx.getTargetFolder();
        LocalStorage storage = new LocalStorage();
        File foxml = new File(tempBatchFolder, originalFilename + ".foxml");
        LocalObject localObj = storage.create(foxml);
        localObj.setOwner(ctx.getUsername());
        return localObj;
    }

    private void createMetadata(DigitalObjectHandler objHandler, ImportOptions ctx) throws DigitalObjectException {
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
        String fedoraModel = ctx.getModel();
        RelationEditor relEditor = objHandler.relations();
        relEditor.setModel(fedoraModel);
        relEditor.setDevice(ctx.getDevice());
        relEditor.setImportFile(f.getName());
        relEditor.write(0, null);
        // XXX use fedora-model:downloadFilename in RELS-INT or label of datastream to specify filename
    }

    private boolean isTiff(FileSet fileSet) {
        return findTiff(fileSet) != null;
    }

    private FileEntry findTiff(FileSet fileSet) {
        for (FileEntry entry : fileSet.getFiles()) {
            String mimetype = entry.getMimetype();
            if (ImageMimeType.TIFF.getMimeType().equals(mimetype)) {
                return entry;
            }
        }
        return null;
    }

    private void importOcr(FileSet fileSet, FedoraObject fo, ImportOptions options)
            throws IOException, DigitalObjectException {

        // XXX find filename.ocr.txt or generate OCR or nothing
        // plain text OCR
        File tempBatchFolder = options.getTargetFolder();
        String originalFilename = fileSet.getName();
        ImportProfile config = options.getConfig();
        List<Object> requiredDatastreamId = config.getRequiredDatastreamId();

        FileEntry ocrEntry = findSibling(fileSet, config.getPlainOcrFileSuffix());
        if (ocrEntry != null) {
            File ocrFile = new File(tempBatchFolder, originalFilename + '.' + StringEditor.OCR_ID + ".txt");
            StringEditor.copy(ocrEntry.getFile(), config.getPlainOcrCharset(), ocrFile, "UTF-8");
            XmlStreamEditor ocrEditor = fo.getEditor(StringEditor.ocrProfile());
            ocrEditor.write(ocrFile.toURI(), 0, null);
        } else if (requiredDatastreamId.contains(StringEditor.OCR_ID)) {
            throw new FileNotFoundException("Missing OCR: " + new File(tempBatchFolder.getParent(),
                    originalFilename + config.getPlainOcrFileSuffix()).toString());
        }
        // ALTO OCR
        FileEntry altoEntry = findSibling(fileSet, config.getAltoFileSuffix());
        if (altoEntry != null) {
            URI altoUri = altoEntry.getFile().toURI();
            AltoDatastream.importAlto(fo, altoUri, null);
        } else if (requiredDatastreamId.contains(AltoDatastream.ALTO_ID)) {
            throw new FileNotFoundException("Missing ALTO: " + new File(tempBatchFolder.getParent(),
                    originalFilename + config.getAltoFileSuffix()).toString());
        }
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

    private void importArchivalCopy(FileSet fileSet, File tiff, FedoraObject fo, ImportOptions options) throws DigitalObjectException, IOException {
        ImportProfile config = options.getConfig();
        FileEntry entry = findSibling(fileSet, config.getNdkArchivalFileSuffix());
        String dsId = BinaryEditor.NDK_ARCHIVAL_ID;
        if (entry == null) {
            entry = processJp2Copy(fileSet, tiff, options.getTargetFolder(), dsId, config.getNdkArchivalProcessor());
        }
        if (entry != null) {
            File entryFile = entry.getFile();
            // do not use entry.getMimeType. JDK 1.6 does not recognize JPEG2000
            if (!InputUtils.isJp2000(entryFile)) {
                throw new IllegalStateException("Not a JP2000 content: " + entryFile);
            }
            BinaryEditor binaryEditor = BinaryEditor.dissemination(fo, dsId, BinaryEditor.IMAGE_JP2);
            binaryEditor.write(entryFile, 0, null);
        } else if (config.getRequiredDatastreamId().contains(dsId)) {
            throw new FileNotFoundException("Missing archival JP2: " + new File(
                    tiff.getParentFile(), fileSet.getName() + config.getNdkArchivalFileSuffix()));
        }
    }

    private void importUserCopy(FileSet fileSet, File tiff, FedoraObject fo, ImportOptions options) throws DigitalObjectException, IOException {
        ImportProfile config = options.getConfig();
        FileEntry entry = findSibling(fileSet, config.getNdkUserFileSuffix());
        String dsId = BinaryEditor.NDK_USER_ID;
        if (entry == null) {
            entry = processJp2Copy(fileSet, tiff, options.getTargetFolder(), dsId, config.getNdkUserProcessor());
        }
        if (entry != null) {
            File entryFile = entry.getFile();
            // do not use entry.getMimeType. JDK 1.6 does not recognize JPEG2000
            if (!InputUtils.isJp2000(entryFile)) {
                throw new IllegalStateException("Not a JP2000 content: " + entryFile);
            }
            BinaryEditor binaryEditor = BinaryEditor.dissemination(fo, dsId, BinaryEditor.IMAGE_JP2);
            binaryEditor.write(entryFile, 0, null);
        } else if (config.getRequiredDatastreamId().contains(dsId)) {
            throw new FileNotFoundException("Missing user JP2: " + new File(
                    tiff.getParentFile(), fileSet.getName() + config.getNdkUserFileSuffix()));
        }
    }

    private FileEntry processJp2Copy(FileSet fileSet, File tiff, File tempBatchFolder, String dsId, Configuration processorConfig) throws IOException {
        if (processorConfig != null && !processorConfig.isEmpty()) {
            File acFile = new File(tempBatchFolder, fileSet.getName() + '.' + dsId + ".jp2");
            String processorType = processorConfig.getString("type");
            ExternalProcess process = null;
            if (KakaduCompress.ID.equals(processorType)) {
                process = new KakaduCompress(processorConfig, tiff, acFile);
            }
            if (process != null) {
                process.run();
                if (!process.isOk()) {
                    throw new IOException(acFile.toString() + "\n" + process.getFullOutput());
                }
            }
            return  new FileEntry(acFile);
        }
        return null;
    }

    private void createImages(File tempBatchFolder, File original,
            String originalFilename, LocalObject foxml, ImportProfile config)
            throws IOException, DigitalObjectException, AppConfigurationException {
        
        BinaryEditor.dissemination(foxml, BinaryEditor.RAW_ID, BinaryEditor.IMAGE_TIFF)
                .write(original, 0, null);

        boolean runCustomConversion = config.isTiffToJpgDefined();

        long start;
        long endRead = 0;
        BufferedImage tiff = null;
        File f;

        if (!runCustomConversion) {
            start = System.nanoTime();
            tiff = ImageSupport.readImage(original.toURI().toURL(), ImageMimeType.TIFF);
            endRead = System.nanoTime() - start;
        }

        ImageMimeType imageType = ImageMimeType.JPEG;
        MediaType mediaType = MediaType.valueOf(imageType.getMimeType());

        start = System.nanoTime();
        String targetName = String.format("%s.full.%s", originalFilename, imageType.getDefaultFileExtension());

        if (runCustomConversion) {
            f  = new File(tempBatchFolder, targetName);
            ExternalProcess p = new TiffToJpgConvert(config.getConvertorTiffToJpgProcessor(), original, f);
            p.run();

            if (!p.isOk()) {
                throw new IllegalStateException("Converting tiff to FULL jpg failed: " + p.getFullOutput());
            }
        } else {
            f = writeImage(tiff, tempBatchFolder, targetName, imageType);
        }

        if (!InputUtils.isJpeg(f)) {
            throw new IllegalStateException("Not a JPEG content: " + f);
        }
        long endFull = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, BinaryEditor.FULL_ID, mediaType).write(f, 0, null);

        start = System.nanoTime();
        Integer previewMaxHeight = config.getPreviewMaxHeight();
        Integer previewMaxWidth = config.getPreviewMaxWidth();
        config.checkPreviewScaleParams();
        targetName = String.format("%s.preview.%s", originalFilename, imageType.getDefaultFileExtension());

        if (runCustomConversion) {
            f  = new File(tempBatchFolder, targetName);
            ExternalProcess p = new TiffToJpgConvert(config.getConvertorTiffToJpgProcessor(), original, f, previewMaxWidth, previewMaxHeight);
            p.run();

            if (!p.isOk()) {
                throw new IllegalStateException("Converting tiff to PREVIEW jpg failed: " + p.getFullOutput());
            }
        } else {
            f = writeImage(
                    scale(tiff, config.getPreviewScaling(), previewMaxWidth, previewMaxHeight),
                    tempBatchFolder, targetName, imageType);
        }
        if (!InputUtils.isJpeg(f)) {
            throw new IllegalStateException("Not a JPEG content: " + f);
        }

        long endPreview = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, BinaryEditor.PREVIEW_ID, mediaType).write(f, 0, null);

        start = System.nanoTime();
        if (runCustomConversion) {
            //check is done within createThumbnail() unlike full and preview variants, should be unified
            targetName = String.format("%s.thumb.%s", originalFilename, imageType.getDefaultFileExtension());
            Integer thumbMaxHeight = config.getThumbnailMaxHeight();
            Integer thumbMaxWidth = config.getThumbnailMaxWidth();
            config.checkThumbnailScaleParams();

            f  = new File(tempBatchFolder, targetName);
            ExternalProcess p = new TiffToJpgConvert(config.getConvertorTiffToJpgProcessor(), original, f, thumbMaxWidth, thumbMaxHeight);
            p.run();

            if (!p.isOk()) {
                throw new IllegalStateException("Converting tiff to THUMBNAIL jpg failed: " + p.getFullOutput());
            }
        } else {
            f = createThumbnail(tempBatchFolder, originalFilename, original, tiff, config);
        }

        long endThumb = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, BinaryEditor.THUMB_ID, mediaType).write(f, 0, null);

        LOG.fine(String.format("file: %s, read: %s, full: %s, preview: %s, thumb: %s",
                originalFilename, endRead / 1000000, endFull / 1000000, endPreview / 1000000, endThumb / 1000000));
    }

    private File createThumbnail(File tempBatchFolder, String originalFilename, File original, BufferedImage tiff, ImportProfile config)
            throws AppConfigurationException, IOException {
        ImageMimeType imageType = ImageMimeType.JPEG;
        String targetName = String.format("%s.thumb.%s", originalFilename, imageType.getDefaultFileExtension());
        // XXX requieres import profiles
//        Configuration processCfg = config.getThumbnailProcessor();
//        if (processCfg.isEmpty()) {
            return createJavaThumbnail(tempBatchFolder, targetName, imageType, tiff, config);
//        } else {
//            GenericExternalProcess process = new GenericExternalProcess(processCfg);
//            process.addInputFile(original);
//            process.addOutputFile(new File(tempBatchFolder, targetName));
//            process.run();
//            if (!process.isOk()) {
//                throw new IOException(process.getOutputFile().toString() + "\n" + process.getFullOutput());
//            }
//            return process.getOutputFile();
//        }
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

    private static BufferedImage scale(BufferedImage tiff, ScalingMethod method,
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

    private void createTechnicalMetadata(LocalObject localObj, ImportOptions ctx)
            throws DigitalObjectException {

        JhoveContext jhoveCtx = ctx.getJhoveContext();
        File file = BinaryEditor.dissemination(localObj, BinaryEditor.RAW_ID, BinaryEditor.IMAGE_TIFF).read();
        MixEditor mixEditor = MixEditor.raw(localObj);
        mixEditor.write(file, jhoveCtx, mixEditor.getLastModified(), null);

        // NDK version
        file = BinaryEditor.dissemination(localObj, BinaryEditor.NDK_ARCHIVAL_ID, BinaryEditor.IMAGE_JP2).read();
        if (file != null) {
            mixEditor = MixEditor.ndkArchival(localObj);
            mixEditor.write(file, jhoveCtx, mixEditor.getLastModified(), null);
        }
    }
}
