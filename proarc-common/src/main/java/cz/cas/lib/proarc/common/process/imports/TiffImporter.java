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
package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.PageView.PageViewHandler;
import cz.cas.lib.proarc.common.storage.PageView.PageViewItem;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.process.imports.FileSet.FileEntry;
import cz.cas.lib.proarc.common.process.BatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.process.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.process.external.ExternalProcess;
import cz.cas.lib.proarc.common.process.external.KakaduCompress;
import cz.cas.lib.proarc.common.process.external.OcrGenerator;
import cz.cas.lib.proarc.common.process.external.TiffToJpgConvert;
import cz.incad.imgsupport.ImageMimeType;
import cz.incad.imgsupport.ImageSupport;
import cz.incad.imgsupport.ImageSupport.ScalingMethod;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.stream.FileImageOutputStream;
import javax.ws.rs.core.MediaType;
import org.apache.commons.configuration.Configuration;

import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_NEW;

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
    private final BatchManager ibm;

    public TiffImporter(BatchManager ibm) {
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
            createImages(ctx.getTargetFolder(), f, originalFilename, localObj, config, fileSet);
            importArchivalCopy(fileSet, f, localObj, ctx);
            importUserCopy(fileSet, f, localObj, ctx);
            importOcr(fileSet, f, localObj, ctx);
            createTechnicalMetadata(localObj, ctx);
            // writes FOXML
            dobjHandler.commit();
            ibm.addChildRelation(ctx.getBatch(), null, localObj.getPid());
            batchLocalObject.setState(ObjectState.LOADED);
        } catch(IllegalStateException ex) {
            LOG.log(Level.SEVERE, f.toString(), ex);
            batchLocalObject.setState(ObjectState.STOPPED);
            batchLocalObject.setLog(BatchManager.toString(ex));
        } catch (Throwable ex) {
            LOG.log(Level.SEVERE, f.toString(), ex);
            batchLocalObject.setState(ObjectState.LOADING_FAILED);
            batchLocalObject.setLog(BatchManager.toString(ex));
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
            if (ctx.isGeneratePageNumber()) {
                if (pageIndex == null && pageIndex.isEmpty()) {
                    pageIndex = String.valueOf(ctx.getConsumedFileCounter() + 1);
                }
                page.setPageNumber(pageIndex); // number is same as pageIndex
            }
            page.setPageType("normalPage");
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
        relEditor.setOrganization(ctx.getOrganization());
        relEditor.setUser(ctx.getConfig().getDefaultProcessor());
        relEditor.setStatus(STATUS_NEW);
        if (ctx.isPagePath()) {
            relEditor.setImportFile(f.getAbsolutePath());
        } else {
            relEditor.setImportFile(f.getName());
        }
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

    private void importOcr(FileSet fileSet, File tiff, ProArcObject fo, ImportOptions options)
            throws IOException, DigitalObjectException {

        // XXX find filename.ocr.txt or generate OCR or nothing
        // plain text OCR
        File tempBatchFolder = options.getTargetFolder();
        String originalFilename = fileSet.getName();
        ImportProfile config = options.getConfig();
        List<Object> requiredDatastreamId = config.getRequiredDatastreamId();
        List<Object> skippedDatastreamId = config.getSkippedDatastreamId();

        FileEntry ocrEntry = findSibling(fileSet, config.getPlainOcrFileSuffix());
        FileEntry altoEntry = findSibling(fileSet, config.getAltoFileSuffix());
        if ((ocrEntry == null || altoEntry == null) && requiredDatastreamId.contains(StringEditor.OCR_ALTO_GEN_ID)) {
            generateOCR(tiff, options);

            File[] ocrFiles = OcrGenerator.getOcrFiles(tiff, config.getPlainOcrFileSuffix(), config.getAltoFileSuffix());

            ocrEntry = new FileEntry(ocrFiles[0]);
            altoEntry = new FileEntry(ocrFiles[1]);
        }
        if (skippedDatastreamId.contains(StringEditor.OCR_ID))  {
            LOG.info("Skip import " + StringEditor.OCR_ID + " for uuid " + fo.getPid());
        } else if (ocrEntry != null) {
            importOcr(tempBatchFolder, originalFilename, ocrEntry.getFile(), config, fo);
        } else if (existsFile(options.getImportFolder(), originalFilename, config.getPlainOcrFilePath(), config.getPlainOcrFileSuffix(), config.getOcrAltoFolderPath())) {
            File ocr = createFile(options.getImportFolder(), originalFilename, config.getPlainOcrFilePath(), config.getPlainOcrFileSuffix(), config.getOcrAltoFolderPath());
            if (ocr != null) {
                doOcrEditor(tempBatchFolder, originalFilename, ocr, config, fo);
            }
        } else if (config.getDefaultAltoAndOcr()) {
            File ocr = new File(ibm.getAppConfig().getConfigHome().toURI().resolve(config.getDefaultOcr()));
            if (ocr != null) {
                doOcrEditor(tempBatchFolder, originalFilename, ocr, config, fo);
            }
        } else if (requiredDatastreamId.contains(StringEditor.OCR_ID)) {
            throw new FileNotFoundException("Missing OCR: " + new File(tempBatchFolder.getParent(),
                    originalFilename + config.getPlainOcrFileSuffix()).toString());
        }
        // ALTO OCR
        if (skippedDatastreamId.contains(AltoDatastream.ALTO_ID))  {
            LOG.info("Skip import " + AltoDatastream.ALTO_ID + " for uuid " + fo.getPid());
        } else if (altoEntry != null) {
            URI altoUri = altoEntry.getFile().toURI();
            AltoDatastream altoDatastrem = new AltoDatastream(config);
            altoDatastrem.importAlto(fo, altoUri, null);
        } else if (existsFile(options.getImportFolder(), originalFilename, config.getAltoFilePath(), config.getAltoFileSuffix(), config.getOcrAltoFolderPath())) {
            File alto = createFile(options.getImportFolder(), originalFilename, config.getAltoFilePath(), config.getAltoFileSuffix(), config.getOcrAltoFolderPath());
            URI altoUri = alto.toURI();
            AltoDatastream altoDatastrem = new AltoDatastream(config);
            altoDatastrem.importAlto(fo, altoUri, null);
        } else if (config.getDefaultAltoAndOcr()) {
            File alto = new File(ibm.getAppConfig().getConfigHome().toURI().resolve(config.getDefaultAlto()));
            URI altoUri = alto.toURI();
            AltoDatastream altoDatastrem = new AltoDatastream(config);
            altoDatastrem.importAlto(fo, altoUri, null);
        } else if (requiredDatastreamId.contains(AltoDatastream.ALTO_ID)) {
            throw new FileNotFoundException("Missing ALTO: " + new File(tempBatchFolder.getParent(),
                    originalFilename + config.getAltoFileSuffix()).toString());
        }
    }

    private void generateOCR(File tiff, ImportOptions options) throws IOException{
        ImportProfile config = options.getConfig();

        ExternalProcess process = new OcrGenerator(config.getOcrGenProcessor(), tiff, config.getPlainOcrFileSuffix(), config.getAltoFileSuffix());

        if (process != null) {
            process.run();

            if (!process.isOk()) {
                throw new IOException("Generating OCR for " + tiff.getName() + " failed. \n " + process.getFullOutput());
            }
        }
    }

    private boolean existsFile(File originalPath, String filename, String path, String suffix, int lastFolder) {
        File file = createFile(originalPath, filename, path, suffix, lastFolder);
        if (file == null) {
            return false;
        } else {
            return file.exists();
        }
    }

    private File createFile(File originalPath, String filename, String path, String suffix, int lastFolder) {
        if (path == null || path.isEmpty() || path.equals("null")) {
            return null;
        }
        StringBuilder pathValue = new StringBuilder();
        pathValue.append(path).append("/");
        String value = getOriginalPath(originalPath, lastFolder);
        pathValue.append(value);
        pathValue.append(filename);
        pathValue.append(suffix);
        return new File(pathValue.toString());
    }

    private String getOriginalPath(File originalPath, int lastFolder) {
        StringBuilder value = new StringBuilder();
        List<String> folderName = new ArrayList<>();
        getFolderName(originalPath, folderName);
        if (folderName.size() > lastFolder - 1) {
            for (int i = lastFolder - 1; i >= 0; i--) {
                value.append(folderName.get(i)).append("/");
            }
            return value.toString();
        }
        return null;
    }

    private void getFolderName(File file, List<String> folderName) {
        if (file != null) {
            folderName.add(file.getName());
            getFolderName(file.getParentFile(), folderName);
        } else {
            return;
        }
    }


    public void doOcrEditor(File tempBatchFolder, String originalFilename, File ocrEntry, ImportProfile config, ProArcObject fo) throws IOException, DigitalObjectException {
        File ocrFile = new File(tempBatchFolder, originalFilename + '.' + StringEditor.OCR_ID + ".txt");
        StringEditor.copy(ocrEntry, config.getPlainOcrCharset(), ocrFile, "UTF-8");
        XmlStreamEditor ocrEditor = fo.getEditor(StringEditor.ocrProfile());
        ocrEditor.write(ocrFile.toURI(), 0, null);
    }

    public void importOcr(File tempBatchFolder, String originalFilename, File ocrEntry, ImportProfile config, ProArcObject fo) throws IOException, DigitalObjectException {
        XmlStreamEditor ocrEditor = fo.getEditor(StringEditor.ocrProfile());
        ocrEditor.write(ocrEntry.toURI(), 0, null);
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

    private void importArchivalCopy(FileSet fileSet, File tiff, ProArcObject fo, ImportOptions options) throws DigitalObjectException, IOException {
        ImportProfile config = options.getConfig();
        FileEntry entry = findSibling(fileSet, config.getNdkArchivalFileSuffix());
        String dsId = BinaryEditor.NDK_ARCHIVAL_ID;
        if (config.getSkippedDatastreamId().contains(dsId))  {
            LOG.info("Skip import " + dsId + " for uuid " + fo.getPid());
            return;
        }
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

    private void importUserCopy(FileSet fileSet, File tiff, ProArcObject fo, ImportOptions options) throws DigitalObjectException, IOException {
        ImportProfile config = options.getConfig();
        FileEntry entry = findSibling(fileSet, config.getNdkUserFileSuffix());
        String dsId = BinaryEditor.NDK_USER_ID;
        if (config.getSkippedDatastreamId().contains(dsId))  {
            LOG.info("Skip import " + dsId + " for uuid " + fo.getPid());
            return;
        }
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

    public FileEntry processJp2Copy(FileSet fileSet, File tiff, File tempBatchFolder, String dsId, Configuration processorConfig) throws IOException {
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
            String originalFilename, LocalObject foxml, ImportProfile config, FileSet fileSet)
            throws IOException, DigitalObjectException, AppConfigurationException {
        
        BinaryEditor.dissemination(foxml, BinaryEditor.RAW_ID, BinaryEditor.IMAGE_TIFF)
                .write(original, 0, null);

        boolean runCustomConversion = config.isTiffToJpgDefined();

        long start;
        long endRead = 0;
        BufferedImage tiff = null;
        File f = null;

        ImageMimeType imageType = ImageMimeType.JPEG;
        MediaType mediaType = MediaType.valueOf(imageType.getMimeType());

        start = System.nanoTime();
        String targetName = String.format("%s.full.%s", originalFilename, imageType.getDefaultFileExtension());

        FileEntry fullEntry = findSibling(fileSet, config.getNdkFullFileSuffix());
        String fullId = BinaryEditor.FULL_ID;

        if (config.getSkippedDatastreamId().contains(fullId))  {
            LOG.info("Skip import " + fullId + " for uuid " + foxml.getPid());
        } else if (fullEntry != null) {
            f = fullEntry.getFile();
        } else if (config.getRequiredDatastreamId().contains(fullId)) {
            throw new FileNotFoundException("Missing full jpg: " + new File(
                    original.getParentFile(), fileSet.getName() + config.getNdkFullFileSuffix()));
        } else {
            if (runCustomConversion) {
                f = new File(tempBatchFolder, targetName);
                ExternalProcess p = new TiffToJpgConvert(config.getConvertorTiffToJpgProcessor(), original, f);
                p.run();

                if (!p.isOk()) {
                    throw new IllegalStateException("Converting tiff to FULL jpg failed: " + p.getFullOutput());
                }
            } else {
                if (tiff == null) {
                    start = System.nanoTime();
                    tiff = ImageSupport.readImage(original.toURI().toURL(), ImageMimeType.TIFF);
                    endRead = System.nanoTime() - start;
                }
                f = writeImage(tiff, tempBatchFolder, targetName, imageType);
            }
        }

        if (!InputUtils.isJpeg(f)) {
            throw new IllegalStateException("Not a JPEG content: " + f);
        }
        long endFull = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, fullId, mediaType).write(f, 0, null);

        start = System.nanoTime();

        FileEntry entry = findSibling(fileSet, config.getNdkPreviewFileSuffix());
        String previewId = BinaryEditor.PREVIEW_ID;
        if (config.getSkippedDatastreamId().contains(previewId))  {
            LOG.info("Skip import " + previewId + " for uuid " + foxml.getPid());
        } else if (entry != null) {
            f = entry.getFile();
        } else if (config.getRequiredDatastreamId().contains(previewId)) {
            throw new FileNotFoundException("Missing preview: " + new File(
                    original.getParentFile(), fileSet.getName() + config.getNdkPreviewFileSuffix()));
        } else {
            Integer previewMaxHeight = config.getPreviewMaxHeight();
            Integer previewMaxWidth = config.getPreviewMaxWidth();
            config.checkPreviewScaleParams();
            targetName = String.format("%s.preview.%s", originalFilename, imageType.getDefaultFileExtension());
            if (runCustomConversion) {
                f = new File(tempBatchFolder, targetName);
                ExternalProcess p = new TiffToJpgConvert(config.getConvertorTiffToJpgProcessor(), original, f, previewMaxWidth, previewMaxHeight);
                p.run();

                if (!p.isOk()) {
                    throw new IllegalStateException("Converting tiff to PREVIEW jpg failed: " + p.getFullOutput());
                }
            } else {
                if (tiff == null) {
                    start = System.nanoTime();
                    tiff = ImageSupport.readImage(original.toURI().toURL(), ImageMimeType.TIFF);
                    endRead = System.nanoTime() - start;
                }
                f = writeImage(
                        scale(tiff, config.getPreviewScaling(), previewMaxWidth, previewMaxHeight),
                        tempBatchFolder, targetName, imageType);
            }
            if (!InputUtils.isJpeg(f)) {
                throw new IllegalStateException("Not a JPEG content: " + f);
            }
        }

        long endPreview = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, previewId, mediaType).write(f, 0, null);

        start = System.nanoTime();

        FileEntry thumbnailEntry = findSibling(fileSet, config.getNdkThumbnailFileSuffix());
        String thumbnailId = BinaryEditor.THUMB_ID;
        if (config.getSkippedDatastreamId().contains(thumbnailId))  {
            LOG.info("Skip import " + thumbnailId + " for uuid " + foxml.getPid());
        } else if (thumbnailEntry != null) {
            f = thumbnailEntry.getFile();
        } else if (config.getRequiredDatastreamId().contains(thumbnailId)) {
            throw new FileNotFoundException("Missing thumbnail: " + new File(
                    original.getParentFile(), fileSet.getName() + config.getNdkThumbnailFileSuffix()));
        } else {
            if (runCustomConversion) {
                //check is done within createThumbnail() unlike full and preview variants, should be unified
                targetName = String.format("%s.thumb.%s", originalFilename, imageType.getDefaultFileExtension());
                Integer thumbMaxHeight = config.getThumbnailMaxHeight();
                Integer thumbMaxWidth = config.getThumbnailMaxWidth();
                config.checkThumbnailScaleParams();

                f = new File(tempBatchFolder, targetName);
                ExternalProcess p = new TiffToJpgConvert(config.getConvertorTiffToJpgProcessor(), original, f, thumbMaxWidth, thumbMaxHeight);
                p.run();

                if (!p.isOk()) {
                    throw new IllegalStateException("Converting tiff to THUMBNAIL jpg failed: " + p.getFullOutput());
                }
            } else {
                if (tiff == null) {
                    start = System.nanoTime();
                    tiff = ImageSupport.readImage(original.toURI().toURL(), ImageMimeType.TIFF);
                    endRead = System.nanoTime() - start;
                }
                f = createThumbnail(tempBatchFolder, originalFilename, original, tiff, config);
            }
        }

        long endThumb = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, thumbnailId, mediaType).write(f, 0, null);

        LOG.info(String.format("file: %s, read: %s, full: %s, preview: %s, thumb: %s",
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

    public static File writeImage(BufferedImage image, File folder, String filename, ImageMimeType imageType) throws IOException {
        File imgFile = new File(folder, filename);
        FileImageOutputStream fos = new FileImageOutputStream(imgFile);
        try {
            ImageSupport.writeImageToStream(image, imageType.getDefaultFileExtension(), fos, 1.0f);
            return imgFile;
        } finally {
            fos.close();
        }
    }

    public static BufferedImage scale(BufferedImage tiff, ScalingMethod method,
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
