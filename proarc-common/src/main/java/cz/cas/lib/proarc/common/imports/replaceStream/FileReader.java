/*
 * Copyright (C) 2023 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.imports.replaceStream;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.MixEditor;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.StringEditor;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportProcess;
import cz.cas.lib.proarc.common.imports.InputUtils;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.process.ExternalProcess;
import cz.cas.lib.proarc.common.process.KakaduCompress;
import cz.cas.lib.proarc.common.process.TiffToJpgConvert;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import cz.incad.imgsupport.ImageMimeType;
import cz.incad.imgsupport.ImageSupport;
import com.yourmediashelf.fedora.client.FedoraClientException;
import java.awt.image.BufferedImage;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

import static cz.cas.lib.proarc.common.imports.TiffImporter.scale;
import static cz.cas.lib.proarc.common.imports.TiffImporter.writeImage;
import static cz.cas.lib.proarc.common.imports.replaceStream.ReplaceStreamScanner.checkIfFileHasExtension;

/**
 * A class that loads files and eventually creates the necessary files.
 *
 * @author Lukas Sykora
 */
public class FileReader {

    private final ImportSession iSession;
    private static final Logger LOG = Logger.getLogger(FileReader.class.getName());


    public FileReader(ImportSession iSession) {
        this.iSession = iSession;
    }

    public void read(File file, ImportProcess.ImportOptions context) {
        try {
            readImpl(file, context);
        } catch (Exception ex) {
            throw new IllegalStateException(file.getAbsolutePath(), ex);
        }
    }

    private void readImpl(File file, ImportProcess.ImportOptions context) throws IOException, FedoraClientException, AppConfigurationException, MetsExportException {
        String pid = getPid(file);
        List<SearchViewItem> items = iSession.getSearch().find(pid);
        if (items == null || items.isEmpty()) {
            throw new IllegalStateException("Object with pid:\"" + pid + "\" not found in storage.");
        } else if (items.size() > 1) {
            throw new IllegalStateException("More objects (count: " + items.size() + ") with pid: \"" + pid + "\" found in storage.");
        }

        String dsId = toValidDsId(file, context);
        if (dsId == null || dsId.isEmpty()) {
            throw new IllegalStateException("Neznamy stream pro soubor \"" + file.getName() + "\".");
        } else {
            LOG.fine("File \"" + file.getAbsolutePath() + "\" will replace " + dsId + " stream.");
            if (BinaryEditor.RAW_ID.equals(dsId)) {
                createFiles(file, context);
            }
        }
    }

    private void createFiles(File original, ImportProcess.ImportOptions context) throws IOException, AppConfigurationException, MetsExportException {
        String name = getName(original);
        createImages(original, name, context);
        File archivalCopy = createArchivalCopy(original, name, context);
        createUserCopy(original, name, context);
//        createTechnicalMetadata(original, name, archivalCopy, context);
    }

    private void createTechnicalMetadata(File original, String name, File archivalCopy, ImportProcess.ImportOptions context) throws MetsExportException, IOException {
        JhoveContext jhoveContext = context.getJhoveContext();

        // RAW
        Mix mix = JhoveUtility.getMix(original, jhoveContext, null, null, name).getMix();
        if (mix == null) {
            throw new IllegalStateException("Mix content not created: " + original.getAbsolutePath());
        } else {
            String mixContent = MixUtils.toXml(mix, true);
            File mixFile = new File(context.getTargetFolder(), String.format("%s.%s.xml", name, BinaryEditor.RAW_ID));
            writeToFile(mixFile, mixContent);
        }

        // NDK ARCHIVAL
        mix = JhoveUtility.getMix(archivalCopy, jhoveContext, null, null, name).getMix();
        if (mix == null) {
            throw new IllegalStateException("Mix content not created: " + archivalCopy.getAbsolutePath());
        } else {
            String mixContent = MixUtils.toXml(mix, true);
            File mixFile = new File(context.getTargetFolder(), String.format("%s.%s.xml", name, BinaryEditor.NDK_ARCHIVAL_ID));
            writeToFile(mixFile, mixContent);
        }
    }

    private void writeToFile(File file, String message) throws IOException {
        BufferedWriter writer = null;
        try {
            writer = new BufferedWriter(new FileWriter(file));
            writer.append(message);
        } finally {
            if (writer != null) {
                writer.close();
            }
        }
    }

    private void createUserCopy(File original, String name, ImportProcess.ImportOptions context) throws IOException {
        File file;
        if (!containsSibling(original, context.getConfig().getNdkUserFileSuffix())) {
            file = processJp2Copy(original, name, context.getTargetFolder(), BinaryEditor.NDK_USER_ID, context.getConfig().getNdkUserProcessor());
            if (file == null || !file.exists()) {
                throw new FileNotFoundException("Missing user JP2:" + new File(
                        original.getParentFile(), name + context.getConfig().getNdkUserFileSuffix()));
            }

        } else {
            file = getSibling(original, context.getConfig().getNdkUserFileSuffix());
        }
        if (!InputUtils.isJp2000(file)) {
            throw new IllegalStateException("Not a JP2000 content: " + file.getAbsolutePath());
        }
    }

    private File createArchivalCopy(File original, String name, ImportProcess.ImportOptions context) throws IOException {
        File file;
        if (!containsSibling(original, context.getConfig().getNdkArchivalFileSuffix())) {
            file = processJp2Copy(original, name, context.getTargetFolder(), BinaryEditor.NDK_ARCHIVAL_ID, context.getConfig().getNdkArchivalProcessor());
            if (file == null || !file.exists()) {
                throw new FileNotFoundException("Missing archival JP2: " + new File(
                        original.getParentFile(), name + context.getConfig().getNdkArchivalFileSuffix()));
            }
        } else {
            file = getSibling(original, context.getConfig().getNdkArchivalFileSuffix());
        }
        if (!InputUtils.isJp2000(file)) {
            throw new IllegalStateException("Not a JP2000 content: " + file.getAbsolutePath());
        }
        return file;
    }

    private File processJp2Copy(File original, String name, File targetFolder, String dsId, Configuration processorConfig) throws IOException {
        ImageMimeType imageType = ImageMimeType.JPEG2000;
        if (processorConfig != null && !processorConfig.isEmpty()) {
            File jp2File = new File(targetFolder, name + '.' + dsId + '.' + imageType.getDefaultFileExtension());
            String processorType = processorConfig.getString("type");
            ExternalProcess process = null;
            if (KakaduCompress.ID.equals(processorType)) {
                process = new KakaduCompress(processorConfig, original, jp2File);
            }
            if (process != null) {
                process.run();
                if (!process.isOk()) {
                    throw new IOException(jp2File.toString() + "\n" + process.getFullOutput());
                }
                return jp2File;
            }
        }
        return null;

    }

    private void createImages(File original, String name, ImportProcess.ImportOptions context) throws IOException, AppConfigurationException {
        File f = null;
        boolean runCustomConversion = context.getConfig().isTiffToJpgDefined();
        BufferedImage tiff = null;
        ImageMimeType imageType = ImageMimeType.JPEG;
        String targetName;

        // full
        if (!containsSibling(original, context.getConfig().getNdkFullFileSuffix())) {
            targetName = String.format("%s.full.%s", name, imageType.getDefaultFileExtension());
            if (runCustomConversion) {
                f = new File(context.getTargetFolder(), targetName);
                ExternalProcess p = new TiffToJpgConvert(context.getConfig().getConvertorTiffToJpgProcessor(), original, f);
                p.run();
                if (!p.isOk()) {
                    throw new IllegalStateException("Converting tiff to FULL jpg failed: " + p.getFullOutput());
                }
            } else {
                if (tiff == null) {
                    tiff = ImageSupport.readImage(original.toURI().toURL(), ImageMimeType.TIFF);
                }
                f = writeImage(tiff, context.getTargetFolder(), targetName, imageType);
            }
            if (!InputUtils.isJpeg(f)) {
                throw new IllegalStateException("Not a JPEG content: " + f);
            }
        }

        // preview
        if (!containsSibling(original, context.getConfig().getNdkPreviewFileSuffix())) {
            Integer previewMaxHeight = context.getConfig().getPreviewMaxHeight();
            Integer previewMaxWidth = context.getConfig().getPreviewMaxWidth();
            context.getConfig().checkPreviewScaleParams();
            targetName = String.format("%s.preview.%s", name, imageType.getDefaultFileExtension());
            if (runCustomConversion) {
                f = new File(context.getTargetFolder(), targetName);
                ExternalProcess p = new TiffToJpgConvert(context.getConfig().getConvertorTiffToJpgProcessor(), original, f, previewMaxWidth, previewMaxHeight);
                p.run();
                if (!p.isOk()) {
                    throw new IllegalStateException("Converting tiff to PREVIEW jpg failed: " + p.getFullOutput());
                }
            } else {
                if (tiff == null) {
                    tiff = ImageSupport.readImage(original.toURI().toURL(), ImageMimeType.TIFF);
                }
                f = writeImage(
                        scale(tiff, context.getConfig().getPreviewScaling(), previewMaxWidth, previewMaxHeight),
                        context.getTargetFolder(), targetName, imageType);
            }
            if (!InputUtils.isJpeg(f)) {
                throw new IllegalStateException("Not a JPEG content: " + f);
            }
        }

        // thumb
        if (!containsSibling(original, context.getConfig().getNdkThumbnailFileSuffix())) {
            targetName = String.format("%s.thumb.%s", name, imageType.getDefaultFileExtension());
            Integer thumbMaxHeight = context.getConfig().getThumbnailMaxHeight();
            Integer thumbMaxWidth = context.getConfig().getThumbnailMaxWidth();
            context.getConfig().checkThumbnailScaleParams();
            if (runCustomConversion) {
                f = new File(context.getTargetFolder(), targetName);
                ExternalProcess p = new TiffToJpgConvert(context.getConfig().getConvertorTiffToJpgProcessor(), original, f, thumbMaxWidth, thumbMaxHeight);
                p.run();
                if (!p.isOk()) {
                    throw new IllegalStateException("Converting tiff to THUMBNAIL jpg failed: " + p.getFullOutput());
                }
            } else {
                if (tiff == null) {
                    tiff = ImageSupport.readImage(original.toURI().toURL(), ImageMimeType.TIFF);
                }
                f = writeImage(
                        scale(tiff, context.getConfig().getThumbnailScaling(), thumbMaxWidth, thumbMaxHeight),
                        context.getTargetFolder(), targetName, imageType);
            }
            if (!InputUtils.isJpeg(f)) {
                throw new IllegalStateException("Not a JPEG content: " + f);
            }
        }
    }

    private boolean containsSibling(File original, String suffix) {
        File sibling = getSibling(original, suffix);
        return sibling != null && sibling.exists();
    }

    private File getSibling(File original, String suffix) {
        return new File(original.getParentFile(), (getName(original)  + "." + suffix).replace("..", "."));
    }

    protected static String toValidDsId(File file, ImportProcess.ImportOptions context) throws IOException {
        if (checkIfFileHasExtension(file.getName(), context.getConfig().getAltoFileSuffix(), ".xml")) {
            if (checkIfFileHasExtension(file.getName(), "." + BinaryEditor.NDK_ARCHIVAL_ID + ".xml")) {
                return MixEditor.RAW_ID;
            } else if (checkIfFileHasExtension(file.getName(), "." + BinaryEditor.RAW_ID + ".xml")) {
                return MixEditor.NDK_ARCHIVAL_ID;
            } else {
                return AltoDatastream.ALTO_ID;
            }
        } else if (checkIfFileHasExtension(file.getName(), context.getConfig().getPlainOcrFileSuffix())) {
            return StringEditor.OCR_ID;
        } else if (checkIfFileHasExtension(file.getName(),  context.getConfig().getRawFileSuffix(), "." + ImageMimeType.TIFF.getDefaultFileExtension()) && InputUtils.isTiff(file)) {
            return BinaryEditor.RAW_ID;
        } else if (checkIfFileHasExtension(file.getName(), context.getConfig().getNdkFullFileSuffix(), ".full." + ImageMimeType.JPEG.getDefaultFileExtension()) && InputUtils.isJpeg(file)) {
            return BinaryEditor.FULL_ID;
        } else if (checkIfFileHasExtension(file.getName(), context.getConfig().getNdkArchivalFileSuffix(), "." + BinaryEditor.NDK_ARCHIVAL_ID + "." + ImageMimeType.JPEG2000.getDefaultFileExtension()) && InputUtils.isJp2000(file)) {
            return BinaryEditor.NDK_ARCHIVAL_ID;
        } else if (checkIfFileHasExtension(file.getName(), context.getConfig().getNdkUserFileSuffix(), "." + BinaryEditor.NDK_USER_ID + "." + ImageMimeType.JPEG2000.getDefaultFileExtension()) && InputUtils.isJp2000(file)) {
            return BinaryEditor.NDK_USER_ID;
        } else if (checkIfFileHasExtension(file.getName(), context.getConfig().getNdkPreviewFileSuffix(), ".preview." + ImageMimeType.JPEG.getDefaultFileExtension()) && InputUtils.isJpeg(file)) {
            return BinaryEditor.PREVIEW_ID;
        } else if (checkIfFileHasExtension(file.getName(), context.getConfig().getNdkThumbnailFileSuffix(), ".thumb." + ImageMimeType.JPEG.getDefaultFileExtension()) && InputUtils.isJpeg(file)) {
            return BinaryEditor.THUMB_ID;
        } else if (checkIfFileHasExtension(file.getName(), context.getConfig().getNdkSourceAudioFileSuffix().toArray()) && InputUtils.isWave(file)) {
            return BinaryEditor.RAW_AUDIO_ID;
        } else if (checkIfFileHasExtension(file.getName(), context.getConfig().getNdkArchivalAudioFileSuffix().toArray())) {
            if (checkIfFileHasExtension(file.getName(), ".flac") && InputUtils.isFlac(file)) {
                return BinaryEditor.NDK_AUDIO_ARCHIVAL_FLAC_ID;
            } else if (checkIfFileHasExtension(file.getName(), ".wav") && InputUtils.isWave(file)) {
                return BinaryEditor.NDK_AUDIO_ARCHIVAL_ID;
            } else {
                return null;
            }
        } else if (checkIfFileHasExtension(file.getName(), context.getConfig().getNdkUserAudioFileSuffix().toArray())) {
            if (checkIfFileHasExtension(file.getName(), ".ogg") && InputUtils.isOgg(file)) {
                return BinaryEditor.NDK_AUDIO_USER_OGG_ID;
            } else if (checkIfFileHasExtension(file.getName(), ".mp3") && InputUtils.isMp3(file)) {
                return BinaryEditor.NDK_AUDIO_USER_ID;
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    protected static String getName(File file) {
        String fileName = file.getName();
        int index = fileName.indexOf('.');
        return index > 0 ? fileName.substring(0, index) : fileName;
    }

    protected static String getPid(File file) {
        String pid = getName(file);
        if (!pid.startsWith("uuid:")) {
            pid = "uuid:" + pid;
        }
        return pid;
    }

    static class ImportSession {

        private final ImportBatchManager ibm;
        private final ImportProcess.ImportOptions options;
        private final Batch batch;
        private final LocalStorage locals;
        private final SearchView search;
        private final Storage typeOfStorage;
        private RemoteStorage remotes;
        private AkubraStorage akubraStorage;

        public ImportSession(ImportBatchManager ibm, ImportProcess.ImportOptions options, AppConfiguration config) {
            this.typeOfStorage = config.getTypeOfStorage();
            try {
                if (Storage.FEDORA.equals(typeOfStorage)) {
                    this.remotes = RemoteStorage.getInstance();
                    this.search = this.remotes.getSearch();
                } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                    AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(config.getConfigHome());
                    this.akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                    this.search = this.akubraStorage.getSearch();
                } else {
                    throw new IllegalStateException("Unsupported type of storage: " + typeOfStorage);
                }
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
            this.locals = new LocalStorage();
            this.ibm = ibm;
            this.options = options;
            this.batch = options.getBatch();
        }

        public ImportBatchManager getImportManager() {
            return ibm;
        }

        public LocalStorage getLocals() {
            return locals;
        }

        public RemoteStorage getRemotes() {
            return remotes;
        }

        public Storage getTypeOfStorage() {
            return typeOfStorage;
        }

        public AkubraStorage getAkubraStorage() {
            return akubraStorage;
        }

        public LocalStorage.LocalObject findLocalObject(ImportBatchManager.BatchItemObject bio) {
            return bio == null || bio.getPid() == null
                    ? null : locals.load(bio.getPid(), bio.getFile());
        }

        public SearchView getSearch() {
            return search;
        }

        public ImportBatchManager.BatchItemObject findItem(String pid) {
            return ibm.findBatchObject(batch.getId(), pid);
        }

        public ImportBatchManager.BatchItemObject addObject(LocalStorage.LocalObject lobj, boolean root) throws DigitalObjectException {
            ImportBatchManager.BatchItemObject bio = ibm.addLocalObject(options.getBatch(), lobj);
            if (root) {
                LocalStorage.LocalObject rootObj = ibm.getRootObject(batch);
                RelationEditor editor = new RelationEditor(rootObj);
                List<String> members = editor.getMembers();
                members.add(lobj.getPid());
                editor.setMembers(members);
                editor.write(editor.getLastModified(), options.getUsername());
                rootObj.flush();
            }
            return bio;
        }
    }
}
