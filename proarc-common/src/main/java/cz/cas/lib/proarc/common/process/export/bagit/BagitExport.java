package cz.cas.lib.proarc.common.process.export.bagit;

import com.google.common.hash.HashCode;
import com.google.common.hash.Hashing;
import com.google.common.io.ByteSource;
import com.google.common.io.Files;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.logging.Logger;
import org.apache.commons.io.FileUtils;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import net.lingala.zip4j.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.ZipParameters;
import net.lingala.zip4j.model.enums.CompressionLevel;
import net.lingala.zip4j.model.enums.CompressionMethod;
import net.lingala.zip4j.model.enums.EncryptionMethod;

public class BagitExport {

    private File exportFolder;
    private File bagitFolder;
    private File destinationFolder;
    private final AppConfiguration appConfiguration;

    private static final Logger LOG = Logger.getLogger(BagitExport.class.getName());

    public BagitExport(AppConfiguration appConfiguration, File exportFolder) {
        this.appConfiguration = appConfiguration;
        this.exportFolder = exportFolder;
    }


    public void bagit() throws IOException {
        String scriptPath = appConfiguration.getBagitScriptPath();
        if (scriptPath == null || scriptPath.isEmpty()) {
            runBagitJavaProcess();
        } else {
            File script = new File(scriptPath);
            if (script == null || !script.exists()) {
                runBagitJavaProcess();
            } else {
                runBagitExternalProcess(script);
            }
        }
    }

    public void zip() throws IOException {
//        File newFile = new File(exportFolder, exportFolder.getName());
//        newFile.createNewFile();
        File tmpFile = new File(exportFolder.getParentFile(), exportFolder.getName() + "_tmp");
        tmpFile.mkdir();
        exportFolder.renameTo(new File(tmpFile, exportFolder.getName()));
        File file2Zip = new File(tmpFile.getParentFile(), tmpFile.getName().substring(0, tmpFile.getName().length() - 4));
        tmpFile.renameTo(file2Zip);
        File zipFileName = createZipFile();
        try {
            ZipFile zipFile = new ZipFile(zipFileName);
            ZipParameters zipParameters = new ZipParameters();
            zipParameters.setEncryptionMethod(EncryptionMethod.ZIP_STANDARD);
            zipParameters.setCompressionMethod(CompressionMethod.DEFLATE);
            zipParameters.setCompressionLevel(CompressionLevel.NORMAL);
            zipParameters.setIncludeRootFolder(true);
            zipParameters.setDefaultFolderPath(file2Zip.getAbsolutePath());
            zipFile.addFiles(listZipFiles(file2Zip), zipParameters);
        } catch (ZipException e) {
            throw new RuntimeException(e);
        }

    }

    private ArrayList listZipFiles(File exportFolder) {
        ArrayList<File> files = new ArrayList<File>();
        ArrayList<File> subfiles = new ArrayList<File>();
        for (File file : exportFolder.listFiles()) {
            if (file.isFile()) {
                files.add(file);
            } else if (file.isDirectory()) {
                subfiles.addAll(listZipFiles(file));
            }
        }
        files.addAll(subfiles);
        return files;
    }

    public void deleteExportFolder() {
        MetsUtils.deleteFolder(exportFolder);
    }

    private File createZipFile() {
        return new File(exportFolder.getAbsolutePath() + ".zip");
    }

    public static File findExportFolder(File userExportFolder, String folderName) {
        if (folderName.startsWith("uuid:")) {
            folderName = folderName.substring(5);
        }
        for (File file : userExportFolder.listFiles()) {
            if (file.isDirectory() && file.exists() && folderName.equals(file.getName())) {
                return file;
            }
        }
        return null;
    }

    public static File findNdkExportFolder(String folderName) {
        if (folderName.startsWith("uuid:")) {
            folderName = folderName.substring(5);
        }
        File targetFolder = new File(folderName);
        if (targetFolder.exists()) {
            return targetFolder;
        }
        return null;
    }

    public void moveToBagitFolder() throws IOException {
        File parentFile = exportFolder.getParentFile();
        bagitFolder = new File(parentFile, "bagit_" + exportFolder.getName());
        if (bagitFolder.exists()) {
            MetsUtils.deleteFolder(bagitFolder);
            if (bagitFolder.exists()) {
                throw new IOException("Impossible to delete previous export " + bagitFolder.getAbsolutePath());
            }
        }
        if (!bagitFolder.mkdir()) {
            throw new IOException("Impossible to create folder " + bagitFolder.getName());
        }

        File zipFolder = new File(exportFolder.getAbsolutePath() + ".zip");
        if (!zipFolder.exists()) {
            throw new IOException("Zip file doesn´t exists.");
        } else {
            zipFolder.renameTo(new File(bagitFolder, zipFolder.getName()));
        }
    }

    public void createMd5File() throws IOException, NoSuchAlgorithmException {
        if (!bagitFolder.exists()) {
            throw new IOException("Bagit folder doesn´t exists " + bagitFolder.getName());
        }
        StringBuilder checksumBuilder = new StringBuilder();
        for (File file : bagitFolder.listFiles()) {
//            byte[] bytes = Files.readAllBytes(Paths.get(file.getPath()));
//            byte[] hash = MessageDigest.getInstance("MD5").digest(bytes);
//            String hashValue = DatatypeConverter.printHexBinary(hash);
            ByteSource byteSource = Files.asByteSource(file);
            HashCode hc = byteSource.hash(Hashing.md5());
            checksumBuilder.append("MD5").append(" ").append(hc.toString().toLowerCase());
        }
        File checksumFile = new File(bagitFolder, exportFolder.getName() + ".sums");
        BufferedWriter writer = null;
        try {
            writer = new BufferedWriter(new FileWriter(checksumFile));
            writer.append(checksumBuilder);
        } finally {
            if (writer != null) {
                writer.close();
            }
        }
    }

    public void prepare() {
        File newName = new File(this.exportFolder.getParentFile(), "archive_" + this.exportFolder.getName());
        this.exportFolder.renameTo(newName);
        this.exportFolder = newName;
    }

    public void prepareFoxml() {
        File newName = new File(this.exportFolder.getParentFile(), "archive_" + this.exportFolder.getName());
        try {
            FileUtils.copyDirectory(exportFolder, newName);
            this.exportFolder.renameTo(newName);
            this.exportFolder = newName;
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void deleteTargetFolderContent(File targetFolder) {
        for (File file : targetFolder.listFiles()) {
            if (!file.getName().endsWith(".log")) {
                MetsUtils.deleteFolder(file);
            }
        }
    }

    public void moveToSpecifiedDirectories() {
        String bagitExportPath = appConfiguration.getBagitExportPath();
        if (bagitExportPath == null || bagitExportPath.isEmpty()) {
            for (File newFile : bagitFolder.listFiles()) {
                if (newFile.getName().endsWith(".zip")) {
                    destinationFolder = newFile;
                    LOG.info("Folder: " + destinationFolder.getAbsolutePath());
                }
            }
            // nikam se nic nepresouva, zustava v puvodnim adresari
            return;
        } else {
            File bagitExportRoot = new File(bagitExportPath);
            LOG.info("Moving to folder: " + bagitExportRoot.getAbsolutePath());
            if (!bagitExportRoot.exists()) {
                bagitExportRoot.mkdir();
            }

            for (File bagitFile : bagitFolder.listFiles()) {
                File newFile = new File(bagitExportRoot, bagitFile.getName());
                bagitFile.renameTo(newFile);
                if (newFile.getName().endsWith(".zip")) {
                    destinationFolder = newFile;
                    LOG.info("Folder: " + destinationFolder.getAbsolutePath());
                }
            }
            LOG.info("Deleting folder: " + bagitFolder.getAbsolutePath());
            MetsUtils.deleteFolder(bagitFolder);
        }
    }

    public void moveToSpecifiedFoxmlDirectories() {
        String bagitFoxmlExportPath = appConfiguration.getBagitFoxmlExportPath();
        if (bagitFoxmlExportPath == null || bagitFoxmlExportPath.isEmpty()) {
            // nikam se nic nepresouva, zustava v puvodnim adresari
            return;
        } else {
            File bagitExportRoot = new File(bagitFoxmlExportPath);
            if (!bagitExportRoot.exists()) {
                bagitExportRoot.mkdir();
            }
            for (File bagitFile : bagitFolder.listFiles()) {
                bagitFile.renameTo(new File(bagitExportRoot, bagitFile.getName()));
            }
            MetsUtils.deleteFolder(bagitFolder);
        }
    }

    public void uploadToLtpCesnet(String ltpCesnetToken, String pid) throws IOException, DigitalObjectException, InterruptedException {
        if (destinationFolder != null && destinationFolder.exists()) {
            String metadata = createMetadataJson(destinationFolder.getName(), pid);
            if (!isValid(metadata)) {
                throw new IOException("Invalid json:" + metadata);
            }
            LtpCesnetProcess process = new LtpCesnetProcess(appConfiguration.getLtpCesnetExportPostProcessor(), ltpCesnetToken, appConfiguration.getLtpCesnetGroupToken(), appConfiguration.getLtpCesnetScriptPath(), destinationFolder, metadata);

            if (process != null) {
                process.run();
                Thread.sleep(60000);

//                if (!process.isOk()) {
//                    throw new IOException("Processing Ltp Cesnet upload failed. \n" + process.getFullOutput());
//                }
            }
        } else {
            LOG.severe("Error in getting destination folder");
        }
    }

    private boolean isValid(String json) {
        try {
            new JSONObject(json);
        } catch (JSONException ex) {
            return false;
        }
        return true;
    }

    private String toPid(String folderName) {
        return folderName;
    }

    private String createMetadataJson(String folderName, String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fo = dom.find(pid, null);
        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
        NdkMapper.Context context = new NdkMapper.Context(handler);
        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();


        String urnNbn = getUrnNbn(mods);
        String title = getTitle(mods);

        return createJson(pid, folderName, urnNbn, title);
    }

    private String getTitle(ModsDefinition mods) {
        StringBuilder builder = new StringBuilder();
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            if (titleInfo.getTitle().size() > 0) {
                builder.append(titleInfo.getTitle().get(0).getValue());
            }
            if (titleInfo.getSubTitle().size() > 0) {
                builder.append(": ");
                builder.append(titleInfo.getSubTitle().get(0).getValue());
            }
            if (titleInfo.getPartName().size() > 0) {
                builder.append(" - ");
                builder.append(titleInfo.getPartName().get(0).getValue());
            }
            if (titleInfo.getPartNumber().size() > 0) {
                builder.append(" / ");
                builder.append(titleInfo.getPartNumber().get(0).getValue());
            }
        }
        return builder.toString();
    }

    private String getUrnNbn(ModsDefinition mods) {
        for (IdentifierDefinition identifier : mods.getIdentifier()) {
            if ("urnnbn".equals(identifier.getType()) && (identifier.getInvalid() == null || "".equals(identifier.getInvalid()) || "false".equals(identifier.getInvalid()))) {
                return identifier.getValue();
            }
        }
        return null;
    }

    private String createJson(String pid, String folderName, String urnNbn, String title) {
        StringWriter writer = new StringWriter();
        writer.append("{\"name\":\"").append(pid).append("\"");
        if ((folderName != null && !folderName.isEmpty()) || (urnNbn != null && !urnNbn.isEmpty()) || (title != null && !title.isEmpty())) {
            writer.append(", \"user_metadata\":{");
            if (folderName != null && !folderName.isEmpty()) {
                writer.append("\"folder\":\"").append(folderName).append("\"");
                if ((urnNbn != null && !urnNbn.isEmpty()) || (title != null && !title.isEmpty())) {
                    writer.append(", ");
                }
            }
            if (urnNbn != null && !urnNbn.isEmpty()) {
                writer.append("\"urn:nbn\":\"").append(urnNbn).append("\"");
                if ((title != null && !title.isEmpty())) {
                    writer.append(", ");
                }
            }
            if (title != null && !title.isEmpty()) {
                writer.append("\"title\":\"").append(title.replaceAll("\"", "\\\\\"").replaceAll("\'", "\\\\\'")).append("\"");
            }
            writer.append("}");
        }
        writer.append("}");
        return writer.toString();
    }

    private void runBagitExternalProcess(File script) throws IOException {
        BagitExternalProcess process = new BagitExternalProcess(appConfiguration.getBagitExportPostProcessor(), script, exportFolder);
        if (process != null) {
            process.run();
            if (!process.isOk(exportFolder)) {
                throw new IOException("Processing external Bagit failed. \n" + process.getFullOutput());
            }
        } else {
            throw new IOException("Processing external Bagit failed - process is null.");
        }
    }

    private void runBagitJavaProcess() throws IOException {
        BagitProcess process = new BagitProcess(appConfiguration.getBagitExportPostProcessor(), exportFolder);
        if (process != null) {
            process.run();
            if (!process.isOk()) {
                throw new IOException("Processing Bagit failed. \n" + process.getFullOutput());
            }
        } else {
            throw new IOException("Processing Bagit failed - process is null.");
        }
    }
}
