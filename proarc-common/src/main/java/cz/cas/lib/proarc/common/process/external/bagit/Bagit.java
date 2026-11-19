package cz.cas.lib.proarc.common.process.external.bagit;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetUploader;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.logging.Logger;
import net.lingala.zip4j.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.ZipParameters;
import net.lingala.zip4j.model.enums.CompressionLevel;
import net.lingala.zip4j.model.enums.CompressionMethod;
import net.lingala.zip4j.model.enums.EncryptionMethod;
import org.apache.commons.io.FileUtils;

public class Bagit {

    private File exportFolder;
    private File bagitFolder;
    private File destinationFolder;
    private final AppConfiguration appConfiguration;

    private static final Logger LOG = Logger.getLogger(Bagit.class.getName());

    public Bagit(AppConfiguration appConfiguration, File exportFolder) {
        this.appConfiguration = appConfiguration;
        this.exportFolder = exportFolder;
    }


    public void bagit() throws IOException {
        switch (appConfiguration.getBagitMode()) {
            case "java":
                runBagitJavaProcess();
                break;
            case "external":
                runBagitCommandProcess();
                break;
            case "script":
                String scriptPath = appConfiguration.getBagitScriptPath();
                if (scriptPath == null || scriptPath.isBlank() || !new File(scriptPath).isFile()) {
                    throw new IOException("BagIt script does not exist: " + scriptPath);
                }
                File script = new File(scriptPath);
                runBagitExternalProcess(script);
                break;
            default:
                throw new IOException("Unsupported export.bagit.mode: " + appConfiguration.getBagitMode()
                        + " (expected java, external or script)");
        }
    }

    public void zip() throws IOException {
//        File newFile = new File(exportFolder, exportFolder.getName());
//        newFile.createNewFile();
        File tmpFile = new File(exportFolder.getParentFile(), exportFolder.getName() + "_tmp");
        Files.createDirectory(tmpFile.toPath());
        Files.move(exportFolder.toPath(), new File(tmpFile, exportFolder.getName()).toPath());
        File file2Zip = new File(tmpFile.getParentFile(), tmpFile.getName().substring(0, tmpFile.getName().length() - 4));
        Files.move(tmpFile.toPath(), file2Zip.toPath());
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
            FileUtils.moveFile(zipFolder, new File(bagitFolder, zipFolder.getName()));
        }
    }

    public void createMd5File() throws IOException, NoSuchAlgorithmException {
        if (!bagitFolder.exists()) {
            throw new IOException("Bagit folder doesn´t exists " + bagitFolder.getName());
        }
        StringBuilder checksumBuilder = new StringBuilder();
        for (File file : bagitFolder.listFiles()) {

            checksumBuilder.append("MD5").append(" ").append(BagitJava.md5(file.toPath()));
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

    public void prepare() throws IOException {
        File newName = new File(this.exportFolder.getParentFile(), "archive_" + this.exportFolder.getName());
        Files.move(this.exportFolder.toPath(), newName.toPath());
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

    public void moveToSpecifiedDirectories() throws IOException {
        moveToSpecifiedDirectories(appConfiguration.getBagitExportPath());
    }

    public void moveToSpecifiedDirectories(String bagitExportPath) throws IOException {
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
                Files.createDirectories(bagitExportRoot.toPath());
            }

            for (File bagitFile : bagitFolder.listFiles()) {
                File newFile = new File(bagitExportRoot, bagitFile.getName());
                FileUtils.moveFile(bagitFile, newFile);
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

    public File getDestinationFile() {
        return destinationFolder;
    }

    public void uploadToLtpCesnet(File archive, String ltpCesnetToken, String pid) throws IOException, DigitalObjectException, InterruptedException {
        this.destinationFolder = archive;
        uploadToLtpCesnet(ltpCesnetToken, pid);
    }

    public void uploadToLtpCesnet(String ltpCesnetToken, String pid) throws IOException, DigitalObjectException, InterruptedException {
        String operation = java.util.UUID.nameUUIDFromBytes((destinationFolder.getAbsolutePath() + ":"
                + destinationFolder.length() + ":" + destinationFolder.lastModified())
                .getBytes(java.nio.charset.StandardCharsets.UTF_8)).toString();
        new CesnetUploader(appConfiguration).upload(destinationFolder, operation, pid);
    }

    private void runBagitExternalProcess(File script) throws IOException {
        BagitExternalScriptProcess process = new BagitExternalScriptProcess(appConfiguration.getBagitExportPostProcessor(), script, exportFolder);
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
        new BagitJava().create(exportFolder.toPath());
    }

    private void runBagitCommandProcess() throws IOException {
        BagitExternalProcess process = new BagitExternalProcess(appConfiguration.getBagitExportPostProcessor(), exportFolder);
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
