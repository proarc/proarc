package cz.cas.lib.proarc.common.export.bagit;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import javax.xml.bind.DatatypeConverter;
import org.apache.commons.configuration.Configuration;

public class BagitExport {

    private File exportFolder;
    private File bagitFolder;
    private final AppConfiguration appConfiguration;


    public BagitExport(AppConfiguration appConfiguration, File exportFolder) {
        this.appConfiguration = appConfiguration;
        this.exportFolder = exportFolder;
    }


    public void bagit() throws IOException {
        BagitProcess process = new BagitProcess(appConfiguration.getBagitExportPosProcessor(), exportFolder);
        if (process != null) {
            process.run();

            if (!process.isOk()) {
                throw new IOException("Processing Bagit failed. \n" + process.getFullOutput());
            }
        }
    }

    public void zip() throws IOException {
        ZipProcess process = new ZipProcess(appConfiguration.getZipExportPostProcessor(), exportFolder, getDestinationFolder(appConfiguration.getZipExportPostProcessor()));
        if (process != null) {
            process.run();

            if (!process.isOk()) {
                throw new IOException("Zipping Bagit failed. \n" + process.getFullOutput());
            }
        }
    }

    public void deleteExportFolder() {
        MetsUtils.deleteFolder(exportFolder);
    }

    private File getDestinationFolder(Configuration zipExportPostProcessor) {
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

    public void moveToBagitFolder() throws IOException {
        File parentFile = exportFolder.getParentFile();
        bagitFolder = new File(parentFile, "bagit_" + exportFolder.getName());
        if (bagitFolder.exists()) {
            MetsUtils.deleteFolder(bagitFolder);
        }
        if(!bagitFolder.mkdir()) {
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
            byte[] bytes = Files.readAllBytes(Paths.get(file.getPath()));
            byte[] hash = MessageDigest.getInstance("MD5").digest(bytes);
            String hashValue = DatatypeConverter.printHexBinary(hash);
            checksumBuilder.append("MD5").append(" ").append(hashValue.toLowerCase());
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
}
