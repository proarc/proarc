package cz.cas.lib.proarc.common.export.bagit;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import java.io.File;
import java.io.IOException;
import org.apache.commons.configuration.Configuration;

public class BagitExport {

    private final File exportFolder;
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
}
