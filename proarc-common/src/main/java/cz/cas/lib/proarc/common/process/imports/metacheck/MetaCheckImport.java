package cz.cas.lib.proarc.common.process.imports.metacheck;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.FileSet;
import cz.cas.lib.proarc.common.process.imports.ImportHandler;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import cz.cas.lib.proarc.common.process.imports.TiffImporter;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class MetaCheckImport implements ImportHandler {

    @Override
    public int estimateItemNumber(ImportProcess.ImportOptions importConfig) throws IOException {
        ImportFileScanner scanner = new ImportFileScanner();
        List<File> files = scanner.findDigitalContent(importConfig.getImportFolder());
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        TiffImporter tiffImporter = new TiffImporter(BatchManager.getInstance());
        int count = 0;
        for (FileSet fileSet : fileSets) {
            if (tiffImporter.accept(fileSet)) {
                count++;
            }
        }
        return count;
    }

    @Override
    public boolean isImportable(File folder) {
        String[] fileNames = folder.list();
        TiffImporter tiffImporter = new TiffImporter(BatchManager.getInstance());
        for (String fileName : fileNames) {
            if (ImportFileScanner.IMPORT_STATE_FILENAME.equals(fileName)) {
                continue;
            }
            File file = new File(folder, fileName);
            if (file.isFile() && file.canRead()) {
                List<FileSet> fileSets = ImportFileScanner.getFileSets(Arrays.asList(file));
                if (tiffImporter.accept(fileSets.get(0))) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {

    }
}
