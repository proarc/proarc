package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.imports.FileSet.FileEntry;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.process.ExternalProcess;
import cz.cas.lib.proarc.common.process.KakaduConvert;
import org.apache.commons.configuration.Configuration;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;

/**
 *
 * Prepares tiff image from jp2 at input for TiffImporter
 *
 * @author Jakub Kremlacek
 */
public class Jp2Importer extends TiffImporter {

    public Jp2Importer(ImportBatchManager ibm) {
        super(ibm);
    }

    @Override
    public boolean accept(FileSet fileSet) { return isJp2(fileSet); }

    private boolean isJp2(FileSet fileSet) { return findJp2(fileSet) != null;}

    private FileEntry findJp2(FileSet fileSet) {

        for (FileEntry entry : fileSet.getFiles()) {
            try {
                if (InputUtils.isJp2000(entry.getFile())) {
                    return entry;
                }
            } catch (IOException e) {
                LOG.log(Level.SEVERE, entry.toString(), e);
            }
        }
        return null;
    }

    @Override
    public BatchItemObject consume(FileSet fileSet, ImportOptions ctx) {

        FileEntry imageFile = fileSet.getFiles().get(0);

        try {
            FileEntry tiff = convertToTiff(imageFile, ctx.getConfig().getConvertorJp2Processor());

            if (tiff == null) return null;

            fileSet.getFiles().add(tiff);

            return super.consume(fileSet, ctx);
        } catch (Throwable ex) {
            LOG.log(Level.SEVERE, imageFile.toString(), ex);
        }

        return null;
    }

    private FileEntry convertToTiff(FileEntry jp, Configuration processorConfig) throws IOException {
        if (processorConfig != null && !processorConfig.isEmpty()) {
            File tiff = new File(jp.getFile().toPath().getParent().toString(), jp.getFile().getName().substring(0, jp.getFile().getName().lastIndexOf('.')) + ".tiff");

            //conversion was done before
            if (tiff.exists()) return null;

            String processorType = processorConfig.getString("type");
            ExternalProcess process = null;
            if (KakaduConvert.ID.equals(processorType)) {
                process = new KakaduConvert(processorConfig, jp.getFile(), tiff);
            }
            if (process != null) {
                process.run();
                if (!process.isOk()) {
                    throw new IOException(tiff.toString() + "\n" + process.getFullOutput());
                }
            }
            return  new FileEntry(tiff);
        }
        return null;
    }
}
