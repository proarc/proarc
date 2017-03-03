package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.imports.FileSet.FileEntry;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.process.ExternalProcess;
import cz.cas.lib.proarc.common.process.GhostConvert;
import cz.cas.lib.proarc.common.process.VIPSConvert;
import cz.incad.imgsupport.ImageMimeType;
import org.apache.commons.configuration.Configuration;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;

/**
 *
 * Prepares tiff image from jpeg at input for TiffImporter
 *
 * @author Jakub Kremlacek
 */
public class JpegImporter extends TiffImporter {

    //set limit filesize for use of small filesize convertor
    private static final long SMALL_IMAGE_SIZE_LIMIT = 5L * 1000 * 1000;

    public JpegImporter(ImportBatchManager ibm) {
        super(ibm);
    }

    @Override
    public boolean accept(FileSet fileSet) { return isJpeg(fileSet); }

    @Override
    public BatchItemObject consume(FileSet fileSet, ImportOptions ctx) {

        FileEntry imageFile = fileSet.getFiles().get(0);

        try {
            FileEntry tiff;
            if (imageFile.getFile().length() > SMALL_IMAGE_SIZE_LIMIT) {
                tiff = convertToTiff(imageFile, ctx.getConfig().getConvertorJpgLargeProcessor());
            } else {
                tiff = convertToTiff(imageFile, ctx.getConfig().getConvertorJpgSmallProcessor());
            }

            if (tiff == null) return null;

            fileSet.getFiles().add(tiff);

            return super.consume(fileSet, ctx);
        } catch (Throwable ex) {
            LOG.log(Level.SEVERE, imageFile.toString(), ex);
        }

        return null;
    }

    private boolean isJpeg(FileSet fileSet) { return findJpeg(fileSet) != null;}

    private FileEntry findJpeg(FileSet fileSet) {

        for (FileEntry entry : fileSet.getFiles()) {
            String mimetype = entry.getMimetype();
            if (ImageMimeType.JPEG.getMimeType().equals(mimetype)) {
                return entry;
            }
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
            if (GhostConvert.ID.equals(processorType)) {
                process = new GhostConvert(processorConfig, jp.getFile(), tiff);
            } else if (VIPSConvert.ID.equals(processorType)) {
                process = new VIPSConvert(processorConfig, jp.getFile(), tiff);
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
