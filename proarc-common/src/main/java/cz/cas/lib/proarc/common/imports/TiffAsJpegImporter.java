/*
 * Copyright (C) 2017 Jakub Kremlacek
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

package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.imports.FileSet.FileEntry;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.process.ExternalProcess;
import cz.cas.lib.proarc.common.process.GhostConvert;
import cz.cas.lib.proarc.common.process.VIPSConvert;
import cz.incad.imgsupport.ImageMimeType;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.io.FilenameUtils;

/**
 *
 * Prepares tiff image from jpeg at input for TiffImporter
 *
 * Original .jpg image is not used after the conversion into .tiff,
 * .tiff image is furthermore treated as original image
 *
 * @author Jakub Kremlacek
 */
public class TiffAsJpegImporter implements ImageImporter {

    private static final Logger LOG = Logger.getLogger(TiffAsJpegImporter.class.getName());

    private TiffImporter importer;

    //set limit filesize for use of small filesize convertor
    private static final long SMALL_IMAGE_SIZE_LIMIT = 5L * 1000 * 1000;

    public TiffAsJpegImporter(TiffImporter importer) {
        this.importer = importer;
    }

    @Override
    public boolean accept(FileSet fileSet) { return isJpeg(fileSet); }

    @Override
    public BatchItemObject consume(FileSet fileSet, ImportOptions ctx) {

        FileEntry jpegEntry = findJpeg(fileSet);
        // check jpeg file
        if (jpegEntry == null) {
            return null;
        }

        try {
            FileEntry tiff;
            File tiffFile = new File(jpegEntry.getFile().getParent(), FilenameUtils.removeExtension(jpegEntry.getFile().getName()) + ".tiff");
            if (jpegEntry.getFile().length() > SMALL_IMAGE_SIZE_LIMIT) {
                tiff = convertToTiff(jpegEntry, ctx.getConfig().getConvertorJpgLargeProcessor(), tiffFile);
            } else {
                tiff = convertToTiff(jpegEntry, ctx.getConfig().getConvertorJpgSmallProcessor(), tiffFile);
            }

            if (tiff == null) {
                return null;
            }

            fileSet.getFiles().add(tiff);

            return importer.consume(fileSet, ctx);
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, jpegEntry.toString(), ex);
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

    private FileEntry convertToTiff(FileEntry jp, Configuration processorConfig, File tiff) throws IOException {
        if (processorConfig == null || processorConfig.isEmpty()) {
            throw new IllegalArgumentException("Convertor config must be set.");
        }

        //conversion was done before
        if (tiff.exists()) {
            return null;
        }

        String processorType = processorConfig.getString("type");
        ExternalProcess process;
        if (GhostConvert.ID.equals(processorType)) {
            process = new GhostConvert(processorConfig, jp.getFile(), tiff);
        } else if (VIPSConvert.ID.equals(processorType)) {
            process = new VIPSConvert(processorConfig, jp.getFile(), tiff);
        } else {
            throw new IllegalArgumentException("No suitable convertor found.");
        }
        process.run();
        if (!process.isOk()) {
            throw new IOException(tiff.toString() + "\n" + process.getFullOutput());
        }
        return new FileEntry(tiff);
    }

    public File getTiff(FileSet fileSet, ImportOptions ctx) {
        FileEntry jpegEntry = findJpeg(fileSet);
        // check jpeg file
        if (jpegEntry == null) {
            return null;
        }

        try {
            FileEntry tiff;
            File tiffFile = new File(ctx.getTargetFolder(), jpegEntry.getFile().getParentFile().getName() + ".tiff");
            if (jpegEntry.getFile().length() > SMALL_IMAGE_SIZE_LIMIT) {
                tiff = convertToTiff(jpegEntry, ctx.getConfig().getConvertorJpgLargeProcessor(), tiffFile);
            } else {
                tiff = convertToTiff(jpegEntry, ctx.getConfig().getConvertorJpgSmallProcessor(), tiffFile);
            }

            if (tiff == null) {
                return null;
            }
            return tiff.getFile();
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, jpegEntry.toString(), ex);
        }
        return null;
    }
}
