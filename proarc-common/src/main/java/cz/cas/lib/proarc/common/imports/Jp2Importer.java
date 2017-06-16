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
import cz.cas.lib.proarc.common.process.KakaduExpand;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.io.FilenameUtils;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * Prepares tiff image from jp2 at input for TiffImporter
 *
 * @author Jakub Kremlacek
 */
public class Jp2Importer implements ImageImporter {

    private static final Logger LOG = Logger.getLogger(Jp2Importer.class.getName());

    private TiffImporter importer;

    public Jp2Importer(TiffImporter importer) {
        this.importer = importer;
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

            return importer.consume(fileSet, ctx);
        } catch (Throwable ex) {
            LOG.log(Level.SEVERE, imageFile.toString(), ex);
        }

        return null;
    }

    private FileEntry convertToTiff(FileEntry jp, Configuration processorConfig) throws IOException {
        if (processorConfig != null && !processorConfig.isEmpty()) {
            File tiff = new File(
                    jp.getFile().getParent(),
                    FilenameUtils.removeExtension(jp.getFile().getName()) + ".tiff");

            //conversion was done before
            if (tiff.exists()) return null;

            String processorType = processorConfig.getString("type");
            ExternalProcess process = null;
            if (KakaduExpand.ID.equals(processorType)) {
                process = new KakaduExpand(processorConfig, jp.getFile(), tiff);
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
