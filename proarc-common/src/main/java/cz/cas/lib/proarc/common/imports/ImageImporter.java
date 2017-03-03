package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;

/**
 * Represents processor for main image at input.
 *
 * @author Jakub Kremlacek
 */
public interface ImageImporter {

    /**
     * checks whether current ImageImporter is capable of importing specified fileset
     *
     * @param fileSet FileSet containing imagefile to be imported
     * @return true if FileSet contains importable imagetype
     */
    boolean accept(FileSet fileSet);

    /**
     * processes specified fileset with options defined in ImportOptions
     *
     * @param fileSet FileSet to be consumed and processed
     * @param ctx setup options for importer
     * @return returns object with processed images from fileSet
     */
    BatchItemObject consume(FileSet fileSet, ImportProcess.ImportOptions ctx);
}
