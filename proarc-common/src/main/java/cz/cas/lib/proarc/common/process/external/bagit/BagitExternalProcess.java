package cz.cas.lib.proarc.common.process.external.bagit;

import cz.cas.lib.proarc.common.process.external.ExternalProcess;
import java.io.File;
import java.util.List;
import org.apache.commons.configuration2.Configuration;

public class BagitExternalProcess extends ExternalProcess {

    private File exportFolder;

    public BagitExternalProcess(Configuration conf, File exportFolder) {
        super(conf);
        this.exportFolder = exportFolder;
    }

    @Override
    public void run() {
        if (exportFolder == null) {
            throw new IllegalStateException("Neznámá složka exportu: " + exportFolder);
        }
        if (!exportFolder.exists()) {
            throw new IllegalStateException(exportFolder.getAbsolutePath() + " neexistuje.");
        }
        super.run();
    }

    @Override
    protected List<String> buildCmdLine(Configuration conf) {
        List<String> cmdLine = super.buildCmdLine(conf);
        cmdLine.add(exportFolder.getAbsolutePath());
        return cmdLine;
    }
}
