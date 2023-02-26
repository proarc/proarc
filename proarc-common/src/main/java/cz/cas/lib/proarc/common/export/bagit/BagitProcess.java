package cz.cas.lib.proarc.common.export.bagit;

import cz.cas.lib.proarc.common.process.ExternalProcess;
import java.io.File;
import java.util.List;
import org.apache.commons.configuration.Configuration;

public class BagitProcess extends ExternalProcess {

    private File exportFolder;

    public BagitProcess(Configuration conf, File exportFolder) {
        super(conf);
        this.exportFolder = exportFolder;
    }

    @Override
    public void run() {
        if (exportFolder == null) {
            throw new IllegalStateException("Unknown export folder :" + exportFolder);
        }
        if (!exportFolder.exists()) {
            throw new IllegalStateException(exportFolder.getAbsolutePath() + "does not exists.");
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
