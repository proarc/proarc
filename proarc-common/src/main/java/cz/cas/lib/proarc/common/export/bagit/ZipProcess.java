package cz.cas.lib.proarc.common.export.bagit;

import cz.cas.lib.proarc.common.process.ExternalProcess;
import java.io.File;
import java.util.List;
import org.apache.commons.configuration.Configuration;

public class ZipProcess extends ExternalProcess {

    private File sourceFolder;
    private File destinationFolder;

    public ZipProcess(Configuration conf, File sourceFolder, File destinationFolder) {
        super(conf);
        this.sourceFolder = sourceFolder;
        this.destinationFolder = destinationFolder;
    }

    @Override
    public void run() {
        if (!sourceFolder.exists()) {
            throw new IllegalStateException(sourceFolder.getAbsolutePath() + "does not exists.");
        }
        super.run();
    }

    @Override
    protected List<String> buildCmdLine(Configuration conf) {
        List<String> cmdLine = super.buildCmdLine(conf);
        cmdLine.add(destinationFolder.getAbsolutePath());
        cmdLine.add(sourceFolder.getAbsolutePath());
        return cmdLine;
    }
}
