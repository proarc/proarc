package cz.cas.lib.proarc.common.process;

import java.io.File;
import java.util.List;
import org.apache.commons.configuration.Configuration;

/**
 * Processes K4 and Image export for Kramerius using Imageserver.
 *
 * @author Jakub Kremlacek
 */
public class ExportKWISPostProcess extends ExternalProcess {

    private String imagesPath;
    private String k4Path;
    private String exportPath;

    public ExportKWISPostProcess(Configuration conf, String imagesPath, String k4Path, String exportPath) {
        super(conf);

        this.imagesPath = imagesPath;
        this.k4Path = k4Path;
        this.exportPath = exportPath;
    }

    @Override
    public void run() {
        if (!new File(imagesPath).exists()) {
            throw new IllegalStateException(imagesPath.toString() + " does not exist.");
        }

        if (!new File(k4Path).exists()) {
            throw new IllegalStateException(k4Path.toString() + " does not exist.");
        }

        if (!new File(exportPath).exists()) {
            throw new IllegalStateException(exportPath.toString() + " does not exist.");
        }

        super.run();
    }

    @Override
    public boolean isOk() {
        return new File(exportPath).listFiles().length > 0;
    }

    @Override
    protected List<String> buildCmdLine(Configuration conf) {
        String imageInput = new File(imagesPath).getAbsolutePath();
        String k4Input = new File(k4Path).getAbsolutePath();
        String output = new File(exportPath).getAbsolutePath();

        List<String> cmdLine = super.buildCmdLine(conf);
        cmdLine.add("-iI");
        cmdLine.add(imageInput);
        cmdLine.add("-iK");
        cmdLine.add(k4Input);
        cmdLine.add("-o");
        cmdLine.add(output);

        return cmdLine;
    }
}