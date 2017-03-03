package cz.cas.lib.proarc.common.process;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.imports.ImportProfile;
import org.apache.commons.configuration.Configuration;

import java.io.File;
import java.util.HashMap;
import java.util.List;

/**
 * Uses GhostMagick for converting small jpg images to tiff
 *
 * @author Jakub Kremlacek
 */
public class GhostConvert extends ExternalProcess {
    public static final String ID = "gm";
    private final File input;
    private final File output;

    public static void main(String[] args) throws Exception {
        if (args != null && args.length > 2) {
            HashMap<String, String> env = new HashMap<String, String>();
            env.put(AppConfiguration.PROPERTY_APP_HOME, args[0]);
            AppConfiguration conf = AppConfigurationFactory.getInstance().create(env);
            Configuration config = conf.getAuthenticators();
            config = new ImportProfile(config).getNdkArchivalProcessor();
            ExternalProcess ep = new GhostConvert(config, new File(args[1]), new File(args[2]));
            ep.run();
        }
    }

    public GhostConvert (Configuration conf, File input, File output) {
        super(conf);
        this.input = input;
        this.output = output;
    }

    @Override
    public void run() {
        if (!input.exists()) {
            throw new IllegalStateException(input.getAbsolutePath() + " not exists!");
        }
        if (output.exists()) {
            throw new IllegalStateException(output.getAbsolutePath() + " exists!");
        }
        super.run();
    }

    @Override
    public boolean isOk() {
        return super.isOk() && output.exists() && output.length() > 0;
    }

    @Override
    protected List<String> buildCmdLine(Configuration conf) {
        String inputFile = input.getAbsolutePath();
        String outputFile = output.getAbsolutePath();
        List<String> cmdLine = super.buildCmdLine(conf);
        //cmdLine.add("-i");
        cmdLine.add(inputFile);
        //cmdLine.add("-o");
        cmdLine.add(outputFile);
        return cmdLine;
    }
}
