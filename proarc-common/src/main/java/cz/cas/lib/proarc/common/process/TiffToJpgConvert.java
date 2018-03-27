/*
 * Copyright (C) 2018 Jakub Kremlacek
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
package cz.cas.lib.proarc.common.process;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.imports.ImportProfile;
import java.io.File;
import java.util.HashMap;
import java.util.List;
import org.apache.commons.configuration.Configuration;

/**
 * Converts input tiff image into jpeg image using external GraphicsMagick application
 *
 * @author Jakub Kremlacek
 */
public class TiffToJpgConvert extends ExternalProcess {

    public static final String ID = "gm";
    private final File input;
    private final File output;
    private final Integer maxWidth;
    private final Integer maxHeight;

    private final String IMAGE_MAX_SIZE = "100000";

    public static void main(String[] args) throws Exception {
        if (args != null && args.length > 2) {
            HashMap<String, String> env = new HashMap<>();
            env.put(AppConfiguration.PROPERTY_APP_HOME, args[0]);
            AppConfiguration conf = AppConfigurationFactory.getInstance().create(env);
            Configuration config = conf.getAuthenticators();
            config = new ImportProfile(config).getNdkArchivalProcessor();
            ExternalProcess ep = new GhostConvert(config, new File(args[1]), new File(args[2]));
            ep.run();
        }
    }

    /**
     * Converts supplied tiff image into jpeg without resizing
     *
     * @param conf processor configuration
     * @param input tiff image file
     * @param output to be generated jpeg file (!must not exist prior to running process)
     */
    public TiffToJpgConvert (Configuration conf, File input, File output) {
        this(conf, input, output, null, null);
    }

    /**
     * Converts supplied tiff image into jpeg with size constraints
     *
     * @param conf processor configuration
     * @param input tiff image file
     * @param output to be generated jpeg file (!must not exist prior to running process)
     * @param maxWidth jpeg maximum width
     * @param maxHeight jpeg maximum height
     */
    public TiffToJpgConvert (Configuration conf, File input, File output, Integer maxWidth, Integer maxHeight) {
        super(conf);
        this.input = input;
        this.output = output;

        if (maxHeight != null && maxHeight <= 0) {
            throw new IllegalArgumentException("maxHeight must be positive or null");
        }

        if (maxWidth != null && maxWidth <= 0) {
            throw new IllegalArgumentException("maxWidth must be positive or null");
        }

        this.maxHeight = maxHeight;
        this.maxWidth = maxWidth;
    }

    @Override
    public void run() {
        if (!input.exists()) {
            throw new IllegalStateException(input.getAbsolutePath() + " not exists!");
        }
        if (output.exists()) {
            output.delete();
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

        if (maxHeight != null || maxWidth != null) {
            cmdLine.add("-resize");
            cmdLine.add(
                    maxWidth != null ? maxWidth.toString() : IMAGE_MAX_SIZE +
                            "x" +
                    maxHeight != null ? maxHeight.toString() : IMAGE_MAX_SIZE
            );
        }

        cmdLine.add(inputFile);
        cmdLine.add(outputFile);
        return cmdLine;
    }
}
