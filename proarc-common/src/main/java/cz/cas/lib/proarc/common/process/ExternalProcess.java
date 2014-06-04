/*
 * Copyright (C) 2014 Jan Pokorsky
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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.io.IOUtils;

/**
 * The helper to run external processes. It builds command line and environment
 * from {@link Configuration} properties.
 *
 * @author Jan Pokorsky
 */
public class ExternalProcess implements Runnable {

    private static final Logger LOG = Logger.getLogger(ExternalProcess.class.getName());
    /**
     * The process environment.
     */
    public static final String PROP_ENVIRONMENT = "environment";
    /**
     * The full path to executable file.
     */
    public static final String PROP_EXEC = "exec";
    /**
     * The command line argument.
     */
    public static final String PROP_ARG = "arg";
    /**
     * The processor type.
     */
    public static final String PROP_TYPE = "type";

    private String out;
    private String err;
    private int exitCode;
    private final Configuration conf;

    protected ExternalProcess(Configuration conf) {
        this.conf = conf;
    }

    @Override
    public void run() {
        Map<String, String> env = buildEnv(conf);
        List<String> cmdLine = buildCmdLine(conf);
        try {
            runCmdLine(cmdLine, env);
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        } catch (InterruptedException ex) {
            throw new IllegalStateException(ex);
        }
    }

    protected Map<String, String> buildEnv(Configuration conf) {
        Configuration envConfig = conf.subset(PROP_ENVIRONMENT);
        Map<String, String> env = new HashMap<String, String>();
        for (Iterator<String> it = envConfig.getKeys(); it.hasNext();) {
            String envKey = it.next();
            env.put(envKey, envConfig.getString(envKey));
        }
        return env;
    }

    protected List<String> buildCmdLine(Configuration conf) {
        String exec = conf.getString(PROP_EXEC);
        if (exec == null) {
            throw new IllegalStateException("Missing 'exec'!");
        }
        String[] args = conf.getStringArray(PROP_ARG);
        List<String> cmdLine = new ArrayList<String>();
        cmdLine.add(exec);
        cmdLine.addAll(Arrays.asList(args));
        return cmdLine;
    }

    private int runCmdLine(List<String> cmdLine, Map<String, String> env) throws IOException, InterruptedException {
        StringBuilder debug = new StringBuilder();
        for (String arg : cmdLine) {
            debug.append(arg).append(" ");
        }
        LOG.fine("run: " + debug);
        ProcessBuilder pb = new ProcessBuilder(cmdLine);
        // for now redirect outputs into a single stream to eliminate
        // the need to run multiple threads to read each output
        pb.redirectErrorStream(true);
        pb.environment().putAll(env);
        Process process = pb.start();
        InputStream outStream = new BufferedInputStream(process.getInputStream(), 1024*1024);
        try {
            out = IOUtils.toString(outStream);
        } catch (IOException ex) {
            IOUtils.closeQuietly(outStream);
            throw ex;
        }
        exitCode = process.exitValue();
        LOG.fine(getFullOutput());
        return exitCode;
    }

    public String getOut() {
        return out;
    }

    public String getErr() {
        return err;
    }

    public int getExitCode() {
        return exitCode;
    }

    public boolean isOk() {
        return exitCode == 0;
    }

    public String getFullOutput() {
        return String.format("exit: %s\nout: %s\nerr: %s", exitCode, out, err);
    }

}
