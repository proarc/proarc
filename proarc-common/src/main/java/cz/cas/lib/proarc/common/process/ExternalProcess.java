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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConversionException;

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
     * The number of attempts to tun a process again in case of failure.
     */
    public static final String PROP_RETRY = "retry";
    /**
     * The time to wait for a running process.
     */
    public static final String PROP_TIMEOUT = "timeout";
    /**
     * The processor type.
     */
    public static final String PROP_TYPE = "type";

    public static final long DEFAULT_TIMEOUT = 2 * 60 * 1000;
    public static final int DEFAULT_RETRY_ATTEMPTS = 0;

    private final Configuration conf;
    private AsyncProcess asyncProcess;
    public String style;

    protected ExternalProcess(Configuration conf) {
        this.conf = conf;
    }

    @Override
    public void run() {
        Map<String, String> env = buildEnv(conf);
        List<String> cmdLine = buildCmdLine(conf);
        try {
            int retry = getRetryCount() + 1;
            for (int i = 0; i < retry; i++) {
                runCmdLine(cmdLine, env);
                if (isOk()) {
                    return ;
                }
                LOG.log(Level.WARNING, "{0}. failure, \n{1}, \nCmd: {2}",
                        new Object[]{i + 1, getFullOutput(), cmdLine});
            }
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
        asyncProcess = new AsyncProcess(cmdLine, env);
        asyncProcess.start();
        long timeout = getTimeout();
        asyncProcess.join(timeout);
        asyncProcess.kill();
        LOG.fine(getFullOutput());
        return asyncProcess.getExitCode();
    }

    public String getOut() {
        return asyncProcess == null ? null: asyncProcess.getOut();
    }

    public String getErr() {
        return null;
    }

    public int getExitCode() {
        return asyncProcess == null ? -1: asyncProcess.getExitCode();
    }

    public boolean isOk() {
        return getExitCode() == 0;
    }

    public String getFullOutput() {
        return String.format("exit: %s,\nout: %s", getExitCode(), getOut());
    }

    int getRetryCount() {
        try {
            int retry = conf.getInt("retry", DEFAULT_RETRY_ATTEMPTS);
            retry = Math.max(0, retry);
            retry = Math.min(100, retry);
            return retry;
        } catch (ConversionException ex) {
            LOG.log(Level.WARNING, null, ex);
            return DEFAULT_RETRY_ATTEMPTS;
        }
    }

    long getTimeout() {
        try {
            long timeout = conf.getLong(PROP_TIMEOUT, DEFAULT_TIMEOUT);
            timeout = Math.max(0, timeout);
            return timeout;
        } catch (ConversionException ex) {
            LOG.log(Level.WARNING, null, ex);
            return DEFAULT_TIMEOUT;
        }
    }

}
