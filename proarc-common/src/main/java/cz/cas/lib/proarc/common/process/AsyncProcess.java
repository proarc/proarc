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

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.io.IOUtils;

/**
 * Runs an external process in own thread to handle possible process freeze.
 *
 * @author Jan Pokorsky
 */
public class AsyncProcess extends Thread {

    private static final Logger LOG = Logger.getLogger(AsyncProcess.class.getName());
    private final List<String> cmdLine;
    private final Map<String, String> env;
    private AtomicReference<Process> refProcess = new AtomicReference<Process>();
    private AtomicBoolean done = new AtomicBoolean();
    private int exitCode;
    private OutputConsumer outputConsumer;

    public AsyncProcess(List<String> cmdLine, Map<String, String> env) {
        this.cmdLine = cmdLine;
        this.env = env;
    }

    @Override
    public void run() {
        done.set(false);
        outputConsumer = null;
        exitCode = -1;
        ProcessBuilder pb = new ProcessBuilder(cmdLine);
        // for now redirect outputs into a single stream to eliminate
        // the need to run multiple threads to read each output
        pb.redirectErrorStream(true);
        pb.environment().putAll(env);
        try {
            Process process = pb.start();
            refProcess.set(process);
            outputConsumer = new OutputConsumer(process.getInputStream());
            outputConsumer.start();
            exitCode = process.waitFor();
            LOG.fine("Done " + cmdLine);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, cmdLine.toString(), ex);
        } finally {
            done.set(true);
        }
    }

    public boolean isDone() {
        return done.get();
    }

    public int getExitCode() {
        return exitCode;
    }

    public String getOut() {
        return outputConsumer != null ? outputConsumer.getOutput() : "";
    }

    public void kill() {
        Level level = isDone() ? Level.FINE : Level.WARNING;
        LOG.log(level, "Kill isDone: " + isDone() + ", " + cmdLine);
        Process process = refProcess.getAndSet(null);
        if (process != null) {
            process.destroy();
            IOUtils.closeQuietly(process.getInputStream());
            IOUtils.closeQuietly(process.getErrorStream());
            IOUtils.closeQuietly(process.getOutputStream());
            done.set(true);
             try {
                outputConsumer.join();
            } catch (InterruptedException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
    }

}
