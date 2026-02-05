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
package cz.cas.lib.proarc.common.process.external;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.logging.Logger;
import org.apache.commons.io.IOUtils;

/**
 * Reads the external process output in separate thread.
 *
 * @author Jan Pokorsky
 */
public class OutputConsumer extends Thread {

    private static final Logger LOG = Logger.getLogger(OutputConsumer.class.getName());
    private final InputStream input;
    private final StringBuilder output;
    private Throwable error;

    public OutputConsumer(InputStream input) {
        this.input = input;
        output = new StringBuilder();
    }

    @Override
    public void run() {
        BufferedReader reader = new BufferedReader(new InputStreamReader(input));
        try {
            for (String line; (line = reader.readLine()) != null; ) {
                if (output.length() > 0) {
                    output.append('\n');
                }
                output.append(line);
            }
        } catch (Throwable ex) {
            error = ex;
        } finally {
            LOG.fine("Close.");
            IOUtils.closeQuietly(reader);
        }
    }

    public String getOutput() {
        return output.toString();
    }

    public Throwable getError() {
        return error;
    }

}
