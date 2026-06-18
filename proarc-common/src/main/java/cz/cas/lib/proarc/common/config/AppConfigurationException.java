/*
 * Copyright (C) 2012 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.config;

/**
 *
 * @author Jan Pokorsky
 */
public class AppConfigurationException extends Exception {

    /**
     * Creates a new instance of
     * <code>AppConfigurationException</code> without detail message.
     */
    public AppConfigurationException() {
    }

    /**
     * Constructs an instance of
     * <code>AppConfigurationException</code> with the specified detail message.
     *
     * @param msg the detail message.
     */
    public AppConfigurationException(String msg) {
        super(msg);
    }

    public AppConfigurationException(String message, Throwable cause) {
        super(message, cause);
    }

    public AppConfigurationException(Throwable cause) {
        super(cause);
    }
}
