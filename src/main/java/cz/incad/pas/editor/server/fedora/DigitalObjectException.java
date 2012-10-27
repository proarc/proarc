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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.fedora;

/**
 *
 * @author Jan Pokorsky
 */
public class DigitalObjectException extends Exception {
    
    private static final long serialVersionUID = 1L;

    private final String pid;

    public DigitalObjectException(String pid) {
        this(pid, (Throwable) null);
    }

    public DigitalObjectException(String pid, String message) {
        this(pid, message, null);
    }
    
    public DigitalObjectException(String pid, Throwable cause) {
        this(pid, pid, cause);
    }

    public DigitalObjectException(String pid, String message, Throwable cause) {
        super(String.format("PID: %s %s", pid, message), cause);
        this.pid = pid;
    }

    public String getPid() {
        return pid;
    }

}
