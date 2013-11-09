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
package cz.cas.lib.proarc.common.fedora;

/**
 *
 * @author Jan Pokorsky
 */
public class DigitalObjectConcurrentModificationException extends DigitalObjectException {

    private static final long serialVersionUID = 1L;

    public DigitalObjectConcurrentModificationException(String pid) {
        super(pid);
    }

    public DigitalObjectConcurrentModificationException(String pid, String message) {
        super(pid, message);
    }

    public DigitalObjectConcurrentModificationException(String pid, Throwable cause) {
        super(pid, cause);
    }

    public DigitalObjectConcurrentModificationException(String pid, String message, Throwable cause) {
        super(pid, message, cause);
    }

}
