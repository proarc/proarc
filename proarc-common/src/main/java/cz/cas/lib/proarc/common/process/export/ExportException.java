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
package cz.cas.lib.proarc.common.process.export;

import cz.cas.lib.proarc.common.object.DigitalObjectElement;

/**
 *
 * @author Jan Pokorsky
 */
public final class ExportException extends Exception {

    private DigitalObjectElement element;
    private String details;

    public ExportException() {
    }

    public ExportException(String msg) {
        super(msg);
    }

    public ExportException(String message, Throwable cause) {
        super(message, cause);
    }

    public ExportException(Throwable cause) {
        super(cause);
    }

    public ExportException(DigitalObjectElement elm, String message, String details, Throwable cause) {
        super(message, cause);
        this.element = elm;
        this.details = details;
    }

    public DigitalObjectElement getElement() {
        return element;
    }

    public void setElement(DigitalObjectElement element) {
        this.element = element;
    }

    public String getDetails() {
        return details;
    }

    public void setDetails(String details) {
        this.details = details;
    }

}
