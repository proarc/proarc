/*
 * Copyright (C) 2013 Robert Simonovsky
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

package cz.cas.lib.proarc.common.export.mets;

import java.util.ArrayList;
import java.util.List;

/**
 * Class for handling exceptions inside Mets export
 * 
 * @author eskymo
 * 
 */
public class MetsExportException extends Exception {

    private static final long serialVersionUID = 1L;

    /**
     * Class containing information about an exception
     * 
     * @author eskymo
     * 
     */
    public class MetsExportExceptionElement {
        String pid;
        String message;
        boolean warning;
        Exception ex;

        public MetsExportExceptionElement(String pid, String message, boolean warning, Exception ex) {
            super();
            this.pid = pid;
            this.message = message;
            this.warning = warning;
            this.ex = ex;

        }

        public String getPid() {
            return pid;
        }

        public String getMessage() {
            return message;
        }

        public boolean isWarning() {
            return warning;
        }

        public Exception getEx() {
            return ex;
        }
    }

    public List<MetsExportExceptionElement> exceptionList = new ArrayList<MetsExportException.MetsExportExceptionElement>();

    public void addException(String pid, String message, boolean warning, Exception ex) {
        exceptionList.add(new MetsExportExceptionElement(pid, message, warning, ex));
    }

    public void addException(String message, boolean warning, Exception ex) {
        exceptionList.add(new MetsExportExceptionElement(null, message, warning, ex));
    }

    public void addException(String message, boolean warning) {
        exceptionList.add(new MetsExportExceptionElement(null, message, warning, null));
    }

    public void addException(String message) {
        exceptionList.add(new MetsExportExceptionElement(null, message, false, null));
    }

    public MetsExportException(String pid, String message, boolean warning, Exception ex) {
        this.addException(pid, message, warning, ex);
    }

    public MetsExportException(String message, boolean warning, Exception ex) {
        this.addException(message, warning, ex);
    }

    public MetsExportException(String message, boolean warning) {
        this.addException(message, warning);
    }

    public MetsExportException(String message) {
        this.addException(message, false);
    }

    public MetsExportException() {
    }

    /**
     * @return the list of export exceptions
     */
    public List<MetsExportExceptionElement> getExceptions() {
        return exceptionList;
    }
}