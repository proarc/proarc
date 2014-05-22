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

import javax.xml.datatype.XMLGregorianCalendar;

/**
 * @author Robert Simonovsky
 *
 *         Class is used to store md5 info and file size
 *
 */
public class FileMD5Info {
    private String fileName;
    private String mimeType;
    private XMLGregorianCalendar created;
    private String formatVersion;

    /**
     * Return the formatVersion
     *
     * @return
     */
    public String getFormatVersion() {
        return formatVersion;
    }

    /**
     * Sets the formatVersion
     *
     * @param formatVersion
     */
    public void setFormatVersion(String formatVersion) {
        this.formatVersion = formatVersion;
    }

    /**
     * Returns the date of creation
     *
     * @return
     */
    public XMLGregorianCalendar getCreated() {
        return created;
    }

    /**
     * Sets the date of creation
     *
     * @param created
     */
    public void setCreated(XMLGregorianCalendar created) {
        this.created = created;
    }

    public String getMimeType() {
        return mimeType;
    }

    public void setMimeType(String mimeType) {
        this.mimeType = mimeType;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    private String md5;
    private int size;

    public String getMd5() {
        return md5;
    }

    public void setMd5(String md5) {
        this.md5 = md5;
    }

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public FileMD5Info(String fileName, String md5, int size) {
        super();
        this.fileName = fileName;
        this.md5 = md5;
        this.size = size;
    }

    public FileMD5Info() {
        super();
    }

    public FileMD5Info(String fileName) {
        super();
        this.fileName = fileName;
    }

    public FileMD5Info(String md5, int size) {
        super();
        this.md5 = md5;
        this.size = size;
    }
}
