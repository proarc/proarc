/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.incad.pas.editor.server.imports;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.incad.pas.editor.server.xml.ProarcXmlUtils;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Holds info about folder tracked as a batch import.
 *
 * @author Jan Pokorsky
 */
@XmlRootElement(name = "folder", namespace = ProarcXmlUtils.NS_IMPORT)
@XmlAccessorType(XmlAccessType.FIELD)
public class ImportFolderStatus {

    @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
    private Integer batchId;

    public ImportFolderStatus() {
    }

    public ImportFolderStatus(Batch b) {
        batchId = b.getId();
    }

    public Integer getBatchId() {
        return batchId;
    }

    public void setBatchId(Integer batchId) {
        this.batchId = batchId;
    }

}
