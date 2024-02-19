/*
 * Copyright (C) 2014 Robert Simonovsky
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

package cz.cas.lib.proarc.common.process.export.mets;

import edu.harvard.hul.ois.xml.ns.jhove.Property;

import cz.cas.lib.proarc.aes57.Aes57Utils;
import cz.cas.lib.proarc.codingHistory.CodingHistoryUtils;
import cz.cas.lib.proarc.common.storage.akubra.AkubraUtils;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import java.util.Date;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.dom.DOMResult;
import org.aes.audioobject.AudioObject;
import org.w3c.dom.Node;

public class JHoveOutput {
    String formatVersion;
    boolean skip;
    XMLGregorianCalendar rawCreated;

    public boolean isSkip() {
        return skip;
    }

    public void setSkip() {
        this.skip = true;
    }

    public String getFormatVersion() {
        return formatVersion;
    }

    public void setFormatVersion(String formatVersion) {
        this.formatVersion = formatVersion;
    }

    public Node getMixNode() {
        if (this.mix != null) {
            DOMResult result = new DOMResult();
            MixUtils.marshal(result, mix, true);
            return result.getNode().getFirstChild();
        }
        return null;
    }

    public void setMixNode(Node mixNode) {
        this.mixNode = mixNode;
    }

    Node mixNode;
    Mix mix;

    public Mix getMix() {
        return mix;
    }

    public void setMix(Mix mix) {
        this.mix = mix;
    }

    Node aesNode;
    AudioObject aes;

    public Node getAesNode() {
        if (this.aes != null) {
            DOMResult result = new DOMResult();
            Aes57Utils.marshal(result, aes, true);
            return result.getNode().getFirstChild();
        }
        return null;
    }

    public void setAesNode(Node aesNode) {
        this.aesNode = aesNode;
    }

    public AudioObject getAes() {
        return  aes;
    }

    public void setAes(AudioObject aes) {
        this.aes = aes;
    }

    Node codingHistoryNode;
    Property codingHistory;

    public Node getCodingHistoryNode() {
        if (this.codingHistory != null) {
            DOMResult result = new DOMResult();
            CodingHistoryUtils.marshal(result, codingHistory, true);
            return result.getNode().getFirstChild();
        }
        return null;
    }

    public void setCodingHistoryNode(Node codingHistoryNode) {
        this.codingHistoryNode = codingHistoryNode;
    }

    public Property getCodingHistory() {
        return codingHistory;
    }

    public void setCodingHistory(Property codingHistory) {
        this.codingHistory = codingHistory;
    }

    Node basicObjectNode;
    Property basicObjectInfo;

    public Node getBasicObjectNode() {
        return basicObjectNode;
    }

    public void setBasicObjectNode(Node basicObjectNode) {
        this.basicObjectNode = basicObjectNode;
    }

    public void setBasicObjectInfo(Property basicObjectInfo) {
        this.basicObjectInfo = basicObjectInfo;
    }

    public Property getBasicObjectInfo() {
        return basicObjectInfo;
    }

    public void setCreatedDate(long lastModified) throws DatatypeConfigurationException {
        this.rawCreated = AkubraUtils.toXmlGregorian(new Date(lastModified));
    }

    public XMLGregorianCalendar getRawCreated() {
        return rawCreated;
    }
}
