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

package cz.cas.lib.proarc.common.export.mets.structure;

import java.util.List;
import java.util.Map;

import javax.xml.datatype.XMLGregorianCalendar;

import org.w3c.dom.Element;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.MdSecType;

/**
 * Interface of Mets Element
 *
 * @author Robert Simonovsky
 *
 */
public interface IMetsElement {

    /**
     * Sets the modsElementID
     *
     * @param modsElementID
     */
    public void setModsElementID(String modsElementID);

    /**
     * Returns the altoFile
     *
     * @return
     */
    public FileType getAltoFile();

    /**
     * Returns the setsAltoFile
     *
     * @return
     */
    public void setAltoFile(FileType altoFile);

    /**
     *
     * Collects all identifiers for mods element
     *
     * @return
     */
    public Map<String, String> getModsIdentifiers() throws MetsExportException;;

    /**
     * Sets the mdSecType of element in the target mets
     *
     * @param modsMetsElement
     */
    public void setModsMetsElement(MdSecType modsMetsElement);

    /**
     * Returns the mdSecType of element in the target mets
     *
     * @param modsMetsElement
     */
    public MdSecType getModsMetsElement();

    /**
     * Returns the label of original object
     *
     * @return
     */
    public String getLabel();

    /**
     * Returns the createDate attribute of original object
     *
     * @return
     */
    public XMLGregorianCalendar getCreateDate();

    /**
     * Returns the lastUpdateDate attribute of original object
     *
     * @return
     */
    public XMLGregorianCalendar getLastUpdateDate();

    /**
     * Returns the mods stream of mets element
     *
     * @return
     */
    public List<Element> getModsStream();

    /**
     * Returns a parent MetsElement
     *
     * @return
     */
    public MetsElement getParent();

    /**
     * Returns the list of child elements
     *
     * @return
     */
    public List<MetsElement> getChildren();

    /**
     * Creates and assigns the child elements
     *
     * @throws MetsExportException
     */
    public void fillChildren() throws MetsExportException;

    /**
     * Returns a descriptor data stream (DC/NSSESS/...)
     *
     * @return
     */
    public List<Element> getDescriptor();

    /**
     * Returns a model of the element
     *
     * @return
     */
    public String getModel();

    /**
     * Returns an original PID of the element
     *
     * @return
     */
    public String getOriginalPid();

    /**
     * Returns a rel-ext datastream
     *
     * @return
     */
    public List<Element> getRelsExt();

    /**
     * Returns the DigitalObject representing current Element
     *
     * @return
     */
    public DigitalObject getSourceObject();

    /**
     * Accept method for generator of mets
     *
     * @param metsVisitor
     * @throws MetsExportException
     */
    public void accept(IMetsElementVisitor metsVisitor) throws MetsExportException;

    /**
     * Returns the type an element
     *
     * @return
     */
    public String getElementType();

    /**
     * Retuns the context of mets export
     *
     * @return
     */
    public MetsContext getMetsContext();

    /**
     * Returns the ID of the element
     *
     * @return
     */
    public String getElementID();

    /**
     * Returns the ID for mods of the element
     *
     * @return
     */
    public String getModsElementID();
}
