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

package cz.cas.lib.proarc.common.process.export.desa.structure;

import java.util.HashMap;
import java.util.List;

import org.w3c.dom.Element;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.process.export.desa.DesaContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;

/**
 * Interface of Desa Element
 *
 * @author Robert Simonovsky
 *
 */
public interface IDesaElement {

    /**
     * Returns the descriptorType of DESA element (DC/NSESS)
     *
     * @return
     */
    public String getDescriptorType();
    /**
     * Returns the IdSIPVersion from the DESA transport
     *
     * @return
     */
    public String getIdSIPVersion();

    /**
     * Sets the IdSIPVersion from the DESA transport
     *
     * @return
     */
    public void setIdSIPVersion(String idSIPVersion);

    /**
     * Returns a parent DesaElement
     *
     * @return
     */
    public DesaElement getParent();

    /**
     * Returns the list of child elements
     *
     * @return
     */
    public List<DesaElement> getChildren();

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
     * @param desaVisitor
     * @throws MetsExportException
     */
    public void accept(IDesaElementVisitor desaVisitor) throws MetsExportException;

    /**
     * Accept method for generator of mets, desaProps contain properties for
     * DESA trasport
     *
     * @param desaVisitor
     * @throws MetsExportException
     */
    public void accept(IDesaElementVisitor desaVisitor, HashMap<String, String> desaProps) throws MetsExportException;

    /**
     * Returns the type an element
     *
     * @return
     */
    public String getElementType();

    /**
     * Retuns the context of desa export
     *
     * @return
     */
    public DesaContext getDesaContext();

    /**
     * Returns the ID of the element
     *
     * @return
     */
    public String getElementID();

    /**
     * Returns the name of a mets-zip file where this document/element is stored
     * the name of the file is without path and suffix (PACKAGE_ID_XXXX)
     *
     * @return
     */
    public String getZipName();

    /**
     * Sets the name of a zip file where this document/element is stored the
     * name of the file is without path and suffix (PACKAGE_ID_XXXX)
     *
     * @param zipName
     */
    public void setZipName(String zipName) throws MetsExportException;
}
