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

package cz.cas.lib.proarc.common.export.mets.structure;

import java.util.List;

import org.apache.log4j.Logger;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.Utils;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.StructMapType;

/**
 * Java class representing a Mets document
 * 
 * @author Robert Simonovsky
 * 
 */
public class MetsEntity extends MetsInfo {
    @SuppressWarnings("unused")
    private static Logger logger = Logger.getLogger(MetsEntity.class);

    /**
     * Initializes the type of mets document
     */
    private void initType(DigitalObject object) {
        rootElement = MetsElement.getElement(object, null, this, true);
        if (Const.PERIODICAL_TITLE.equals(rootElement.type)) {
            this.setType(Const.PERIODICAL);
        } else {
            this.setType(Const.MONOGRAPH);
        }
    }

    /**
     * Constructor
     * 
     * @param object
     * @param path
     * @param packageId
     */
    public MetsEntity(DigitalObject object, String path, String packageId) {
        super(object, path, packageId);
        initType(object);
    }

    /**
     * Constructor
     * 
     * @param remoteStorage
     * @param packageId
     */
    public MetsEntity(DigitalObject object, FedoraClient fedoraClient, String packageId) {
        super(fedoraClient, packageId);
        initType(object);
    }

    /**
     * Inits the logical structure of Mets
     */
    private void insertLogicalStructure() {
        DivType rootDiv = null;
        if ((Const.MONOGRAPH.equals(this.getType())) && (!Utils.isMultiUnitMonograph(rootElement))) {
            DivType logicalDivType = Utils.createStructureDiv(mets, Const.DIV_LOGICAL_LABEL, Const.DIV_LOGICAL_ID);
            logicalDivType.setID(this.getType().toUpperCase() + "_0001");
            rootDiv = rootElement.insertIntoDiv(logicalDivType);
        } else {
            StructMapType structType = new StructMapType();
            mets.getStructMap().add(structType);
            structType.setLabel2(Const.DIV_LOGICAL_LABEL);
            structType.setTYPE(Const.DIV_LOGICAL_ID);
            rootDiv = rootElement.insertIntoDiv(null);
            structType.setDiv(rootDiv);
        }
        addRecursiveElements(rootDiv, rootElement.children);
    }

    /**
     * Inserts a recursive element into the logical div
     * 
     * @param parentDiv
     * @param children
     */
    private void addRecursiveElements(DivType parentDiv, List<MetsElement> children) {
        for (MetsElement element : children) {
            if (element instanceof Page) {
                continue;
            }
            DivType elementDivType = element.insertIntoDiv(parentDiv);
            if (element.children.size() > 0) {
                addRecursiveElements(elementDivType, element.children);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.kramerius.importFoXML.structure.MetsInfo#insertIntoMets(java.lang
     * .String, boolean)
     */
    @Override
    public void insertIntoMets(String outputPath, boolean withChildren) {
        super.insertIntoMets(outputPath, withChildren);
        rootElement.insertIntoMets(mets, withChildren, outputPath);
        insertLogicalStructure();
    }

}