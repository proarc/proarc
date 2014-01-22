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

package cz.cas.lib.proarc.common.export.desa;

import java.util.HashMap;
import java.util.Map;

import com.yourmediashelf.fedora.client.FedoraClient;

import cz.cas.lib.proarc.common.export.desa.structure.DesaElement;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;

/**
 * Context for Desa mets export
 * 
 * If Fedora is used as a source for FoXML documents, then fedoraClient should
 * be set and fsParentMap and path should be empty.
 * 
 * If FoXML documents are stored on a file system, then fedoraClient should be
 * empty and fsParentMap must contain parent mappings and path is an absolute
 * path to the directory with FoXML documents
 * 
 * outputPath is an absolute path where ZIP files are stored ZIP files (minimum
 * 2) - 1 descriptor for files ([PACKAGE_ID]_FILE.ZIP) and then the documents
 * ([PACKAGE_ID]_XXXX.ZIP)
 * 
 * @author Robert Simonovsky
 * 
 */
public class DesaContext {
    private FedoraClient fedoraClient;
    private RemoteStorage remoteStorage;
    private final Map<String, Integer> elementIds = new HashMap<String, Integer>();
    private DesaElement rootElement;
    private Map<String, String> fsParentMap;
    private String path;
    private String outputPath;
    private String desaResultPath;
    private String packageID;
    private final MetsExportException metsExportException = new MetsExportException();

    /**
     * Returns the path with desa export result
     * 
     * @return
     */
    public String getDesaResultPath() {
        return desaResultPath;
    }

    /**
     * Sets the path where the desa export result is stored
     * 
     * @param desaResultPath
     */
    public void setDesaResultPath(String desaResultPath) {
        this.desaResultPath = desaResultPath;
    }

    /**
     * Returns the export exception type
     * 
     * @return
     */
    public MetsExportException getMetsExportException() {
        return metsExportException;
    }

    /**
     * Returns a package ID for desa export
     * 
     * @return
     */
    public String getPackageID() {
        return packageID;
    }

    /**
     * Sets the package ID for the desa export
     * 
     * @param packageID
     */
    public void setPackageID(String packageID) {
        this.packageID = packageID;
    }

    /**
     * Returns the output absolute path where ZIP files are stored ZIP files
     * (minimum 2) - 1 descriptor for files ([PACKAGE_ID]_FILE.ZIP) and then the
     * documents ([PACKAGE_ID]_XXXX.ZIP)
     * 
     * @return
     */
    public String getOutputPath() {
        return outputPath;
    }

    /**
     * Sets the output absolute path where ZIP files are stored ZIP files
     * (minimum 2) - 1 descriptor for files ([PACKAGE_ID]_FILE.ZIP) and then the
     * documents ([PACKAGE_ID]_XXXX.ZIP)
     * 
     * @param outputPath
     */
    public void setOutputPath(String outputPath) {
        this.outputPath = outputPath;
    }

    /**
     * Returns the absolute path for FoXML documents on file system - not used
     * when using Fedora
     * 
     * @return
     */
    public String getPath() {
        return path;
    }

    /**
     * Sets the absolute path for FoXML documents on file system - not used when
     * using Fedora
     * 
     * @param path
     */
    public void setPath(String path) {
        this.path = path;
    }

    /**
     * Returns the parent map for resource index on a file system - not used
     * when using Fedora
     * 
     * @return
     */
    public Map<String, String> getFsParentMap() {
        return fsParentMap;
    }

    /**
     * Sets the parent map for resource index on a file system - not used when
     * using Fedora
     * 
     * @param fsParentMap
     */
    public void setFsParentMap(Map<String, String> fsParentMap) {
        this.fsParentMap = fsParentMap;
    }

    /**
     * Returns the root element of Desa export - used for DesaVisitor
     * 
     * @return
     */
    public DesaElement getRootElement() {
        return rootElement;
    }

    /**
     * Sets the root element of Desa export
     * 
     * @param rootElement
     */
    public void setRootElement(DesaElement rootElement) {
        this.rootElement = rootElement;
    }

    /**
     * Returns the map of Element IDs
     * 
     * @return
     */
    public Map<String, Integer> getElementIds() {
        return elementIds;
    }

    /**
     * Returns the fedora client
     * 
     * @return
     */
    public FedoraClient getFedoraClient() {
        return fedoraClient;
    }

    /**
     * Sets the fedora client - used when FoXML documents are stored in Fedora
     * 
     * @param fedoraClient
     */
    public void setFedoraClient(FedoraClient fedoraClient) {
        this.fedoraClient = fedoraClient;
    }

    /**
     * Returns the remote storage (Fedora)
     * 
     * @return
     */
    public RemoteStorage getRemoteStorage() {
        return remoteStorage;
    }

    /**
     * Sets the remote storage (Fedora)
     * 
     * @param remoteStorage
     */
    public void setRemoteStorage(RemoteStorage remoteStorage) {
        this.remoteStorage = remoteStorage;
    }

    /**
     * Adds a new element ID
     * 
     * @param elementId
     * @return
     */
    public Integer addElementId(String elementId) {
        Integer id = elementIds.get(elementId);
        if (id == null) {
            id = 0;
        } else {
            elementIds.remove(elementId);
        }
        id++;
        elementIds.put(elementId, id);
        return id;
    }

    /**
     * Returns the last ID for given element
     * 
     * @param elementId
     * @return
     */
    public Integer getElementId(String elementId) {
        return elementIds.get(elementId);
    }
}
