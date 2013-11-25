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

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.datatype.XMLGregorianCalendar;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp;
import cz.cas.lib.proarc.mets.MetsType.MetsHdr;
import edu.harvard.hul.ois.jhove.App;
import edu.harvard.hul.ois.jhove.JhoveBase;

/**
 * Java class representing the generic Mets element
 * 
 * @author Robert Simonovsky
 * 
 */
public class MetsInfo {
    public MetsExportException metsExportException = new MetsExportException();
    private XMLGregorianCalendar createDate;
    private XMLGregorianCalendar lastModDate;
    private String type;
    private String label;
    protected Mets mets = new Mets();
    private final String packageId;
    HashMap<String, FileGrp> fileGrpMap;
    public RemoteStorage remoteStorage;
    private String path;
    private String outputPath;
    public JhoveBase jhoveBase;
    public App jhoveApp;
    DivType physDivType;
    public MetsElement rootElement;
    private int seq = 0;
    private final List<FileMD5Info> fileList = new ArrayList<FileMD5Info>();
    public HashMap<String, MetsElement> pidElements = new HashMap<String, MetsElement>();
    private final HashMap<String, Integer> modOrderMap = new HashMap<String, Integer>();
    public FedoraClient fedoraClient;
    public Map<String, String> fileSystemParents = null;

    /**
     * Registers file
     * 
     * @param fileName
     */
    public void addFile(FileMD5Info fileMD5info) {
        this.fileList.add(fileMD5info);
    }

    /**
     * Returns a new number of mod (sequence)
     * 
     * @param type
     * @return
     */
    public Integer getModOrder(String type) {
        Integer result = 1;
        if (modOrderMap.get(type) != null) {
            result = modOrderMap.get(type) + 1;
            modOrderMap.remove(type);
        }
        modOrderMap.put(type, result);
        return result;
    }

    /**
     * Returns list of files registered in mets document
     * 
     * @return
     */
    public List<FileMD5Info> getFileList() {
        return fileList;
    }

    /**
     * Returns a new number in sequence
     * 
     * @return
     */
    public int getSeq() {
        seq++;
        return seq;
    }

    /**
     * Saves a mets into a file
     */
    public void save() throws MetsExportException {
        MetsUtils.saveMets(this.outputPath, this);
    }

    /**
     * Returns an instance of Mets document (jaxb)
     * 
     * @return
     */
    public Mets getMets() {
        return mets;
    }

    /**
     * Returns a packageID
     * 
     * @return
     */
    public String getPackageId() {
        return packageId;
    }

    /**
     * Getter for createDate attribute
     * 
     * @return
     */
    public XMLGregorianCalendar getCreateDate() {
        return createDate;
    }

    /**
     * Setter for createDate attribute
     * 
     * @param createDate
     */
    public void setCreateDate(XMLGregorianCalendar createDate) {
        this.createDate = createDate;
    }

    /**
     * Getter for lastModDate attribute
     * 
     * @return
     */
    public XMLGregorianCalendar getLastModDate() {
        return lastModDate;
    }

    /**
     * Setter for lastModDate attribute
     * 
     * @param lastModDate
     */
    public void setLastModDate(XMLGregorianCalendar lastModDate) {
        this.lastModDate = lastModDate;
    }

    /**
     * Returns a type of mets document
     * 
     * @return
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the type of mets document
     * 
     * @param type
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Getter for label attribute
     * 
     * @return
     */
    public String getLabel() {
        return label;
    }

    /**
     * Setter for label attribute
     * 
     * @param label
     */
    public void setLabel(String label) {
        this.label = label;
    }

    /**
     * Constructor for FileSystem
     * 
     * @param object
     * @param path
     * @param packageId
     */
    public MetsInfo(DigitalObject object, String path, String packageId, Map<String, String> fileSystemParents) {
        this.packageId = packageId;
        this.path = path;
        this.fileSystemParents = fileSystemParents;
        fileGrpMap = MetsUtils.initFileGroups(mets);
    }

    /**
     * Constructor for Fedora
     * 
     * @param object
     * @param path
     * @param packageId
     */
    public MetsInfo(FedoraClient fedoraClient, String packageId) {
        this.packageId = packageId;
        this.fedoraClient = fedoraClient;
        this.remoteStorage = new RemoteStorage(fedoraClient);
        fileGrpMap = MetsUtils.initFileGroups(mets);
    }

    /**
     * Inits the Mets header info
     */
    protected void initHeader() {
        mets.setLabel1(getLabel());
        MetsHdr metsHdr = new MetsHdr();
        metsHdr.setCREATEDATE(getCreateDate());
        metsHdr.setLASTMODDATE(getLastModDate());
        mets.setMetsHdr(metsHdr);
        physDivType = MetsUtils.createStructureDiv(mets, Const.DIV_PHYSICAL_LABEL, Const.DIV_PHYSICAL_ID);
    }

    /**
     * 
     * Recursive method for deleting the folder
     * 
     * @param folder
     */
    public static void deleteFolder(File folder) {
        File[] files = folder.listFiles();
        if (files != null) {
            for (File f : files) {
                if (f.isDirectory()) {
                    deleteFolder(f);
                } else {
                    f.delete();
                }
            }
        }
        folder.delete();
    }

    /**
     * creates directory structure for mets elements
     */
    private void createDirectoryStructure() {
        for (String directory : Page.streamMappingFile.values()) {
            File file = new File(this.outputPath + "/" + directory);
            if (file.exists()) {
                deleteFolder(file);
            }
            file.mkdir();
        }
    }

    /**
     * Inserts basic info into the mets document
     * 
     * @param outputPath
     * @param withChildren
     */
    public void insertIntoMets(String outputPath, boolean withChildren) throws MetsExportException {
        this.outputPath = outputPath;
        createDirectoryStructure();
        initHeader();
    }

    /**
     * Getter for source path attribute
     * 
     * @return
     */
    public String getPath() {
        return path;
    }

    /**
     * Setter for source path attribute
     * 
     * @param path
     */
    public void setPath(String path) {
        this.path = path;
    }

    /**
     * Getter for outputPath attribute
     * 
     * @return
     */
    public String getOutputPath() {
        return outputPath;
    }

    /**
     * Setter for outputPath attribute
     * 
     * @param outputPath
     */
    public void setOutputPath(String outputPath) {
        this.outputPath = outputPath;
    }
}