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

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Context for Mets mets export
 * <p>
 * If Fedora is used as a source for FoXML documents, then fedoraClient should
 * be set and fsParentMap and path should be empty.
 * <p>
 * If FoXML documents are stored on a file system, then fedoraClient should be
 * empty and fsParentMap must contain parent mappings and path is an absolute
 * path to the directory with FoXML documents
 * <p>
 * outputPath is an absolute path where PSP packages are stored
 *
 * @author Robert Simonovsky
 *
 */
public class MetsContext {

    private static final Set<String> PERIODICAL_MODELS = Set.of(NdkPlugin.MODEL_PERIODICAL,NdkPlugin.MODEL_PERIODICALVOLUME,NdkPlugin.MODEL_PERIODICALISSUE,NdkPlugin.MODEL_PERIODICALSUPPLEMENT,NdkPlugin.MODEL_ARTICLE);
    private static final Set<String> EPERIODICAL_MODELS = Set.of(NdkEbornPlugin.MODEL_EPERIODICAL, NdkEbornPlugin.MODEL_EPERIODICALVOLUME, NdkEbornPlugin.MODEL_EPERIODICALISSUE, NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT, NdkEbornPlugin.MODEL_EARTICLE);
    private static final Set<String> MONOGRAPH_MODELS = Set.of(NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, NdkPlugin.MODEL_CARTOGRAPHIC, NdkPlugin.MODEL_GRAPHIC, NdkPlugin.MODEL_SHEETMUSIC, NdkPlugin.MODEL_CHAPTER, NdkPlugin.MODEL_PICTURE);
    private static final Set<String> EMONOGRAPH_MODELS = Set.of(NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, NdkEbornPlugin.MODEL_EMONOGRAPHUNIT, NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT, NdkEbornPlugin.MODEL_ECHAPTER);
    private static final Set<String> OLD_PRINT_MODELS = Set.of(OldPrintPlugin.MODEL_MONOGRAPHTITLE, OldPrintPlugin.MODEL_MONOGRAPHUNIT, OldPrintPlugin.MODEL_MONOGRAPHVOLUME, OldPrintPlugin.MODEL_SUPPLEMENT, OldPrintPlugin.MODEL_PAGE, OldPrintPlugin.MODEL_CHAPTER, OldPrintPlugin.MODEL_CONVOLUTTE, OldPrintPlugin.MODEL_GRAPHICS, OldPrintPlugin.MODEL_CARTOGRAPHIC, OldPrintPlugin.MODEL_SHEETMUSIC);
    private static final Set<String> SOUND_MODELS = Set.of(NdkAudioPlugin.MODEL_MUSICDOCUMENT, NdkAudioPlugin.MODEL_PHONOGRAPH, NdkAudioPlugin.MODEL_SONG, NdkAudioPlugin.MODEL_TRACK, NdkAudioPlugin.MODEL_PAGE);

    private Storage typeOfStorage;
    private AkubraStorage akubraStorage;
    private Map<String, Integer> elementIds = new HashMap<String, Integer>();
    private MetsElement rootElement;
    private Map<String, String> fsParentMap;
    private String path;
    private final List<String> generatedPSP = new ArrayList<String>();
    private boolean allowNonCompleteStreams = false;
    private boolean allowMissingURNNBN = false;
    private File packageDir;
    private String proarcVersion;
    private Float packageVersion;
    private JhoveContext jhoveContext;
    private NdkExportOptions options;

    /**
     * Sets options
     */
    public void setConfig(NdkExportOptions options) {
        this.options = options;
    }


    /**
     * Returns options
     */
    public NdkExportOptions getOptions() {
        return options;
    }

    /**
     * Returns the version of ProArc
     *
     * @return
     */
    public String getProarcVersion() {
        if (proarcVersion == null) {
            proarcVersion = AppConfiguration.VERSION;
        }
        return proarcVersion;
    }

    /**
     * Sets the ProArc version
     *
     * @param proarcVersion
     */
    public void setProarcVersion(String proarcVersion) {
        this.proarcVersion = proarcVersion;
    }

    public File getPackageDir() {
        return packageDir;
    }

    public void setPackageDir(File packageDir) {
        this.packageDir = packageDir;
    }

    /**
     * Resets the element Id counter
     *
     */
    public void resetContext() {
        elementIds = new HashMap<String, Integer>();
        packageDir = null;
        packageID = null;
        rootElement = null;
    }

    /**
     * returns true if URNNBN is not mandatory
     *
     * @return
     */
    public boolean isAllowMissingURNNBN() {
        return allowMissingURNNBN;
    }

    /**
     * Allows/disallows missing URNNBN in mods for logical units (Issue,
     * Monograph Unit)
     *
     * @param allowMissingURNNBN
     */
    public void setAllowMissingURNNBN(boolean allowMissingURNNBN) {
        this.allowMissingURNNBN = allowMissingURNNBN;
    }

    /**
     * returns true, if datastream presence is not mandatory
     *
     * @return
     */
    public boolean isAllowNonCompleteStreams() {
        return allowNonCompleteStreams;
    }

    /**
     * Allows/disallows optional/mandatory datastreams
     *
     * @param allowNonCompleteStreams
     */
    public void setAllowNonCompleteStreams(boolean allowNonCompleteStreams) {
        this.allowNonCompleteStreams = allowNonCompleteStreams;
    }

    public List<String> getGeneratedPSP() {
        return generatedPSP;
    }

    private String outputPath;
    private String packageID;
    private final MetsExportException metsExportException = new MetsExportException();
    private final List<FileMD5Info> fileList = new ArrayList<FileMD5Info>();
    private final HashMap<String, IMetsElement> pidElements = new HashMap<String, IMetsElement>();

    /**
     * return the map of elements for specified pid
     *
     * @return
     */
    public HashMap<String, IMetsElement> getPidElements() {
        return pidElements;
    }

    /**
     * Return the list of all exported files
     *
     * @return
     */
    public List<FileMD5Info> getFileList() {
        return fileList;
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
     * Returns a package ID for mets export
     *
     * @return
     */
    public String getPackageID() {
        return packageID;
    }

    /**
     * Sets the package ID for the mets export
     *
     * @param packageID
     */
    public void setPackageID(String packageID) {
        this.packageID = packageID;
    }

    public Optional<Float> getPackageVersion() {
        return Optional.ofNullable(packageVersion);
    }

    public void setPackageVersion(Float packageVersion) {
        this.packageVersion = packageVersion;
    }

    /**
     * Returns the output absolute path where output Mets file is stored
     *
     * @return
     */
    public String getOutputPath() {
        return outputPath;
    }

    /**
     * Sets the output absolute path where output Mets file is stored
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
     * Returns the root element of Mets export - used for MetsVisitor
     *
     * @return
     */
    public MetsElement getRootElement() {
        return rootElement;
    }

    /**
     * Sets the root element of Mets export
     *
     * @param rootElement
     */
    public void setRootElement(MetsElement rootElement) {
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
     * Gets the shared JHOVE instance.
     */
    public JhoveContext getJhoveContext() {
        return jhoveContext;
    }

    /**
     * Sets a shared JHOVE instance.
     */
    public void setJhoveContext(JhoveContext jhoveContext) {
        this.jhoveContext = jhoveContext;
    }

    public Storage getTypeOfStorage() {
        return typeOfStorage;
    }

    public void setTypeOfStorage(Storage typeOfStorage) {
        this.typeOfStorage = typeOfStorage;
    }

    public AkubraStorage getAkubraStorage() {
        return akubraStorage;
    }

    public void setAkubraStorage(AkubraStorage akubraStorage) {
        this.akubraStorage = akubraStorage;
    }

    public static MetsContext buildAkubraContext(ProArcObject object, String packageId, File targetFolder, AkubraStorage akubraStorage, NdkExportOptions exportOptions) {
        MetsContext metsContext = buildContext(object, packageId, targetFolder, exportOptions);
        metsContext.setTypeOfStorage(Storage.AKUBRA);
        metsContext.setAkubraStorage(akubraStorage);
        return metsContext;
    }

    private static MetsContext buildContext(ProArcObject fo, String packageId, File targetFolder, NdkExportOptions exportOptions) {
        MetsContext mc = new MetsContext();
        mc.setPackageID(packageId);
        mc.setPackageVersion(getPackageVersion(fo == null ? null : fo.getModel()));
        mc.setOutputPath(targetFolder == null ? null : targetFolder.getAbsolutePath());
        mc.setAllowNonCompleteStreams(false);
        mc.setAllowMissingURNNBN(false);
        mc.setConfig(exportOptions);
        return mc;
    }

    static float getPackageVersion(String model) {
        if (model == null) {
            return 0.0f;
        }
        String normalizedModel = model.startsWith(Const.FEDORAPREFIX) ? model.substring(Const.FEDORAPREFIX.length()) : model;
        if (PERIODICAL_MODELS.contains(normalizedModel)) {
            return 2.2f;
        } else if (EPERIODICAL_MODELS.contains(normalizedModel)) {
            return 2.6f;
        } else if (MONOGRAPH_MODELS.contains(normalizedModel)) {
            return 2.3f;
        } else if (OLD_PRINT_MODELS.contains(normalizedModel)) {
            return 2.0f;
        } else if (SOUND_MODELS.contains(normalizedModel)) {
            return 1.0f;
        } else if (EMONOGRAPH_MODELS.contains(normalizedModel)) {
            return 3.1f;
        } else {
            return 0.0f;
        }
    }
}
