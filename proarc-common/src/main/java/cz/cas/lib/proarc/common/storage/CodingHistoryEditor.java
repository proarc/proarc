/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.common.storage;


import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.codingHistory.CodingHistoryUtils;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.export.mets.JHoveOutput;
import cz.cas.lib.proarc.common.process.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.process.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.MimeType;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.storage.akubra.AkubraUtils;
import cz.cas.lib.proarc.common.object.technicalMetadata.CodingHistoryMapper;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Logger;
import javax.xml.transform.Source;
import edu.harvard.hul.ois.xml.ns.jhove.Property;
import edu.harvard.hul.ois.xml.ns.jhove.PropertyType;

import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildAkubraContext;
import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildFedoraContext;

/**
 * Edits technical metadata in Coding history format.
 *
 * @author Lukas Sykora
 */
public class CodingHistoryEditor {

    public static final String NDK_ARCHIVAL_ID = "NDK_ARCHIVAL_CODINGHISTORY";
    public static final String RAW_ID = "RAW_CODING_HISTORY";
    private static final String AES_FORMAT_URI = CodingHistoryUtils.NS;

    private final XmlStreamEditor editor;
    private final ProArcObject object;
    private final DatastreamProfile profileTemplate;

    private static final Logger LOG = Logger.getLogger(CodingHistoryEditor.class.getName());

    public static DatastreamProfile rawProfile() {
        return FoxmlUtils.managedProfile(RAW_ID, AES_FORMAT_URI, "Technical metadata (coding history) for RAW stream.");
    }

    public static DatastreamProfile ndkArchivalProfile() {
        return FoxmlUtils.managedProfile(NDK_ARCHIVAL_ID, AES_FORMAT_URI, "Technical metadata (coding history) for NDK_ARCHIVAL stream.");
    }

    /**
     * Gets editor to manage NDK_ARCHIVAL datastream metadata.
     * Coding history does not contain info about the scanner.
     */
    public static CodingHistoryEditor ndkArchival(ProArcObject object) {
        return new CodingHistoryEditor(object, ndkArchivalProfile());
    }

    /**
     * Gets editor to manage RAW datastream metadata. AES does not contain info
     * about the scanner.
     */
    public static CodingHistoryEditor raw(ProArcObject object) {
        return new CodingHistoryEditor(object, rawProfile());
    }

    public CodingHistoryEditor(ProArcObject object, DatastreamProfile profile) {
        this.editor = object.getEditor(profile);
        this.object = object;
        this.profileTemplate = profile;
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    /**
     * Gets persisted Coding History.
     * @return PropertyType or {@code null}
     * @throws DigitalObjectException failure
     */
    public PropertyType read() throws DigitalObjectException {
        Source src = editor.read();
        PropertyType result = null;
        if (src != null) {
            result = CodingHistoryUtils.unmarshal(src, PropertyType.class);
        }
        return result;
    }

    /**
     * Gets persisted Coding History as {@link Property} class.
     * @return Property or {@code null}
     * @throws DigitalObjectException failure
     */
    public Property readCodingHistory() throws DigitalObjectException {
        Source src = editor.read();
        Property result = null;
        if (src != null) {
            result = CodingHistoryUtils.unmarshal(src, Property.class);
        }
        return result;
    }

    public String readAsString() throws DigitalObjectException {
        Property codingHistory = readCodingHistory();
        if (codingHistory != null) {
            return CodingHistoryUtils.toXml(codingHistory, true);
        }
        return null;
    }

    public void write(Property codingHistory, long timestamp, String msg) throws DigitalObjectException {
        EditorResult result = editor.createResult();
        CodingHistoryUtils.marshal(result, codingHistory, true);
        editor.write(result, timestamp, msg);
    }

    /**
     * Generates and writes Coding History for the passed content.
     *
     * @param content file containing e.g. an image
     * @param jhoveCtx jHove context
     * @param timestamp timestamp
     * @param msg log message
     * @throws DigitalObjectException failure
     */
    public void write(File content, JhoveContext jhoveCtx, long timestamp, String msg) throws DigitalObjectException {
        try {
            Property codingHistory = JhoveUtility.getCodingHistory(content, jhoveCtx, null, null).getCodingHistory();
            if (codingHistory == null) {
                LOG.warning("jHove cannot generate Coding history for " + content.toString() + ".");
                //throw new DigitalObjectException(object.getPid(), null, profileTemplate.getDsID(), "jHove cannot generate Coding history for " + content.toString(), null);
            } else {
                CodingHistoryMapper mapper = new CodingHistoryMapper();
                mapper.update(codingHistory);
                write(codingHistory, timestamp, msg);
            }
        } catch (DigitalObjectException ex) {
            throw ex;
        } catch (Exception ex) {
            throw new DigitalObjectException(
                    object.getPid(), null, profileTemplate.getDsID(), null, ex);
        }
    }

    public Property generate(ProArcObject fobject, AppConfiguration config, AkubraConfiguration akubraConfiguration, String importName) throws DigitalObjectException {
        IMetsElement element = null;
        try {
            element = getElement(fobject.getPid(), config, akubraConfiguration);
            DatastreamType ndkArchivalDS = FoxmlUtils.findDatastream(element.getSourceObject(), BinaryEditor.NDK_AUDIO_ARCHIVAL_ID);
            if (ndkArchivalDS != null) {
                InputStream inputStream = null;
                if (Storage.FEDORA.equals(element.getMetsContext().getTypeOfStorage())) {
                    GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(element.getOriginalPid(), BinaryEditor.NDK_AUDIO_ARCHIVAL_ID);
                    inputStream = dsRaw.execute(element.getMetsContext().getFedoraClient()).getEntityInputStream();
                } else if (Storage.AKUBRA.equals(element.getMetsContext().getTypeOfStorage())) {
                    AkubraObject object = element.getMetsContext().getAkubraStorage().find(element.getOriginalPid());
                    inputStream = AkubraUtils.getDatastreamDissemination(object, BinaryEditor.NDK_AUDIO_ARCHIVAL_ID);
                } else {
                    throw new IllegalStateException("Unsupported type of Storage: " + element.getMetsContext().getTypeOfStorage());
                }
                String extension = MimeType.getExtension(ndkArchivalDS.getDatastreamVersion().get(0).getMIMETYPE());
                if (importName.contains("/")) {
                    importName = importName.split("/")[importName.split("/").length -1];
                } else if (importName.contains("\\")) {
                    importName = importName.split("\\\\")[importName.split("\\\\").length - 1];
                }
                importName = importName.substring(0, importName.indexOf(".") - 1);
                File file = new File(getTemp(config), importName + "." + extension);
                file.createNewFile();

                try {
                    MetsUtils.getDigestAndCopy(inputStream, new FileOutputStream(file));
                } catch (NoSuchAlgorithmException e) {
                    throw new DigitalObjectException(element.getOriginalPid(), "Unable to copy RAW image and get digest");
                }

                JHoveOutput jHoveOutput = JhoveUtility.createCodingHistory(new File(file.getAbsolutePath()), element.getMetsContext(), null, null, config);
                Property codingHistory = jHoveOutput.getCodingHistory();

                file.delete();

                return codingHistory;
            }
            return null;
        } catch (Exception e) {
            throw new DigitalObjectException(fobject.getPid(), "Nepodarilo se vytvorit Technicka metadata");
        } finally {
            if (element != null) {
                JhoveUtility.destroyConfigFiles(element.getMetsContext().getJhoveContext());
            }
        }
    }

    private File getTemp(AppConfiguration config) throws IOException {
        File home = config.getConfigHome();
        for (File file : home.listFiles()) {
            if (file.getName().equals("temp")) {
                return file;
            }
        }
        File temp = new File(home, "temp");
        temp.mkdir();
        return temp;
    }

    private IMetsElement getElement(String pid, AppConfiguration config, AkubraConfiguration akubraConfiguration) throws IOException, MetsExportException {
        MetsContext metsContext = null;
        ProArcObject object = null;

        if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
            FedoraStorage fedoraStorage = FedoraStorage.getInstance(config);
            object = fedoraStorage.find(pid);
            metsContext = buildFedoraContext(object, null, null, fedoraStorage, config.getNdkExportOptions());
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
            object = akubraStorage.find(pid);
            metsContext = buildAkubraContext(object, null, null, akubraStorage, config.getNdkExportOptions());
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
        }

        DigitalObject dobj = MetsUtils.readFoXML(object.getPid(), metsContext);
        if (dobj == null) {
            return null;
        }
        return MetsElement.getElement(dobj, null, metsContext, true);
    }
}
