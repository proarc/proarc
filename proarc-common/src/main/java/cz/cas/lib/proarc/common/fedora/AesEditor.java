/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.fedora;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.aes57.Aes57Utils;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.mets.JHoveOutput;
import cz.cas.lib.proarc.common.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.MimeType;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.common.object.technicalMetadata.AesMapper;
import cz.cas.lib.proarc.mix.Mix;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Logger;
import javax.xml.transform.Source;
import org.aes.audioobject.AudioObject;
import org.aes.audioobject.AudioObjectType;

/**
 * Edits technical metadata in MIX format.
 *
 * @author Jan Pokorsky
 */
public class AesEditor {

    public static final String NDK_ARCHIVAL_ID = "NDK_ARCHIVAL_AES";
    public static final String RAW_ID = "RAW_AES";
    private static final String AES_FORMAT_URI = Aes57Utils.NS;

    private final XmlStreamEditor editor;
    private final FedoraObject object;
    private final DatastreamProfile profileTemplate;

    private static final Logger LOG = Logger.getLogger(AesEditor.class.getName());

    public static DatastreamProfile rawProfile() {
        return FoxmlUtils.managedProfile(RAW_ID, AES_FORMAT_URI, "Technical metadata for RAW stream.");
    }

    public static DatastreamProfile ndkArchivalProfile() {
        return FoxmlUtils.managedProfile(NDK_ARCHIVAL_ID, AES_FORMAT_URI, "Technical metadata for NDK_ARCHIVAL stream.");
    }

    /**
     * Gets editor to manage NDK_ARCHIVAL datastream metadata.
     * AES does not contain info about the scanner.
     */
    public static AesEditor ndkArchival(FedoraObject object) {
        return new AesEditor(object, ndkArchivalProfile());
    }

    /**
     * Gets editor to manage RAW datastream metadata. AES does not contain info
     * about the scanner.
     */
    public static AesEditor raw(FedoraObject object) {
        return new AesEditor(object, rawProfile());
    }

    public AesEditor(FedoraObject object, DatastreamProfile profile) {
        this.editor = object.getEditor(profile);
        this.object = object;
        this.profileTemplate = profile;
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    /**
     * Gets persisted MIX.
     * @return MIX or {@code null}
     * @throws DigitalObjectException failure
     */
    public AudioObjectType read() throws DigitalObjectException {
        Source src = editor.read();
        AudioObjectType result = null;
        if (src != null) {
            result = Aes57Utils.unmarshal(src, AudioObjectType.class);
        }
        return result;
    }

    /**
     * Gets persisted MIX as {@link Mix} class.
     * @return AES or {@code null}
     * @throws DigitalObjectException failure
     */
    public AudioObject readAes() throws DigitalObjectException {
        Source src = editor.read();
        AudioObject result = null;
        if (src != null) {
            result = Aes57Utils.unmarshal(src, AudioObject.class);
        }
        return result;
    }

    public String readAsString() throws DigitalObjectException {
        AudioObject aes = readAes();
        if (aes != null) {
            return Aes57Utils.toXml(aes, true);
        }
        return null;
    }

    public void write(AudioObject aes, long timestamp, String msg) throws DigitalObjectException {
        EditorResult result = editor.createResult();
        Aes57Utils.marshal(result, aes, true);
        editor.write(result, timestamp, msg);
    }

    /**
     * Generates and writes MIX for the passed content.
     *
     * @param content   file containing e.g. an image
     * @param jhoveCtx  jHove context
     * @param timestamp timestamp
     * @param msg       log message
     * @throws DigitalObjectException failure
     */
    public void write(File content, JhoveContext jhoveCtx, long timestamp, String msg) throws DigitalObjectException {
        try {
            AudioObject aes = JhoveUtility.getAes(content, jhoveCtx, null, null, null).getAes();
            if (aes == null) {
                LOG.warning("jHove cannot generate AES for " + content.toString() + ".");
                //throw new DigitalObjectException(object.getPid(), null, profileTemplate.getDsID(), "jHove cannot generate AES for " + content.toString(), null);
            } else {
                AesMapper mapper = new AesMapper();
                mapper.update(aes);
                write(aes, timestamp, msg);
            }
        } catch (DigitalObjectException ex) {
            throw ex;
        } catch (Exception ex) {
            throw new DigitalObjectException(
                    object.getPid(), null, profileTemplate.getDsID(), null, ex);
        }
    }

    public AudioObject generate(FedoraObject fobject, AppConfiguration config, String importName) throws DigitalObjectException {
        IMetsElement element = null;
        try {
            element = getElement(fobject.getPid(), config);
            DatastreamType ndkArchivalDS = FoxmlUtils.findDatastream(element.getSourceObject(), BinaryEditor.NDK_AUDIO_ARCHIVAL_ID);
            if (ndkArchivalDS != null) {
                GetDatastreamDissemination dsNdkArchival = FedoraClient.getDatastreamDissemination(element.getOriginalPid(), BinaryEditor.NDK_AUDIO_ARCHIVAL_ID);
                InputStream is = dsNdkArchival.execute(element.getMetsContext().getFedoraClient()).getEntityInputStream();
                String extension = MimeType.getExtension(ndkArchivalDS.getDatastreamVersion().get(0).getMIMETYPE());
                if (importName.contains("/")) {
                    importName = importName.split("/")[importName.split("/").length - 1];
                } else if (importName.contains("\\")) {
                    importName = importName.split("\\\\")[importName.split("\\\\").length - 1];
                }
                importName = importName.substring(0, importName.indexOf(".") - 1);
                File file = new File(getTemp(config), importName + "." + extension);
                file.createNewFile();

                try {
                    MetsUtils.getDigestAndCopy(is, new FileOutputStream(file));
                } catch (NoSuchAlgorithmException e) {
                    throw new DigitalObjectException(element.getOriginalPid(), "Unable to copy RAW image and get digest");
                }

                JHoveOutput jHoveOutput = JhoveUtility.createAes(new File(file.getAbsolutePath()), element.getMetsContext(), null, null, null, config);
                AudioObject audioObject = jHoveOutput.getAes();

                file.delete();

                return audioObject;
            }
            return null;
        } catch (IOException e) {
            throw new DigitalObjectException(fobject.getPid(), "Nepodarilo se vytvorit Technicka metadata");
        } catch (MetsExportException e) {
            throw new DigitalObjectException(fobject.getPid(), "Nepodařilo se vytvorit Technicka metadata");
        } catch (FedoraClientException e) {
            throw new DigitalObjectException(fobject.getPid(), "Nepodařilo se vytvořit Technická metadata");
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

    private IMetsElement getElement(String pid, AppConfiguration config) throws IOException, MetsExportException {
        RemoteStorage rstorage = RemoteStorage.getInstance(config);
        RemoteStorage.RemoteObject robject = rstorage.find(pid);
        MetsContext metsContext = buildContext(robject, null, null, rstorage);
        DigitalObject dobj = MetsUtils.readFoXML(robject.getPid(), robject.getClient());
        if (dobj == null) {
            return null;
        }
        return MetsElement.getElement(dobj, null, metsContext, true);
    }

    private MetsContext buildContext(RemoteStorage.RemoteObject fo, String packageId, File targetFolder, RemoteStorage rstorage) {
        MetsContext mc = new MetsContext();
        mc.setFedoraClient(fo.getClient());
        mc.setRemoteStorage(rstorage);
        mc.setPackageID(packageId);
        mc.setOutputPath(null);
        mc.setAllowNonCompleteStreams(false);
        mc.setAllowMissingURNNBN(false);
        mc.setConfig(null);
        return mc;
    }
}
