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
package cz.cas.lib.proarc.common.object.technicalMetadata;

import cz.cas.lib.proarc.aes57.Aes57Utils;
import cz.cas.lib.proarc.codingHistory.CodingHistoryUtils;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.AesEditor;
import cz.cas.lib.proarc.common.storage.CodingHistoryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.PremisEditor;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import edu.harvard.hul.ois.xml.ns.jhove.Property;
import java.io.IOException;
import java.io.StringReader;
import javax.xml.transform.stream.StreamSource;
import org.aes.audioobject.AudioObject;

public class TechnicalMetadataMapper {

    private String model;
    private Integer batchId;
    private String pid;
    private AppConfiguration config;
    private final AkubraConfiguration akubraConfiguration;

    public static final String AES = "aes";
    public static final String CODING_HISTORY = "codingHistory";
    public static final String PREMIS = "premis";
    public static final String MIX = "mix";

    public TechnicalMetadataMapper(String model, Integer batchId, String pid, AppConfiguration config, AkubraConfiguration akubraConfiguration) {
        this.model = model;
        this.batchId = batchId;
        this.pid = pid;
        this.config = config;
        this.akubraConfiguration = akubraConfiguration;
    }

    public DescriptionMetadata<Object> getMetadataAsJsonObject(ProArcObject fobject, String importName, String type) throws DigitalObjectException {
        if (model == null) {
            throw new DigitalObjectException("Missing model!");
        }
        if (type == null || type.length() == 0) {
            throw new DigitalObjectException("Missing type of date!");
        }
        switch (model) {
            case NdkAudioPlugin.MODEL_PAGE:
                switch (type) {
                    case AES:
                        return getAesMetadata(fobject, importName);
                    case CODING_HISTORY:
                        return getCodingHistoryMetadata(fobject, importName);
                    default:
                        throw new DigitalObjectException("Unsupported type of data");
                }
            case NdkPlugin.MODEL_PAGE:
            case NdkPlugin.MODEL_NDK_PAGE:
            case OldPrintPlugin.MODEL_PAGE:
                switch (type) {
                    case PREMIS:
                        return getPremisMetadata(fobject, importName);
//                    case MIX:
//                        return getMixMetadata(fobject);
                    default:
                        throw new DigitalObjectException("Unsupported type of data");
                }
            default:
                throw new DigitalObjectException("Unsupported model");
        }
    }

    public String getMetadataAsXml(ProArcObject fobject, AppConfiguration config, String importFile, String type) throws DigitalObjectException {
        if (model == null) {
            throw new DigitalObjectException("Missing model!");
        }
        if (type == null || type.length() == 0) {
            throw new DigitalObjectException("Missing type of date!");
        }
        switch (model) {
            case NdkAudioPlugin.MODEL_PAGE:
                switch (type) {
                    case AES:
                        return getAesMetadataAsXml(fobject, importFile);
                    case CODING_HISTORY:
                        return getCodingHistoryAsXml(fobject, importFile);
                    default:
                        throw new DigitalObjectException("Unsupported type of data");
                }
            case NdkPlugin.MODEL_PAGE:
            case NdkPlugin.MODEL_NDK_PAGE:
            case OldPrintPlugin.MODEL_PAGE:
                switch (type) {
                    case PREMIS:
                        return getPremisAsXml(fobject, importFile);
//                    case MIX:
//                        return getMixAsXml(fobject, importFile);
                    default:
                        throw new DigitalObjectException("Unsupported type of data");
                }
            default:
                throw new DigitalObjectException("Unsupported model");
        }
    }

    private DescriptionMetadata<Object> getMixMetadata(ProArcObject fobject) throws DigitalObjectException {
        MixEditor mixEditor = MixEditor.ndkArchival(fobject);
        DescriptionMetadata<Mix> dm = new DescriptionMetadata<>();
        dm.setPid(fobject.getPid());
        dm.setTimestamp(mixEditor.getLastModified());
        dm.setData(mixEditor.readMix());

        DescriptionMetadata json = dm;

        MixMapper mapper = new MixMapper();
        json.setData(mapper.toJsonObject(dm.getData()));
        json.setEditor(model);
        return json;
    }

    private DescriptionMetadata<Object> getAesMetadata(ProArcObject fobject, String importName) throws DigitalObjectException {
        AesEditor aesEditor = AesEditor.ndkArchival(fobject);
        AesMapper mapper = new AesMapper();

        DescriptionMetadata<AudioObject> dm = new DescriptionMetadata<>();
        dm.setPid(fobject.getPid());
        dm.setTimestamp(aesEditor.getLastModified());
        dm.setData(aesEditor.readAes());
        if (dm.getData() == null && !(fobject instanceof LocalStorage.LocalObject)) {
            dm.setData(aesEditor.generate(fobject, config, akubraConfiguration, importName));
        }

        DescriptionMetadata json = dm;


        json.setData(mapper.toJsonObject(dm.getData()));
        json.setEditor(model);
        return json;
    }

    private DescriptionMetadata<Object> getCodingHistoryMetadata(ProArcObject fobject, String importName) throws DigitalObjectException {
        CodingHistoryEditor codingHistoryEditor = CodingHistoryEditor.ndkArchival(fobject);
        CodingHistoryMapper mapper = new CodingHistoryMapper();

        DescriptionMetadata<Property> dm = new DescriptionMetadata<>();
        dm.setPid(fobject.getPid());
        dm.setTimestamp(codingHistoryEditor.getLastModified());
        dm.setData(codingHistoryEditor.readCodingHistory());
        if (dm.getData() == null && !(fobject instanceof LocalStorage.LocalObject)) {
            dm.setData(codingHistoryEditor.generate(fobject, config, akubraConfiguration, importName));
        }

        DescriptionMetadata json = dm;


        json.setData(mapper.toJsonObject(dm.getData()));
        json.setEditor(model);
        return json;
    }

    public DescriptionMetadata<Object> getPremisMetadata(ProArcObject fobject, String importName) throws DigitalObjectException {
        PremisEditor premisEditor = PremisEditor.ndkArchival(fobject);
        PremisMapper mapper = new PremisMapper();

        DescriptionMetadata<Mets> dm = new DescriptionMetadata<>();
        dm.setPid(fobject.getPid());
        dm.setTimestamp(premisEditor.getLastModified());
        dm.setData(premisEditor.readMets());
        if (dm.getData() == null && !(fobject instanceof LocalStorage.LocalObject)) {
            dm.setData(premisEditor.generate(fobject, config, akubraConfiguration, importName));
        }

        DescriptionMetadata json = dm;

        json.setData(mapper.toJsonObject(dm.getData()));
        json.setEditor(model);
        return json;
    }

    private String getAesMetadataAsXml(ProArcObject fobject, String importFile) throws DigitalObjectException {
        AesEditor aesEditor = AesEditor.ndkArchival(fobject);

        AudioObject aes = aesEditor.readAes();
        if (aes == null && !(fobject instanceof LocalStorage.LocalObject)) {
            aes = aesEditor.generate(fobject, config, akubraConfiguration, importFile);
        }

        if (aes != null) {
            return Aes57Utils.toXml(aes, true);
        }

        return null;
    }

    private String getCodingHistoryAsXml(ProArcObject fobject, String importFile) throws DigitalObjectException {
        CodingHistoryEditor codingHistoryEditor = CodingHistoryEditor.ndkArchival(fobject);

        Property codingHistory = codingHistoryEditor.readCodingHistory();
        if (codingHistory == null && !(fobject instanceof LocalStorage.LocalObject)) {
            codingHistory = codingHistoryEditor.generate(fobject, config, akubraConfiguration, importFile);
        }
        if (codingHistory != null) {
            return CodingHistoryUtils.toXml(codingHistory, true);
        }
        return null;
    }

    private String getPremisAsXml(ProArcObject fobject, String importFile) throws DigitalObjectException {
        PremisEditor premisEditor = PremisEditor.ndkArchival(fobject);

        Mets mets = premisEditor.readMets();
        if (mets == null && !(fobject instanceof LocalStorage.LocalObject)) {
            mets = premisEditor.generate(fobject, config, akubraConfiguration, importFile);
        }
        if (mets != null) {
            return MetsUtils.toXml(mets, true);
        }
        return null;
    }

    public void updateMetadataAsJson(ProArcObject fobject, String data, Long timestamp, String message, String type) throws DigitalObjectException, IOException {
        if (model == null) {
            throw new DigitalObjectException("Missing model!");
        }
        if (type == null || type.length() == 0) {
            throw new DigitalObjectException("Missing type of date!");
        }
        switch (model) {
            case NdkAudioPlugin.MODEL_PAGE:
                switch (type) {
                    case AES:
                        updateAesMetadataAsJson(fobject, data, timestamp, message);
                        break;
                    case CODING_HISTORY:
                        updateCodingHistoryMetadataAsJson(fobject, data, timestamp, message);
                        break;
                    default:
                        throw new DigitalObjectException("Unsupported type of data");
                }
                break;
            /*case NdkPlugin.MODEL_PAGE:
                updateMixMetadataAsJson(fobject, data, timestamp, message);
                break;
            case NdkPlugin.MODEL_NDK_PAGE:
                updateMixMetadataAsJson(fobject, data, timestamp, message);
                break;
            case OldPrintPlugin.MODEL_PAGE:
                updateMixMetadataAsJson(fobject, data, timestamp, message);
                break;*/
            default:
                throw new DigitalObjectException("Unsupported model/ Nepodporovany model");
        }
    }

    private void updateMixMetadataAsJson(ProArcObject fobject, String data, Long timestamp, String message) throws IOException, DigitalObjectException {
        MixEditor editor = MixEditor.ndkArchival(fobject);
        MixMapper mapper = new MixMapper();

        Mix mix = mapper.fromJsonObject(data);
        mapper.update(mix);

        editor.write(mix, timestamp, message);
        fobject.flush();
    }

    private void updateAesMetadataAsJson(ProArcObject fobject, String data, Long timestamp, String message) throws IOException, DigitalObjectException {
        AesEditor editor = AesEditor.ndkArchival(fobject);
        AesMapper mapper = new AesMapper();

        AudioObject aes = mapper.fromJsonObject(data);
        mapper.update(aes);

        editor.write(aes, timestamp, message);
        fobject.flush();
    }

    private void updateCodingHistoryMetadataAsJson(ProArcObject fobject, String data, Long timestamp, String message) throws IOException, DigitalObjectException {
        CodingHistoryEditor editor = CodingHistoryEditor.ndkArchival(fobject);
        CodingHistoryMapper mapper = new CodingHistoryMapper();

        Property codingHistory = mapper.fromJsonObject(data);
        mapper.update(codingHistory);

        editor.write(codingHistory, timestamp, message);
        fobject.flush();
    }

    public void updateMetadataAsXml(ProArcObject fobject, String data, Long timestamp, String message, String type) throws DigitalObjectException {
        if (model == null) {
            throw new DigitalObjectException("Missing model!");
        }
        switch (model) {
            case NdkAudioPlugin.MODEL_PAGE:
                switch (type) {
                    case AES:
                        updateAesMetadataAsXml(fobject, data, timestamp, message);
                        return;
                    case CODING_HISTORY:
                        updateCodingHistoryMetadataAsXml(fobject, data, timestamp, message);
                        return;
                    default:
                        throw new DigitalObjectException("Unsupported type of data");
                }
            case NdkPlugin.MODEL_PAGE:
            case NdkPlugin.MODEL_NDK_PAGE:
            case OldPrintPlugin.MODEL_PAGE:
                switch (type) {
                    case PREMIS:
                        updatePremisAsXml(fobject, data, timestamp, message);
                        return;
//                    case MIX:
//                       updateMixAsXml(fobject, data, timestamp, message);
                    default:
                        throw new DigitalObjectException("Unsupported type of data");
                }
            default:
                throw new DigitalObjectException("Unsupported model");
        }
    }

    private void updateMixMetadataAsXml(ProArcObject fobject, String data, Long timestamp, String message) throws DigitalObjectException {
        MixEditor mixEditor = MixEditor.ndkArchival(fobject);
        Mix mix = MixUtils.unmarshalMix(new StreamSource(new StringReader(data)));

        /*
        MixMapper mixMapper = new MixMapper();
        mixMapper.update(mix)
        */

        mixEditor.write(mix, timestamp, message);
        fobject.flush();

    }

    private void updateAesMetadataAsXml(ProArcObject fobject, String data, Long timestamp, String message) throws DigitalObjectException {
        AesEditor aesEditor = AesEditor.ndkArchival(fobject);
        AudioObject aes = Aes57Utils.unmarshalAes(new StreamSource(new StringReader(data)));

        AesMapper aesMapper = new AesMapper();
        aesMapper.update(aes);

        aesEditor.write(aes, timestamp, message);
        fobject.flush();
    }

    private void updateCodingHistoryMetadataAsXml(ProArcObject fobject, String data, Long timestamp, String message) throws DigitalObjectException {
        CodingHistoryEditor codingHistoryEditor = CodingHistoryEditor.ndkArchival(fobject);
        Property codingHistory = CodingHistoryUtils.unmarshalCodingHistory(new StreamSource(new StringReader(data)));

        CodingHistoryMapper mapper = new CodingHistoryMapper();
        mapper.update(codingHistory);

        codingHistoryEditor.write(codingHistory, timestamp, message);
        fobject.flush();
    }

    private void updatePremisAsXml(ProArcObject fobject, String data, Long timestamp, String message) throws DigitalObjectException {
        PremisEditor premisEditor = PremisEditor.ndkArchival(fobject);
        Mets mets = MetsUtils.unmarshalMets(new StreamSource(new StringReader(data)));

        PremisMapper mapper = new PremisMapper();
        mapper.update(mets);

        premisEditor.write(mets, timestamp, message);
        fobject.flush();
    }


    public static class TechnicalMetadataWrapper {
        private String metadataType;

        public TechnicalMetadataWrapper() {
        }

        public TechnicalMetadataWrapper(String metadataType) {
            this.metadataType = metadataType;
        }

        public String getMetadata() {
            return metadataType;
        }

        public void setMetadata(String metadataType) {
            this.metadataType = metadataType;
        }
    }


}
