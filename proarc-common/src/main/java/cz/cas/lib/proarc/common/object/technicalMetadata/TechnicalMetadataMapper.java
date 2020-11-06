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
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.fedora.AesEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.MixEditor;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import org.aes.audioobject.AudioObject;
import javax.xml.transform.stream.StreamSource;
import java.io.IOException;
import java.io.StringReader;

public class TechnicalMetadataMapper {

    private String model;
    private Integer batchId;
    private String pid;
    private AppConfiguration config;

    public TechnicalMetadataMapper(String model, Integer batchId, String pid, AppConfiguration config) {
        this.model = model;
        this.batchId = batchId;
        this.pid = pid;
        this.config = config;
    }

    public DescriptionMetadata<Object> getMetadataAsJsonObject(FedoraObject fobject, String importName) throws DigitalObjectException {
        if (model == null) {
            throw new DigitalObjectException("Missing model!");
        }
        switch(model) {
            case NdkAudioPlugin.MODEL_PAGE:
                return getAesMetadata(fobject, importName);
            /*case NdkPlugin.MODEL_PAGE:
                return getMixMetadata(fobject);
            case NdkPlugin.MODEL_NDK_PAGE:
                return getMixMetadata(fobject);
            case OldPrintPlugin.MODEL_PAGE:
                return getMixMetadata(fobject);*/
            default:
                throw new DigitalObjectException("Unsupported model");
        }
    }

    public String getMetadataAsXml(FedoraObject fobject, AppConfiguration config, String importFile) throws DigitalObjectException {
        if (model == null) {
            throw new DigitalObjectException("Missing model!");
        }
        switch(model) {
            case NdkAudioPlugin.MODEL_PAGE:
                return getAesMetadataAsXml(fobject, config, importFile);
            /*case NdkPlugin.MODEL_PAGE:
                return getMixMetadata(fobject);
            case NdkPlugin.MODEL_NDK_PAGE:
                return getMixMetadata(fobject);
            case OldPrintPlugin.MODEL_PAGE:
                return getMixMetadata(fobject);*/
            default:
                throw new DigitalObjectException("Unsupported model");
        }
    }

    private DescriptionMetadata<Object> getMixMetadata(FedoraObject fobject) throws DigitalObjectException {
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

    private DescriptionMetadata<Object> getAesMetadata(FedoraObject fobject, String importName) throws DigitalObjectException {
        AesEditor aesEditor = AesEditor.ndkArchival(fobject);
        AesMapper mapper = new AesMapper();

        DescriptionMetadata<AudioObject> dm = new DescriptionMetadata<>();
        dm.setPid(fobject.getPid());
        dm.setTimestamp(aesEditor.getLastModified());
        dm.setData(aesEditor.readAes());
        if (dm.getData() == null) {
            dm.setData(aesEditor.generate(fobject, config, importName));
        }

        DescriptionMetadata json = dm;
        

        json.setData(mapper.toJsonObject(dm.getData()));
        json.setEditor(model);
        return json;
    }

    private String getAesMetadataAsXml(FedoraObject fobject, AppConfiguration config, String importFile) throws DigitalObjectException {
        AesEditor aesEditor = AesEditor.ndkArchival(fobject);

        AudioObject aes = aesEditor.readAes();
        if (aes == null) {
            aes = aesEditor.generate(fobject, config, importFile);
        }

        if (aes != null) {
            return Aes57Utils.toXml(aes, true);
        }

        return null;
    }

    public void updateMetadataAsJson(FedoraObject fobject, String data, Long timestamp, String message) throws DigitalObjectException, IOException {
        if (model == null) {
            throw new DigitalObjectException("Missing model!");
        }
        switch(model) {
            case NdkAudioPlugin.MODEL_PAGE:
                updateAesMetadataAsJson(fobject, data, timestamp, message);
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

    private void updateMixMetadataAsJson(FedoraObject fobject, String data, Long timestamp, String message) throws IOException, DigitalObjectException {
        MixEditor editor = MixEditor.ndkArchival(fobject);
        MixMapper mapper = new MixMapper();

        Mix mix = mapper.fromJsonObject(data);
        mapper.update(mix);

        editor.write(mix, timestamp, message);
        fobject.flush();
    }

    private void updateAesMetadataAsJson(FedoraObject fobject, String data, Long timestamp, String message) throws IOException, DigitalObjectException {
        AesEditor editor = AesEditor.ndkArchival(fobject);
        AesMapper mapper = new AesMapper();

        AudioObject aes = mapper.fromJsonObject(data);
        mapper.update(aes);

        editor.write(aes, timestamp, message);
        fobject.flush();
    }

    public void updateMetadataAsXml(FedoraObject fobject, String data, Long timestamp, String message) throws DigitalObjectException {
        if (model == null) {
            throw new DigitalObjectException("Missing model!");
        }
        switch(model) {
            case NdkAudioPlugin.MODEL_PAGE:
                updateAesMetadataAsXml(fobject, data, timestamp, message);
                break;
            /*case NdkPlugin.MODEL_PAGE:
                updateMixMetadataAsXml(fobject, data, timestamp, message);
                break;
            case NdkPlugin.MODEL_NDK_PAGE:
                updateMixMetadataAsXml(fobject, data, timestamp, message);
                break;
            case OldPrintPlugin.MODEL_PAGE:
                updateMixMetadataAsXml(fobject, data, timestamp, message);
                break;*/
            default:
                throw new DigitalObjectException("Unsupported model");
        }
    }

    private void updateMixMetadataAsXml(FedoraObject fobject, String data, Long timestamp, String message) throws DigitalObjectException {
        MixEditor mixEditor = MixEditor.ndkArchival(fobject);
        Mix mix = MixUtils.unmarshalMix(new StreamSource(new StringReader(data)));

        /*
        MixMapper mixMapper = new MixMapper();
        mixMapper.update(mix)
        */

        mixEditor.write(mix, timestamp, message);
        fobject.flush();

    }

    private void updateAesMetadataAsXml(FedoraObject fobject, String data, Long timestamp, String message) throws DigitalObjectException {
        AesEditor aesEditor = AesEditor.ndkArchival(fobject);
        AudioObject aes = Aes57Utils.unmarshalAes(new StreamSource(new StringReader(data)));

        AesMapper aesMapper = new AesMapper();
        aesMapper.update(aes);

        aesEditor.write(aes, timestamp, message);
        fobject.flush();
    }


    public static class TechnicalMetadataWrapper {
        private String metadataType;

        public TechnicalMetadataWrapper() {}

        public TechnicalMetadataWrapper(String metadataType) {
            this.metadataType = metadataType;
        }

        public String getMetadata () {
            return  metadataType;
        }

        public void setMetadata(String metadataType) {
            this.metadataType = metadataType;
        }
    }





}
