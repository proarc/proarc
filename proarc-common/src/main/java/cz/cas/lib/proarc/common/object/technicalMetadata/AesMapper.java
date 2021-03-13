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

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.object.technicalMetadata.TechnicalMetadataMapper.TechnicalMetadataWrapper;
import org.aes.audioobject.AudioObject;
import org.aes.audioobject.FaceRegionType;
import org.aes.audioobject.FaceType;
import org.aes.audioobject.FormatRegionType;
import org.aes.audioobject.FormatType;
import java.io.IOException;

public class AesMapper {

    public TechnicalMetadataWrapper toJsonObject(AudioObject data) {
        AesWrapper wrapper = new AesWrapper();
        wrapper.setMetadata("aes");
        wrapper.setAes(data);
        return wrapper;
    }

    public AudioObject fromJsonObject(String data) throws IOException {
        ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
        AesWrapper wrapper = jsMapper.readValue(data, AesWrapper.class);
        return wrapper.getAes();
    }

    /* Not supported yet */
    public void update(AudioObject aes) {
        setFormat(aes);
        setIdAndRef(aes);
    }

    private void setIdAndRef(AudioObject aes) {
        String aesId = "id1";
        String faceId = "id2";
        String formatId = "id3";
        String regionId = "id4";

        aes.setID(aesId);
        for (FaceType face : aes.getFace()) {
            face.setAudioObjectRef(aesId);
            face.setID(faceId);
            for (FaceRegionType region : face.getRegion()) {
                region.setID(regionId);
                region.setFaceRef(faceId);
                region.setFormatRef(formatId);
            }
        }
        if (aes.getFormatList() != null) {
            for (FormatRegionType formatRegion : aes.getFormatList().getFormatRegion()) {
                formatRegion.setID(formatId);
            }
        }
    }

    private void setFormat(AudioObject aes) {
        if (aes.getFormat() == null) {
            FormatType format = new FormatType();
            aes.setFormat(format);
        }

        FormatType format = aes.getFormat();
        format.setValue("Waveform_Audio");
    }

    public static class AesWrapper extends TechnicalMetadataWrapper {
        private AudioObject aes;

        public AudioObject getAes() {
            return aes;
        }

        public void setAes(AudioObject aes) {
            this.aes = aes;
        }
    }


}
