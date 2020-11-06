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
import cz.cas.lib.proarc.mix.Mix;
import java.io.IOException;

public class MixMapper {

    public TechnicalMetadataWrapper toJsonObject(Mix mix) {
        MixWrapper wrapper = new MixWrapper();
        wrapper.setMetadata("mix");
        wrapper.setMix(mix);
        return wrapper;
    }

    public Mix fromJsonObject(String data) throws IOException {
        ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
        MixWrapper wrapper = jsMapper.readValue(data, MixWrapper.class);
        return wrapper.getMix();
    }

    /* Not supported yet */
    public void update (Mix mix) {
    }

    public static class MixWrapper extends TechnicalMetadataWrapper {
        private Mix mix;

        public Mix getMix() {
            return mix;
        }

        public void setMix(Mix mix) {
            this.mix = mix;
        }
    }

}
