package cz.cas.lib.proarc.common.object.technicalMetadata;

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.mets.Mets;
import java.io.IOException;

public class PremisMapper {

    public TechnicalMetadataMapper.TechnicalMetadataWrapper toJsonObject(Mets data) {
        PremisWrapper wrapper = new PremisWrapper();
        wrapper.setMetadata("premis");
        wrapper.setMets(data);
        return wrapper;
    }

    public Mets fromJsonObject(String data) throws IOException {
        ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
        PremisWrapper wrapper = jsMapper.readValue(data, PremisWrapper.class);
        return wrapper.getMets();
    }

    /* Not supported yet */
    public void update(Mets mets) {
    }

    public static class PremisWrapper extends TechnicalMetadataMapper.TechnicalMetadataWrapper {
        private Mets mets;

        public Mets getMets() {
            return mets;
        }

        public void setMets(Mets mets) {
            this.mets = mets;
        }
    }
}
