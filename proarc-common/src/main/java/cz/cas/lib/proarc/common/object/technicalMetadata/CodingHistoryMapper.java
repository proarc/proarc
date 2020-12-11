package cz.cas.lib.proarc.common.object.technicalMetadata;

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.json.JsonUtils;
import edu.harvard.hul.ois.xml.ns.jhove.Property;
import java.io.IOException;

public class CodingHistoryMapper {

    public TechnicalMetadataMapper.TechnicalMetadataWrapper toJsonObject(Property data) {
        CodingHistoryWrapper wrapper = new CodingHistoryWrapper();
        wrapper.setMetadata("codingHistory");
        wrapper.setCodingHistory(data);
        return wrapper;
    }

    public Property fromJsonObject(String data) throws IOException {
        ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
        CodingHistoryWrapper wrapper = jsMapper.readValue(data, CodingHistoryWrapper.class);
        return wrapper.getCodingHistory();
    }

    /* Not supported yet */
    public void update(Property codingHistory) {
    }

    public static class CodingHistoryWrapper extends TechnicalMetadataMapper.TechnicalMetadataWrapper {
        private Property codingHistory;

        public Property getCodingHistory() {
            return codingHistory;
        }

        public void setCodingHistory(Property codingHistory) {
            this.codingHistory = codingHistory;
        }
    }
}
