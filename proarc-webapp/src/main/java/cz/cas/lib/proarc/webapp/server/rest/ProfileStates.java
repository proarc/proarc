package cz.cas.lib.proarc.webapp.server.rest;

import com.fasterxml.jackson.annotation.JsonProperty;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.FIELD)
public class ProfileStates {

    @XmlElement(name = ImportResourceApi.IMPORT_BATCH_PROFILE)
    @JsonProperty(ImportResourceApi.IMPORT_BATCH_PROFILE)
    String profileId;
    @XmlElement(name = ImportResourceApi.IMPORT_FOLDER_STATE)
    @JsonProperty(ImportResourceApi.IMPORT_FOLDER_STATE)
    String state;
    @XmlElement(name = "params")
    @JsonProperty("params")
    Params params;

    public ProfileStates(String profileId, String state) {
        this(profileId, state, null);
    }

    public ProfileStates(String profileId, String state, Params params) {
        this.profileId = profileId;
        this.state = state;
        this.params = params;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Params {

        @XmlElement(name = "device")
        @JsonProperty("device")
        boolean device;
        @XmlElement(name = "software")
        @JsonProperty("software")
        boolean software;
        @XmlElement(name = "ocrEngine")
        @JsonProperty("ocrEngine")
        boolean ocrEngine;
        @XmlElement(name = "metakatEngine")
        @JsonProperty("metakatEngine")
        boolean metakatEngine;
        @XmlElement(name = "generateIndex")
        @JsonProperty("generateIndex")
        boolean generateIndex;

        public Params(boolean device, boolean software, boolean ocrEngine, boolean metakatEngine, boolean generateIndex) {
            this.device = device;
            this.software = software;
            this.ocrEngine = ocrEngine;
            this.metakatEngine = metakatEngine;
            this.generateIndex = generateIndex;
        }
    }
}
