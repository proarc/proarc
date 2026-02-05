package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.FIELD)
public class ProfileStates {

    @XmlElement(name = ImportResourceApi.IMPORT_BATCH_PROFILE)
    String profileId;
    @XmlElement(name = ImportResourceApi.IMPORT_FOLDER_STATE)
    String state;

    public ProfileStates(String profileId, String state) {
        this.profileId = profileId;
        this.state = state;
    }
}
