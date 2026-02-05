package cz.cas.lib.proarc.audiopremis;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "NkSettingsComplexType", namespace = "info:lc/xmlns/premis-v2", propOrder = {
        "nkSettings"
})
public class NkSettingsComplexType {
    @XmlElement(namespace = "info:lc/xmlns/premis-v2", required = false)
    private String nkSettings;

    public String getNkSettings() {
        return nkSettings;
    }

    public void setNkSettings(String nkSettings) {
        this.nkSettings = nkSettings;
    }
}