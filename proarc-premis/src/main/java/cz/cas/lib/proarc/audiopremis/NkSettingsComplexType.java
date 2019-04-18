package cz.cas.lib.proarc.audiopremis;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "NkSettingsComplexType", namespace = "info:lc/xmlns/premis-v2", propOrder = {
        "nkSettings"
})
public class NkSettingsComplexType {
    @XmlElement(namespace = "info:lc/xmlns/premis-v2",required = false)
    private String nkSettings;

    public String getNkSettings() {
        return nkSettings;
    }

    public void setNkSettings(String nkSettings) {
        this.nkSettings = nkSettings;
    }
}