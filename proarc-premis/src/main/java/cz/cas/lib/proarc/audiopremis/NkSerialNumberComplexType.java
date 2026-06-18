package cz.cas.lib.proarc.audiopremis;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "NkSerialNumberComplexType", namespace = "info:lc/xmlns/premis-v2", propOrder = {
        "nkSerialNumber"
})
public class NkSerialNumberComplexType {
    @XmlElement(namespace = "info:lc/xmlns/premis-v2", required = false)
    private String nkSerialNumber;

    public String getNkSerialNumber() {
        return nkSerialNumber;
    }

    public void setNkSerialNumber(String nkSerialNumber) {
        this.nkSerialNumber = nkSerialNumber;
    }
}