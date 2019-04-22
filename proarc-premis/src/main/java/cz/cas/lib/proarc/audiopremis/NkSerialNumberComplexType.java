package cz.cas.lib.proarc.audiopremis;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

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