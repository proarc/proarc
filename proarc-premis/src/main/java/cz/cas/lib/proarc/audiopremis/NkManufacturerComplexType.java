package cz.cas.lib.proarc.audiopremis;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "NkManufacturerComplexType", namespace = "info:lc/xmlns/premis-v2", propOrder = {
        "nkManufacturer"
})
public class NkManufacturerComplexType {
    @XmlElement(namespace = "info:lc/xmlns/premis-v2", required = false)
    private String nkManufacturer;

    public String getNkManufacturer() {
        return nkManufacturer;
    }

    public void setNkManufacturer(String nkManufacturer) {
        this.nkManufacturer = nkManufacturer;
    }
}


