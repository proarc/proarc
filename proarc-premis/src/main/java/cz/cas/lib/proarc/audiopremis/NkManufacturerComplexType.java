package cz.cas.lib.proarc.audiopremis;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;


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


