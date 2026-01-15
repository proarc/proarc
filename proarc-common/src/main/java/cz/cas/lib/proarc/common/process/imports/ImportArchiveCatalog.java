package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;

@XmlRootElement(name = "Archivy")
@XmlAccessorType(value = XmlAccessType.FIELD)
public class ImportArchiveCatalog {

    @XmlElement(name = "Archiv")
    private List<Archiv> archivy;

    public List<Archiv> getArchivy() {
        if (archivy == null) {
            archivy = new ArrayList<>();
        }
        return archivy;
    }

    @XmlType(namespace = ProarcXmlUtils.NS_IMPORT)
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Archiv {
        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String id;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String value;

        public String getId() {
            return id;
        }

        public void setId(String id) {
            this.id = id;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }
    }
}

