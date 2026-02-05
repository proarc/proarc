package cz.cas.lib.proarc.common.xml.citation;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import java.util.List;

@XmlRootElement(name = "citation_list")
@XmlAccessorType(value = XmlAccessType.FIELD)
public class CitationList {

    @XmlElement(name = "citation")
    private List<Citation> citations;

    public List<Citation> getCitations() {
        return citations;
    }

    public void setCitations(List<Citation> citations) {
        this.citations = citations;
    }
}
