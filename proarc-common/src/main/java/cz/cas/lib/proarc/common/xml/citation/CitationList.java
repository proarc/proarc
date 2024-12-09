package cz.cas.lib.proarc.common.xml.citation;

import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

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
