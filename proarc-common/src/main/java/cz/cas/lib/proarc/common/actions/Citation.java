package cz.cas.lib.proarc.common.actions;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "citation")
@XmlAccessorType(value = XmlAccessType.FIELD)
public class Citation {

    @XmlAttribute(name = "key")
    private String key;

    @XmlElement
    private String journal_title;

    @XmlElement
    private String volume_title;

    @XmlElement
    private String article_title;

    @XmlElement
    private String volume;

    @XmlElement
    private String issue;

    @XmlElement
    private String cYear;

    @XmlElement
    private String author;

    @XmlElement
    private String first_page;

    @XmlElement
    private String last_page;

    @XmlElement
    private String isbn;

    @XmlElement
    private String issn;

    @XmlElement
    private String doi;


    public String getKey() {
        return key;
    }

    public String getArticle_title() {
        return article_title;
    }

    public String getAuthor() {
        return author;
    }

    public String getDoi() {
        return doi;
    }

    public String getFirst_page() {
        return first_page;
    }

    public String getLast_page() {
        return last_page;
    }

    public String getJournal_title() {
        return journal_title;
    }

    public String getVolume_title() {
        return volume_title;
    }

    public String getVolume() {
        return volume;
    }

    public String getIssue() {
        return issue;
    }

    public String getcYear() {
        return cYear;
    }

    public String getIsbn() {
        return isbn;
    }

    public String getIssn() {
        return issn;
    }
}
