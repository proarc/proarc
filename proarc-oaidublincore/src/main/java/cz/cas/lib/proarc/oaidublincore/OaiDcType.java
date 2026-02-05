/*
 * Copyright (C) 2013 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.oaidublincore;

import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;

import static cz.cas.lib.proarc.oaidublincore.DcConstants.CONTRIBUTOR;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.COVERAGE;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.CREATOR;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.DATE;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.DC;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.DESCRIPTION;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.FORMAT;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.IDENTIFIER;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.LANGUAGE;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.NS_OAIDC;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.NS_PURL;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.PUBLISHER;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.RELATION;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.RIGHTS;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.SOURCE;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.SUBJECT;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.TITLE;
import static cz.cas.lib.proarc.oaidublincore.DcConstants.TYPE;

/**
 * <p>Java class for dc element of oai_dcType complex type.
 * <p>
 * It simplifies work with DC elements and allows to read and generate data
 * in JSON format.
 *
 * <p>The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType name="oai_dcType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;choice maxOccurs="unbounded" minOccurs="0">
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}title"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}creator"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}subject"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}description"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}publisher"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}contributor"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}date"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}type"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}format"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}identifier"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}source"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}language"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}relation"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}coverage"/>
 *         &lt;element ref="{http://purl.org/dc/elements/1.1/}rights"/>
 *       &lt;/choice>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = DC, namespace = NS_OAIDC)
@XmlType(name = "oai_dcType", namespace = NS_OAIDC)
public class OaiDcType {

    @XmlElement(name = CONTRIBUTOR, namespace = NS_PURL)
    private List<ElementType> contributors;

    @XmlElement(name = COVERAGE, namespace = NS_PURL)
    private List<ElementType> coverages;

    @XmlElement(name = CREATOR, namespace = NS_PURL)
    private List<ElementType> creators;

    @XmlElement(name = DATE, namespace = NS_PURL)
    private List<ElementType> dates;

    @XmlElement(name = DESCRIPTION, namespace = NS_PURL)
    private List<ElementType> descriptions;

    @XmlElement(name = FORMAT, namespace = NS_PURL)
    private List<ElementType> formats;

    @XmlElement(name = IDENTIFIER, namespace = NS_PURL)
    private List<ElementType> identifiers;

    @XmlElement(name = LANGUAGE, namespace = NS_PURL)
    private List<ElementType> languages;

    @XmlElement(name = PUBLISHER, namespace = NS_PURL)
    private List<ElementType> publishers;

    @XmlElement(name = RELATION, namespace = NS_PURL)
    private List<ElementType> relations;

    @XmlElement(name = RIGHTS, namespace = NS_PURL)
    private List<ElementType> rights;

    @XmlElement(name = SOURCE, namespace = NS_PURL)
    private List<ElementType> sources;

    @XmlElement(name = SUBJECT, namespace = NS_PURL)
    private List<ElementType> subjects;

    @XmlElement(name = TITLE, namespace = NS_PURL)
    private List<ElementType> titles;

    @XmlElement(name = TYPE, namespace = NS_PURL)
    private List<ElementType> types;

    public OaiDcType() {
    }

    public List<ElementType> getContributors() {
        if (contributors == null) {
            contributors = new ArrayList<ElementType>();
        }
        return contributors;
    }

    public void setContributors(List<ElementType> contributors) {
        this.contributors = contributors;
    }

    public List<ElementType> getCoverages() {
        if (coverages == null) {
            coverages = new ArrayList<ElementType>();
        }
        return coverages;
    }

    public void setCoverages(List<ElementType> coverages) {
        this.coverages = coverages;
    }

    public List<ElementType> getCreators() {
        if (creators == null) {
            creators = new ArrayList<ElementType>();
        }
        return creators;
    }

    public void setCreators(List<ElementType> creators) {
        this.creators = creators;
    }

    public List<ElementType> getDates() {
        if (dates == null) {
            dates = new ArrayList<ElementType>();
        }
        return dates;
    }

    public void setDates(List<ElementType> dates) {
        this.dates = dates;
    }

    public List<ElementType> getDescriptions() {
        if (descriptions == null) {
            descriptions = new ArrayList<ElementType>();
        }
        return descriptions;
    }

    public void setDescriptions(List<ElementType> descriptions) {
        this.descriptions = descriptions;
    }

    public List<ElementType> getFormats() {
        if (formats == null) {
            formats = new ArrayList<ElementType>();
        }
        return formats;
    }

    public void setFormats(List<ElementType> formats) {
        this.formats = formats;
    }

    public List<ElementType> getIdentifiers() {
        if (identifiers == null) {
            identifiers = new ArrayList<ElementType>();
        }
        return identifiers;
    }

    public void setIdentifiers(List<ElementType> identifier) {
        this.identifiers = identifier;
    }

    public List<ElementType> getLanguages() {
        if (languages == null) {
            languages = new ArrayList<ElementType>();
        }
        return languages;
    }

    public void setLanguages(List<ElementType> languages) {
        this.languages = languages;
    }

    public List<ElementType> getPublishers() {
        if (publishers == null) {
            publishers = new ArrayList<ElementType>();
        }
        return publishers;
    }

    public void setPublishers(List<ElementType> publishers) {
        this.publishers = publishers;
    }

    public List<ElementType> getRelations() {
        if (relations == null) {
            relations = new ArrayList<ElementType>();
        }
        return relations;
    }

    public void setRelations(List<ElementType> relations) {
        this.relations = relations;
    }

    public List<ElementType> getRights() {
        if (rights == null) {
            rights = new ArrayList<ElementType>();
        }
        return rights;
    }

    public void setRights(List<ElementType> rights) {
        this.rights = rights;
    }

    public List<ElementType> getSources() {
        if (sources == null) {
            sources = new ArrayList<ElementType>();
        }
        return sources;
    }

    public void setSources(List<ElementType> sources) {
        this.sources = sources;
    }

    public List<ElementType> getSubjects() {
        if (subjects == null) {
            subjects = new ArrayList<ElementType>();
        }
        return subjects;
    }

    public void setSubjects(List<ElementType> subjects) {
        this.subjects = subjects;
    }

    public List<ElementType> getTitles() {
        if (titles == null) {
            titles = new ArrayList<ElementType>();
        }
        return titles;
    }

    public void setTitles(List<ElementType> titles) {
        this.titles = titles;
    }

    public List<ElementType> getTypes() {
        if (types == null) {
            types = new ArrayList<ElementType>();
        }
        return types;
    }

    public void setTypes(List<ElementType> types) {
        this.types = types;
    }

    void beforeMarshal(Marshaller m) {
        collapse(contributors);
        collapse(coverages);
        collapse(creators);
        collapse(dates);
        collapse(descriptions);
        collapse(formats);
        collapse(identifiers);
        collapse(languages);
        collapse(publishers);
        collapse(relations);
        collapse(rights);
        collapse(sources);
        collapse(subjects);
        collapse(titles);
        collapse(types);
    }

    /**
     * Collapses empty elements. It trims element values and removes elements
     * with empty value.
     */
    private static List<ElementType> collapse(List<ElementType> l) {
        if (l != null) {
            for (int i = l.size() - 1; i >= 0; i--) {
                ElementType elm = l.get(i);
                String string = null;
                if (elm != null) {
                    string = elm.getValue();
                    string = string == null ? string : string.trim();
                }
                if (string == null || string.isEmpty()) {
                    l.remove(i);
                } else {
                    elm.setValue(string);
                }
            }
        }
        return l;
    }

}
