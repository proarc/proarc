/*
 * Copyright (C) 2014 Robert Simonovsky
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

package cz.cas.lib.proarc.mets;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlID;
import jakarta.xml.bind.annotation.XmlIDREF;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.CollapsedStringAdapter;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * divType: Complex Type for Divisions The METS standard represents a document
 * structurally as a series of nested div elements, that is, as a hierarchy
 * (e.g., a book, which is composed of chapters, which are composed of
 * subchapters, which are composed of text). Every div node in the structural
 * map hierarchy may be connected (via subsidiary mptr or fptr elements) to
 * content files which represent that div's portion of the whole document.
 * <p>
 * SPECIAL NOTE REGARDING DIV ATTRIBUTE VALUES: to clarify the differences
 * between the ORDER, ORDERLABEL, and LABEL attributes for the <div> element,
 * imagine a text with 10 roman numbered pages followed by 10 arabic numbered
 * pages. Page iii would have an ORDER of "3", an ORDERLABEL of "iii" and a
 * LABEL of "Page iii", while page 3 would have an ORDER of "13", an ORDERLABEL
 * of "3" and a LABEL of "Page 3".
 *
 *
 * <p>
 * Java class for divType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 *
 * <pre>
 * &lt;complexType name="divType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="mptr" maxOccurs="unbounded" minOccurs="0">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;attGroup ref="{http://www.w3.org/1999/xlink}simpleLink"/>
 *                 &lt;attGroup ref="{http://www.loc.gov/METS/}LOCATION"/>
 *                 &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *                 &lt;attribute name="CONTENTIDS" type="{http://www.loc.gov/METS/}URIs" />
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;element name="fptr" maxOccurs="unbounded" minOccurs="0">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;choice>
 *                   &lt;element name="par" type="{http://www.loc.gov/METS/}parType" minOccurs="0"/>
 *                   &lt;element name="seq" type="{http://www.loc.gov/METS/}seqType" minOccurs="0"/>
 *                   &lt;element name="area" type="{http://www.loc.gov/METS/}areaType" minOccurs="0"/>
 *                 &lt;/choice>
 *                 &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *                 &lt;attribute name="FILEID" type="{http://www.w3.org/2001/XMLSchema}IDREF" />
 *                 &lt;attribute name="CONTENTIDS" type="{http://www.loc.gov/METS/}URIs" />
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;element name="div" type="{http://www.loc.gov/METS/}divType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *       &lt;attribute name="ORDER" type="{http://www.w3.org/2001/XMLSchema}integer" />
 *       &lt;attribute name="ORDERLABEL" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="LABEL" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="DMDID" type="{http://www.w3.org/2001/XMLSchema}IDREFS" />
 *       &lt;attribute name="ADMID" type="{http://www.w3.org/2001/XMLSchema}IDREFS" />
 *       &lt;attribute name="TYPE" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="CONTENTIDS" type="{http://www.loc.gov/METS/}URIs" />
 *       &lt;attribute ref="{http://www.w3.org/1999/xlink}label"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "divType", namespace = "http://www.loc.gov/METS/", propOrder = {"mptr", "fptr", "div"})
public class DivType {

    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<Mptr> mptr;
    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<DivType.Fptr> fptr;
    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<DivType> div;
    @XmlAttribute(name = "ID")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;
    @XmlAttribute(name = "ORDER")
    protected BigInteger order;
    @XmlAttribute(name = "ORDERLABEL")
    protected String orderlabel;
    @XmlAttribute(name = "LABEL")
    protected String label3;
    @XmlAttribute(name = "DMDID")
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Object> dmdid;
    @XmlAttribute(name = "ADMID")
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Object> admid;
    @XmlAttribute(name = "TYPE")
    protected String type;
    @XmlAttribute(name = "CONTENTIDS")
    protected List<String> contentids;
    @XmlAttribute(name = "label", namespace = "http://www.w3.org/1999/xlink")
    protected String label;

    /**
     * Gets the value of the mptr property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the mptr property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getMptr().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link DivType.Mptr }
     *
     *
     */
    public List<DivType.Mptr> getMptr() {
        if (mptr == null) {
            mptr = new ArrayList<Mptr>();
        }
        return this.mptr;
    }

    /**
     * Gets the value of the fptr property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the fptr property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getFptr().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link DivType.Fptr }
     *
     *
     */
    public List<DivType.Fptr> getFptr() {
        if (fptr == null) {
            fptr = new ArrayList<DivType.Fptr>();
        }
        return this.fptr;
    }

    /**
     * Gets the value of the div property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the div property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getDiv().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list {@link DivType }
     *
     *
     */
    public List<DivType> getDiv() {
        if (div == null) {
            div = new ArrayList<DivType>();
        }
        return this.div;
    }

    /**
     * Gets the value of the id property.
     *
     * @return possible object is {@link String }
     *
     */
    public String getID() {
        return id;
    }

    /**
     * Sets the value of the id property.
     *
     * @param value allowed object is {@link String }
     *
     */
    public void setID(String value) {
        this.id = value;
    }

    /**
     * Gets the value of the order property.
     *
     * @return possible object is {@link BigInteger }
     *
     */
    public BigInteger getORDER() {
        return order;
    }

    /**
     * Sets the value of the order property.
     *
     * @param value allowed object is {@link BigInteger }
     *
     */
    public void setORDER(BigInteger value) {
        this.order = value;
    }

    /**
     * Gets the value of the orderlabel property.
     *
     * @return possible object is {@link String }
     *
     */
    public String getORDERLABEL() {
        return orderlabel;
    }

    /**
     * Sets the value of the orderlabel property.
     *
     * @param value allowed object is {@link String }
     *
     */
    public void setORDERLABEL(String value) {
        this.orderlabel = value;
    }

    /**
     * Gets the value of the label3 property.
     *
     * @return possible object is {@link String }
     *
     */
    public String getLabel3() {
        return label3;
    }

    /**
     * Sets the value of the label3 property.
     *
     * @param value allowed object is {@link String }
     *
     */
    public void setLabel3(String value) {
        this.label3 = value;
    }

    /**
     * Gets the value of the dmdid property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the dmdid property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getDMDID().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list {@link Object }
     *
     *
     */
    public List<Object> getDMDID() {
        if (dmdid == null) {
            dmdid = new ArrayList<Object>();
        }
        return this.dmdid;
    }

    /**
     * Gets the value of the admid property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the admid property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getADMID().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list {@link Object }
     *
     *
     */
    public List<Object> getADMID() {
        if (admid == null) {
            admid = new ArrayList<Object>();
        }
        return this.admid;
    }

    /**
     * Gets the value of the type property.
     *
     * @return possible object is {@link String }
     *
     */
    public String getTYPE() {
        return type;
    }

    /**
     * Sets the value of the type property.
     *
     * @param value allowed object is {@link String }
     *
     */
    public void setTYPE(String value) {
        this.type = value;
    }

    /**
     * Gets the value of the contentids property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the contentids property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getCONTENTIDS().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list {@link String }
     *
     *
     */
    public List<String> getCONTENTIDS() {
        if (contentids == null) {
            contentids = new ArrayList<String>();
        }
        return this.contentids;
    }

    /**
     * xlink:label - an xlink label to be referred to by an smLink element
     *
     * @return possible object is {@link String }
     *
     */
    public String getLabel() {
        return label;
    }

    /**
     * Sets the value of the label property.
     *
     * @param value allowed object is {@link String }
     *
     */
    public void setLabel(String value) {
        this.label = value;
    }

    /**
     * <p>
     * Java class for anonymous complex type.
     *
     * <p>
     * The following schema fragment specifies the expected content contained
     * within this class.
     *
     * <pre>
     * &lt;complexType>
     *   &lt;complexContent>
     *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *       &lt;choice>
     *         &lt;element name="par" type="{http://www.loc.gov/METS/}parType" minOccurs="0"/>
     *         &lt;element name="seq" type="{http://www.loc.gov/METS/}seqType" minOccurs="0"/>
     *         &lt;element name="area" type="{http://www.loc.gov/METS/}areaType" minOccurs="0"/>
     *       &lt;/choice>
     *       &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
     *       &lt;attribute name="FILEID" type="{http://www.w3.org/2001/XMLSchema}IDREF" />
     *       &lt;attribute name="CONTENTIDS" type="{http://www.loc.gov/METS/}URIs" />
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     *
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = {"par", "seq", "area"})
    public static class Fptr {

        @XmlElement(namespace = "http://www.loc.gov/METS/")
        protected ParType par;
        @XmlElement(namespace = "http://www.loc.gov/METS/")
        protected SeqType seq;
        @XmlElement(namespace = "http://www.loc.gov/METS/")
        protected AreaType area;
        @XmlAttribute(name = "ID")
        @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
        @XmlID
        @XmlSchemaType(name = "ID")
        protected String id;
        @XmlAttribute(name = "FILEID")
        @XmlIDREF
        @XmlSchemaType(name = "IDREF")
        protected Object fileid;
        @XmlAttribute(name = "CONTENTIDS")
        protected List<String> contentids;

        /**
         * Gets the value of the par property.
         *
         * @return possible object is {@link ParType }
         *
         */
        public ParType getPar() {
            return par;
        }

        /**
         * Sets the value of the par property.
         *
         * @param value allowed object is {@link ParType }
         *
         */
        public void setPar(ParType value) {
            this.par = value;
        }

        /**
         * Gets the value of the seq property.
         *
         * @return possible object is {@link SeqType }
         *
         */
        public SeqType getSeq() {
            return seq;
        }

        /**
         * Sets the value of the seq property.
         *
         * @param value allowed object is {@link SeqType }
         *
         */
        public void setSeq(SeqType value) {
            this.seq = value;
        }

        /**
         * Gets the value of the area property.
         *
         * @return possible object is {@link AreaType }
         *
         */
        public AreaType getArea() {
            return area;
        }

        /**
         * Sets the value of the area property.
         *
         * @param value allowed object is {@link AreaType }
         *
         */
        public void setArea(AreaType value) {
            this.area = value;
        }

        /**
         * Gets the value of the id property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getID() {
            return id;
        }

        /**
         * Sets the value of the id property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setID(String value) {
            this.id = value;
        }

        /**
         * Gets the value of the fileid property.
         *
         * @return possible object is {@link Object }
         *
         */
        public Object getFILEID() {
            return fileid;
        }

        /**
         * Sets the value of the fileid property.
         *
         * @param value allowed object is {@link Object }
         *
         */
        public void setFILEID(Object value) {
            this.fileid = value;
        }

        /**
         * Gets the value of the contentids property.
         *
         * <p>
         * This accessor method returns a reference to the live list, not a
         * snapshot. Therefore any modification you make to the returned list
         * will be present inside the JAXB object. This is why there is not a
         * <CODE>set</CODE> method for the contentids property.
         *
         * <p>
         * For example, to add a new item, do as follows:
         *
         * <pre>
         * getCONTENTIDS().add(newItem);
         * </pre>
         *
         *
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link String }
         *
         *
         */
        public List<String> getCONTENTIDS() {
            if (contentids == null) {
                contentids = new ArrayList<String>();
            }
            return this.contentids;
        }

    }

    /**
     * <p>
     * Java class for anonymous complex type.
     *
     * <p>
     * The following schema fragment specifies the expected content contained
     * within this class.
     *
     * <pre>
     * &lt;complexType>
     *   &lt;complexContent>
     *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *       &lt;attGroup ref="{http://www.w3.org/1999/xlink}simpleLink"/>
     *       &lt;attGroup ref="{http://www.loc.gov/METS/}LOCATION"/>
     *       &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
     *       &lt;attribute name="CONTENTIDS" type="{http://www.loc.gov/METS/}URIs" />
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     *
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "")
    public static class Mptr {

        @XmlAttribute(name = "ID")
        @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
        @XmlID
        @XmlSchemaType(name = "ID")
        protected String id;
        @XmlAttribute(name = "CONTENTIDS")
        protected List<String> contentids;
        @XmlAttribute(name = "type", namespace = "http://www.w3.org/1999/xlink")
        protected String type;
        @XmlAttribute(name = "href", namespace = "http://www.w3.org/1999/xlink")
        @XmlSchemaType(name = "anyURI")
        protected String href;
        @XmlAttribute(name = "role", namespace = "http://www.w3.org/1999/xlink")
        protected String role;
        @XmlAttribute(name = "arcrole", namespace = "http://www.w3.org/1999/xlink")
        protected String arcrole;
        @XmlAttribute(name = "title", namespace = "http://www.w3.org/1999/xlink")
        protected String title;
        @XmlAttribute(name = "show", namespace = "http://www.w3.org/1999/xlink")
        protected String show;
        @XmlAttribute(name = "actuate", namespace = "http://www.w3.org/1999/xlink")
        protected String actuate;
        @XmlAttribute(name = "LOCTYPE", required = true)
        protected String loctype;
        @XmlAttribute(name = "OTHERLOCTYPE")
        protected String otherloctype;

        /**
         * Gets the value of the id property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getID() {
            return id;
        }

        /**
         * Sets the value of the id property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setID(String value) {
            this.id = value;
        }

        /**
         * Gets the value of the contentids property.
         *
         * <p>
         * This accessor method returns a reference to the live list, not a
         * snapshot. Therefore any modification you make to the returned list
         * will be present inside the JAXB object. This is why there is not a
         * <CODE>set</CODE> method for the contentids property.
         *
         * <p>
         * For example, to add a new item, do as follows:
         *
         * <pre>
         * getCONTENTIDS().add(newItem);
         * </pre>
         *
         *
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link String }
         *
         *
         */
        public List<String> getCONTENTIDS() {
            if (contentids == null) {
                contentids = new ArrayList<String>();
            }
            return this.contentids;
        }

        /**
         * Gets the value of the type property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getType() {
            if (type == null) {
                return "simple";
            } else {
                return type;
            }
        }

        /**
         * Sets the value of the type property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setType(String value) {
            this.type = value;
        }

        /**
         * Gets the value of the href property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getHref() {
            return href;
        }

        /**
         * Sets the value of the href property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setHref(String value) {
            this.href = value;
        }

        /**
         * Gets the value of the role property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getRole() {
            return role;
        }

        /**
         * Sets the value of the role property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setRole(String value) {
            this.role = value;
        }

        /**
         * Gets the value of the arcrole property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getArcrole() {
            return arcrole;
        }

        /**
         * Sets the value of the arcrole property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setArcrole(String value) {
            this.arcrole = value;
        }

        /**
         * Gets the value of the title property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getTitle() {
            return title;
        }

        /**
         * Sets the value of the title property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setTitle(String value) {
            this.title = value;
        }

        /**
         * Gets the value of the show property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getShow() {
            return show;
        }

        /**
         * Sets the value of the show property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setShow(String value) {
            this.show = value;
        }

        /**
         * Gets the value of the actuate property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getActuate() {
            return actuate;
        }

        /**
         * Sets the value of the actuate property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setActuate(String value) {
            this.actuate = value;
        }

        /**
         * Gets the value of the loctype property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getLOCTYPE() {
            return loctype;
        }

        /**
         * Sets the value of the loctype property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setLOCTYPE(String value) {
            this.loctype = value;
        }

        /**
         * Gets the value of the otherloctype property.
         *
         * @return possible object is {@link String }
         *
         */
        public String getOTHERLOCTYPE() {
            return otherloctype;
        }

        /**
         * Sets the value of the otherloctype property.
         *
         * @param value allowed object is {@link String }
         *
         */
        public void setOTHERLOCTYPE(String value) {
            this.otherloctype = value;
        }

    }

}
