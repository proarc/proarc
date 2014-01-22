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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.datatype.XMLGregorianCalendar;

import org.w3c.dom.Element;

/**
 * mdSecType: Complex Type for Metadata Sections A generic framework for
 * pointing to/including metadata within a METS document, a la Warwick
 * Framework.
 * 
 * 
 * <p>
 * Java class for mdSecType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="mdSecType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;all>
 *         &lt;element name="mdRef" minOccurs="0">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;attGroup ref="{http://www.loc.gov/METS/}METADATA"/>
 *                 &lt;attGroup ref="{http://www.loc.gov/METS/}LOCATION"/>
 *                 &lt;attGroup ref="{http://www.w3.org/1999/xlink}simpleLink"/>
 *                 &lt;attGroup ref="{http://www.loc.gov/METS/}FILECORE"/>
 *                 &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *                 &lt;attribute name="LABEL" type="{http://www.w3.org/2001/XMLSchema}string" />
 *                 &lt;attribute name="XPTR" type="{http://www.w3.org/2001/XMLSchema}string" />
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;element name="mdWrap" minOccurs="0">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;choice>
 *                   &lt;element name="binData" type="{http://www.w3.org/2001/XMLSchema}base64Binary" minOccurs="0"/>
 *                   &lt;element name="xmlData" minOccurs="0">
 *                     &lt;complexType>
 *                       &lt;complexContent>
 *                         &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                           &lt;sequence>
 *                             &lt;any processContents='lax' maxOccurs="unbounded"/>
 *                           &lt;/sequence>
 *                         &lt;/restriction>
 *                       &lt;/complexContent>
 *                     &lt;/complexType>
 *                   &lt;/element>
 *                 &lt;/choice>
 *                 &lt;attGroup ref="{http://www.loc.gov/METS/}FILECORE"/>
 *                 &lt;attGroup ref="{http://www.loc.gov/METS/}METADATA"/>
 *                 &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *                 &lt;attribute name="LABEL" type="{http://www.w3.org/2001/XMLSchema}string" />
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *       &lt;/all>
 *       &lt;attribute name="ID" use="required" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *       &lt;attribute name="GROUPID" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="ADMID" type="{http://www.w3.org/2001/XMLSchema}IDREFS" />
 *       &lt;attribute name="CREATED" type="{http://www.w3.org/2001/XMLSchema}dateTime" />
 *       &lt;attribute name="STATUS" type="{http://www.w3.org/2001/XMLSchema}string" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "mdSecType", namespace = "http://www.loc.gov/METS/", propOrder = {

})
public class MdSecType {

    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected MdSecType.MdRef mdRef;
    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected MdSecType.MdWrap mdWrap;
    @XmlAttribute(name = "ID", required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;
    @XmlAttribute(name = "GROUPID")
    protected String groupid;
    @XmlAttribute(name = "ADMID")
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Object> admid;
    @XmlAttribute(name = "CREATED")
    @XmlSchemaType(name = "dateTime")
    protected XMLGregorianCalendar created;
    @XmlAttribute(name = "STATUS")
    protected String status;

    /**
     * Gets the value of the mdRef property.
     * 
     * @return possible object is {@link MdSecType.MdRef }
     * 
     */
    public MdSecType.MdRef getMdRef() {
        return mdRef;
    }

    /**
     * Sets the value of the mdRef property.
     * 
     * @param value
     *            allowed object is {@link MdSecType.MdRef }
     * 
     */
    public void setMdRef(MdSecType.MdRef value) {
        this.mdRef = value;
    }

    /**
     * Gets the value of the mdWrap property.
     * 
     * @return possible object is {@link MdSecType.MdWrap }
     * 
     */
    public MdSecType.MdWrap getMdWrap() {
        return mdWrap;
    }

    /**
     * Sets the value of the mdWrap property.
     * 
     * @param value
     *            allowed object is {@link MdSecType.MdWrap }
     * 
     */
    public void setMdWrap(MdSecType.MdWrap value) {
        this.mdWrap = value;
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
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setID(String value) {
        this.id = value;
    }

    /**
     * Gets the value of the groupid property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getGROUPID() {
        return groupid;
    }

    /**
     * Sets the value of the groupid property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setGROUPID(String value) {
        this.groupid = value;
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
     * Gets the value of the created property.
     * 
     * @return possible object is {@link XMLGregorianCalendar }
     * 
     */
    public XMLGregorianCalendar getCREATED() {
        return created;
    }

    /**
     * Sets the value of the created property.
     * 
     * @param value
     *            allowed object is {@link XMLGregorianCalendar }
     * 
     */
    public void setCREATED(XMLGregorianCalendar value) {
        this.created = value;
    }

    /**
     * Gets the value of the status property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getSTATUS() {
        return status;
    }

    /**
     * Sets the value of the status property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setSTATUS(String value) {
        this.status = value;
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
     *       &lt;attGroup ref="{http://www.loc.gov/METS/}METADATA"/>
     *       &lt;attGroup ref="{http://www.loc.gov/METS/}LOCATION"/>
     *       &lt;attGroup ref="{http://www.w3.org/1999/xlink}simpleLink"/>
     *       &lt;attGroup ref="{http://www.loc.gov/METS/}FILECORE"/>
     *       &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
     *       &lt;attribute name="LABEL" type="{http://www.w3.org/2001/XMLSchema}string" />
     *       &lt;attribute name="XPTR" type="{http://www.w3.org/2001/XMLSchema}string" />
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "")
    public static class MdRef {

        @XmlAttribute(name = "ID")
        @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
        @XmlID
        @XmlSchemaType(name = "ID")
        protected String id;
        @XmlAttribute(name = "LABEL")
        protected String label7;
        @XmlAttribute(name = "XPTR")
        protected String xptr;
        @XmlAttribute(name = "MDTYPE", required = true)
        protected String mdtype;
        @XmlAttribute(name = "OTHERMDTYPE")
        protected String othermdtype;
        @XmlAttribute(name = "MDTYPEVERSION")
        protected String mdtypeversion;
        @XmlAttribute(name = "LOCTYPE", required = true)
        protected String loctype;
        @XmlAttribute(name = "OTHERLOCTYPE")
        protected String otherloctype;
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
        @XmlAttribute(name = "MIMETYPE")
        protected String mimetype;
        @XmlAttribute(name = "SIZE")
        protected Long size;
        @XmlAttribute(name = "CREATED")
        @XmlSchemaType(name = "dateTime")
        protected XMLGregorianCalendar created;
        @XmlAttribute(name = "CHECKSUM")
        protected String checksum;
        @XmlAttribute(name = "CHECKSUMTYPE")
        protected String checksumtype;

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
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setID(String value) {
            this.id = value;
        }

        /**
         * Gets the value of the label7 property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getLabel7() {
            return label7;
        }

        /**
         * Sets the value of the label7 property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setLabel7(String value) {
            this.label7 = value;
        }

        /**
         * Gets the value of the xptr property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getXPTR() {
            return xptr;
        }

        /**
         * Sets the value of the xptr property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setXPTR(String value) {
            this.xptr = value;
        }

        /**
         * Gets the value of the mdtype property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getMDTYPE() {
            return mdtype;
        }

        /**
         * Sets the value of the mdtype property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setMDTYPE(String value) {
            this.mdtype = value;
        }

        /**
         * Gets the value of the othermdtype property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getOTHERMDTYPE() {
            return othermdtype;
        }

        /**
         * Sets the value of the othermdtype property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setOTHERMDTYPE(String value) {
            this.othermdtype = value;
        }

        /**
         * Gets the value of the mdtypeversion property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getMDTYPEVERSION() {
            return mdtypeversion;
        }

        /**
         * Sets the value of the mdtypeversion property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setMDTYPEVERSION(String value) {
            this.mdtypeversion = value;
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
         * @param value
         *            allowed object is {@link String }
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
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setOTHERLOCTYPE(String value) {
            this.otherloctype = value;
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
         * @param value
         *            allowed object is {@link String }
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
         * @param value
         *            allowed object is {@link String }
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
         * @param value
         *            allowed object is {@link String }
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
         * @param value
         *            allowed object is {@link String }
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
         * @param value
         *            allowed object is {@link String }
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
         * @param value
         *            allowed object is {@link String }
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
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setActuate(String value) {
            this.actuate = value;
        }

        /**
         * Gets the value of the mimetype property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getMIMETYPE() {
            return mimetype;
        }

        /**
         * Sets the value of the mimetype property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setMIMETYPE(String value) {
            this.mimetype = value;
        }

        /**
         * Gets the value of the size property.
         * 
         * @return possible object is {@link Long }
         * 
         */
        public Long getSIZE() {
            return size;
        }

        /**
         * Sets the value of the size property.
         * 
         * @param value
         *            allowed object is {@link Long }
         * 
         */
        public void setSIZE(Long value) {
            this.size = value;
        }

        /**
         * Gets the value of the created property.
         * 
         * @return possible object is {@link XMLGregorianCalendar }
         * 
         */
        public XMLGregorianCalendar getCREATED() {
            return created;
        }

        /**
         * Sets the value of the created property.
         * 
         * @param value
         *            allowed object is {@link XMLGregorianCalendar }
         * 
         */
        public void setCREATED(XMLGregorianCalendar value) {
            this.created = value;
        }

        /**
         * Gets the value of the checksum property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getCHECKSUM() {
            return checksum;
        }

        /**
         * Sets the value of the checksum property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setCHECKSUM(String value) {
            this.checksum = value;
        }

        /**
         * Gets the value of the checksumtype property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getCHECKSUMTYPE() {
            return checksumtype;
        }

        /**
         * Sets the value of the checksumtype property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setCHECKSUMTYPE(String value) {
            this.checksumtype = value;
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
     *       &lt;choice>
     *         &lt;element name="binData" type="{http://www.w3.org/2001/XMLSchema}base64Binary" minOccurs="0"/>
     *         &lt;element name="xmlData" minOccurs="0">
     *           &lt;complexType>
     *             &lt;complexContent>
     *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *                 &lt;sequence>
     *                   &lt;any processContents='lax' maxOccurs="unbounded"/>
     *                 &lt;/sequence>
     *               &lt;/restriction>
     *             &lt;/complexContent>
     *           &lt;/complexType>
     *         &lt;/element>
     *       &lt;/choice>
     *       &lt;attGroup ref="{http://www.loc.gov/METS/}FILECORE"/>
     *       &lt;attGroup ref="{http://www.loc.gov/METS/}METADATA"/>
     *       &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
     *       &lt;attribute name="LABEL" type="{http://www.w3.org/2001/XMLSchema}string" />
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = { "binData", "xmlData" })
    public static class MdWrap {

        @XmlElement(namespace = "http://www.loc.gov/METS/")
        protected byte[] binData;
        @XmlElement(namespace = "http://www.loc.gov/METS/")
        protected MdSecType.MdWrap.XmlData xmlData;
        @XmlAttribute(name = "ID")
        @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
        @XmlID
        @XmlSchemaType(name = "ID")
        protected String id;
        @XmlAttribute(name = "LABEL")
        protected String label8;
        @XmlAttribute(name = "MIMETYPE")
        protected String mimetype;
        @XmlAttribute(name = "SIZE")
        protected Long size;
        @XmlAttribute(name = "CREATED")
        @XmlSchemaType(name = "dateTime")
        protected XMLGregorianCalendar created;
        @XmlAttribute(name = "CHECKSUM")
        protected String checksum;
        @XmlAttribute(name = "CHECKSUMTYPE")
        protected String checksumtype;
        @XmlAttribute(name = "MDTYPE", required = true)
        protected String mdtype;
        @XmlAttribute(name = "OTHERMDTYPE")
        protected String othermdtype;
        @XmlAttribute(name = "MDTYPEVERSION")
        protected String mdtypeversion;

        /**
         * Gets the value of the binData property.
         * 
         * @return possible object is byte[]
         */
        public byte[] getBinData() {
            return binData;
        }

        /**
         * Sets the value of the binData property.
         * 
         * @param value
         *            allowed object is byte[]
         */
        public void setBinData(byte[] value) {
            this.binData = value;
        }

        /**
         * Gets the value of the xmlData property.
         * 
         * @return possible object is {@link MdSecType.MdWrap.XmlData }
         * 
         */
        public MdSecType.MdWrap.XmlData getXmlData() {
            return xmlData;
        }

        /**
         * Sets the value of the xmlData property.
         * 
         * @param value
         *            allowed object is {@link MdSecType.MdWrap.XmlData }
         * 
         */
        public void setXmlData(MdSecType.MdWrap.XmlData value) {
            this.xmlData = value;
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
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setID(String value) {
            this.id = value;
        }

        /**
         * Gets the value of the label8 property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getLabel8() {
            return label8;
        }

        /**
         * Sets the value of the label8 property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setLabel8(String value) {
            this.label8 = value;
        }

        /**
         * Gets the value of the mimetype property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getMIMETYPE() {
            return mimetype;
        }

        /**
         * Sets the value of the mimetype property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setMIMETYPE(String value) {
            this.mimetype = value;
        }

        /**
         * Gets the value of the size property.
         * 
         * @return possible object is {@link Long }
         * 
         */
        public Long getSIZE() {
            return size;
        }

        /**
         * Sets the value of the size property.
         * 
         * @param value
         *            allowed object is {@link Long }
         * 
         */
        public void setSIZE(Long value) {
            this.size = value;
        }

        /**
         * Gets the value of the created property.
         * 
         * @return possible object is {@link XMLGregorianCalendar }
         * 
         */
        public XMLGregorianCalendar getCREATED() {
            return created;
        }

        /**
         * Sets the value of the created property.
         * 
         * @param value
         *            allowed object is {@link XMLGregorianCalendar }
         * 
         */
        public void setCREATED(XMLGregorianCalendar value) {
            this.created = value;
        }

        /**
         * Gets the value of the checksum property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getCHECKSUM() {
            return checksum;
        }

        /**
         * Sets the value of the checksum property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setCHECKSUM(String value) {
            this.checksum = value;
        }

        /**
         * Gets the value of the checksumtype property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getCHECKSUMTYPE() {
            return checksumtype;
        }

        /**
         * Sets the value of the checksumtype property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setCHECKSUMTYPE(String value) {
            this.checksumtype = value;
        }

        /**
         * Gets the value of the mdtype property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getMDTYPE() {
            return mdtype;
        }

        /**
         * Sets the value of the mdtype property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setMDTYPE(String value) {
            this.mdtype = value;
        }

        /**
         * Gets the value of the othermdtype property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getOTHERMDTYPE() {
            return othermdtype;
        }

        /**
         * Sets the value of the othermdtype property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setOTHERMDTYPE(String value) {
            this.othermdtype = value;
        }

        /**
         * Gets the value of the mdtypeversion property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getMDTYPEVERSION() {
            return mdtypeversion;
        }

        /**
         * Sets the value of the mdtypeversion property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setMDTYPEVERSION(String value) {
            this.mdtypeversion = value;
        }

        /**
         * <p>
         * Java class for anonymous complex type.
         * 
         * <p>
         * The following schema fragment specifies the expected content
         * contained within this class.
         * 
         * <pre>
         * &lt;complexType>
         *   &lt;complexContent>
         *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
         *       &lt;sequence>
         *         &lt;any processContents='lax' maxOccurs="unbounded"/>
         *       &lt;/sequence>
         *     &lt;/restriction>
         *   &lt;/complexContent>
         * &lt;/complexType>
         * </pre>
         * 
         * 
         */
        @XmlAccessorType(XmlAccessType.FIELD)
        @XmlType(name = "", propOrder = { "any" })
        public static class XmlData {

            @XmlAnyElement(lax = true)
            protected List<Object> any;

            /**
             * Gets the value of the any property.
             * 
             * <p>
             * This accessor method returns a reference to the live list, not a
             * snapshot. Therefore any modification you make to the returned
             * list will be present inside the JAXB object. This is why there is
             * not a <CODE>set</CODE> method for the any property.
             * 
             * <p>
             * For example, to add a new item, do as follows:
             * 
             * <pre>
             * getAny().add(newItem);
             * </pre>
             * 
             * 
             * <p>
             * Objects of the following type(s) are allowed in the list
             * {@link Object } {@link Element }
             * 
             * 
             */
            public List<Object> getAny() {
                if (any == null) {
                    any = new ArrayList<Object>();
                }
                return this.any;
            }

        }

    }

}
