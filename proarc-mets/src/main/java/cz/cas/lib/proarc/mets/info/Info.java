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

package cz.cas.lib.proarc.mets.info;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * <p>
 * Java class for anonymous complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="created" type="{http://www.w3.org/2001/XMLSchema}dateTime"/>
 *         &lt;element name="metadataversion">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;enumeration value="1.0"/>
 *               &lt;enumeration value="1.1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="packageid" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="titleid" maxOccurs="unbounded">
 *           &lt;complexType>
 *             &lt;simpleContent>
 *               &lt;extension base="&lt;http://www.w3.org/2001/XMLSchema>string">
 *                 &lt;attribute name="type" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
 *               &lt;/extension>
 *             &lt;/simpleContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;element name="collection" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="institution" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="creator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="size" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="itemlist">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;sequence>
 *                   &lt;element name="item" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded"/>
 *                 &lt;/sequence>
 *                 &lt;attribute name="itemtotal" type="{http://www.w3.org/2001/XMLSchema}integer" />
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;element name="checksum">
 *           &lt;complexType>
 *             &lt;simpleContent>
 *               &lt;extension base="&lt;http://www.w3.org/2001/XMLSchema>string">
 *                 &lt;attribute name="type" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
 *                 &lt;attribute name="checksum" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
 *               &lt;/extension>
 *             &lt;/simpleContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;element name="note" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
        "created",
        "metadataversion",
        "packageid",
        "titleid",
        "collection",
        "institution",
        "creator",
        "size",
        "itemlist",
        "checksum",
        "note"
})
@XmlRootElement(name = "info")
public class Info {

    @XmlElement(required = true)
    @XmlSchemaType(name = "dateTime")
    protected XMLGregorianCalendar created;
    @XmlElement(required = true)
    protected String metadataversion;
    @XmlElement(required = true)
    protected String packageid;
    @XmlElement(required = true)
    protected List<Info.Titleid> titleid;
    protected String collection;
    protected String institution;
    protected String creator;
    protected int size;
    @XmlElement(required = true)
    protected Info.Itemlist itemlist;
    @XmlElement(required = true)
    protected Info.Checksum checksum;
    protected String note;

    /**
     * Gets the value of the created property.
     * 
     * @return possible object is {@link XMLGregorianCalendar }
     * 
     */
    public XMLGregorianCalendar getCreated() {
        return created;
    }

    /**
     * Sets the value of the created property.
     * 
     * @param value
     *            allowed object is {@link XMLGregorianCalendar }
     * 
     */
    public void setCreated(XMLGregorianCalendar value) {
        this.created = value;
    }

    /**
     * Gets the value of the metadataversion property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getMetadataversion() {
        return metadataversion;
    }

    /**
     * Sets the value of the metadataversion property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setMetadataversion(String value) {
        this.metadataversion = value;
    }

    /**
     * Gets the value of the packageid property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getPackageid() {
        return packageid;
    }

    /**
     * Sets the value of the packageid property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setPackageid(String value) {
        this.packageid = value;
    }

    /**
     * Gets the value of the titleid property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the titleid property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getTitleid().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Info.Titleid }
     * 
     * 
     */
    public List<Info.Titleid> getTitleid() {
        if (titleid == null) {
            titleid = new ArrayList<Info.Titleid>();
        }
        return this.titleid;
    }

    /**
     * Gets the value of the collection property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getCollection() {
        return collection;
    }

    /**
     * Sets the value of the collection property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setCollection(String value) {
        this.collection = value;
    }

    /**
     * Gets the value of the institution property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getInstitution() {
        return institution;
    }

    /**
     * Sets the value of the institution property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setInstitution(String value) {
        this.institution = value;
    }

    /**
     * Gets the value of the creator property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getCreator() {
        return creator;
    }

    /**
     * Sets the value of the creator property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setCreator(String value) {
        this.creator = value;
    }

    /**
     * Gets the value of the size property.
     * 
     */
    public int getSize() {
        return size;
    }

    /**
     * Sets the value of the size property.
     * 
     */
    public void setSize(int value) {
        this.size = value;
    }

    /**
     * Gets the value of the itemlist property.
     * 
     * @return possible object is {@link Info.Itemlist }
     * 
     */
    public Info.Itemlist getItemlist() {
        return itemlist;
    }

    /**
     * Sets the value of the itemlist property.
     * 
     * @param value
     *            allowed object is {@link Info.Itemlist }
     * 
     */
    public void setItemlist(Info.Itemlist value) {
        this.itemlist = value;
    }

    /**
     * Gets the value of the checksum property.
     * 
     * @return possible object is {@link Info.Checksum }
     * 
     */
    public Info.Checksum getChecksum() {
        return checksum;
    }

    /**
     * Sets the value of the checksum property.
     * 
     * @param value
     *            allowed object is {@link Info.Checksum }
     * 
     */
    public void setChecksum(Info.Checksum value) {
        this.checksum = value;
    }

    /**
     * Gets the value of the note property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getNote() {
        return note;
    }

    /**
     * Sets the value of the note property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setNote(String value) {
        this.note = value;
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
     *   &lt;simpleContent>
     *     &lt;extension base="&lt;http://www.w3.org/2001/XMLSchema>string">
     *       &lt;attribute name="type" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
     *       &lt;attribute name="checksum" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
     *     &lt;/extension>
     *   &lt;/simpleContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = {
            "value"
    })
    public static class Checksum {

        @XmlValue
        protected String value;
        @XmlAttribute(name = "type", required = true)
        protected String type;
        @XmlAttribute(name = "checksum", required = true)
        protected String checksum;

        /**
         * Gets the value of the value property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getValue() {
            return value;
        }

        /**
         * Sets the value of the value property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setValue(String value) {
            this.value = value;
        }

        /**
         * Gets the value of the type property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getType() {
            return type;
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
         * Gets the value of the checksum property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getChecksum() {
            return checksum;
        }

        /**
         * Sets the value of the checksum property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setChecksum(String value) {
            this.checksum = value;
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
     *       &lt;sequence>
     *         &lt;element name="item" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded"/>
     *       &lt;/sequence>
     *       &lt;attribute name="itemtotal" type="{http://www.w3.org/2001/XMLSchema}integer" />
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = {
            "item"
    })
    public static class Itemlist {

        @XmlElement(required = true)
        protected List<String> item;
        @XmlAttribute(name = "itemtotal")
        protected BigInteger itemtotal;

        /**
         * Gets the value of the item property.
         * 
         * <p>
         * This accessor method returns a reference to the live list, not a
         * snapshot. Therefore any modification you make to the returned list
         * will be present inside the JAXB object. This is why there is not a
         * <CODE>set</CODE> method for the item property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * 
         * <pre>
         * getItem().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link String }
         * 
         * 
         */
        public List<String> getItem() {
            if (item == null) {
                item = new ArrayList<String>();
            }
            return this.item;
        }

        /**
         * Gets the value of the itemtotal property.
         * 
         * @return possible object is {@link BigInteger }
         * 
         */
        public BigInteger getItemtotal() {
            return itemtotal;
        }

        /**
         * Sets the value of the itemtotal property.
         * 
         * @param value
         *            allowed object is {@link BigInteger }
         * 
         */
        public void setItemtotal(BigInteger value) {
            this.itemtotal = value;
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
     *   &lt;simpleContent>
     *     &lt;extension base="&lt;http://www.w3.org/2001/XMLSchema>string">
     *       &lt;attribute name="type" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
     *     &lt;/extension>
     *   &lt;/simpleContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = {
            "value"
    })
    public static class Titleid {

        @XmlValue
        protected String value;
        @XmlAttribute(name = "type", required = true)
        protected String type;

        /**
         * Gets the value of the value property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getValue() {
            return value;
        }

        /**
         * Sets the value of the value property.
         * 
         * @param value
         *            allowed object is {@link String }
         * 
         */
        public void setValue(String value) {
            this.value = value;
        }

        /**
         * Gets the value of the type property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getType() {
            return type;
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

    }

}
