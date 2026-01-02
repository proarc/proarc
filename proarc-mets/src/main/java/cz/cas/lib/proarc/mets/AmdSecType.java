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
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.CollapsedStringAdapter;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.ArrayList;
import java.util.List;

/**
 * amdSecType: Complex Type for Administrative Metadata Sections The
 * administrative metadata section consists of four possible subsidiary
 * sections: techMD (technical metadata for text/image/audio/video files),
 * rightsMD (intellectual property rights metadata), sourceMD (analog/digital
 * source metadata), and digiprovMD (digital provenance metadata, that is, the
 * history of migrations/translations performed on a digital library object from
 * it's original digital capture/encoding).
 *
 *
 * <p>
 * Java class for amdSecType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 *
 * <pre>
 * &lt;complexType name="amdSecType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="techMD" type="{http://www.loc.gov/METS/}mdSecType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="rightsMD" type="{http://www.loc.gov/METS/}mdSecType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="sourceMD" type="{http://www.loc.gov/METS/}mdSecType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="digiprovMD" type="{http://www.loc.gov/METS/}mdSecType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "amdSecType", namespace = "http://www.loc.gov/METS/", propOrder = {"techMD", "rightsMD", "sourceMD", "digiprovMD"})
public class AmdSecType {

    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<MdSecType> techMD;
    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<MdSecType> rightsMD;
    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<MdSecType> sourceMD;
    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<MdSecType> digiprovMD;
    @XmlAttribute(name = "ID")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;

    /**
     * Gets the value of the techMD property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the techMD property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getTechMD().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MdSecType }
     *
     *
     */
    public List<MdSecType> getTechMD() {
        if (techMD == null) {
            techMD = new ArrayList<MdSecType>();
        }
        return this.techMD;
    }

    /**
     * Gets the value of the rightsMD property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the rightsMD property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getRightsMD().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MdSecType }
     *
     *
     */
    public List<MdSecType> getRightsMD() {
        if (rightsMD == null) {
            rightsMD = new ArrayList<MdSecType>();
        }
        return this.rightsMD;
    }

    /**
     * Gets the value of the sourceMD property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the sourceMD property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getSourceMD().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MdSecType }
     *
     *
     */
    public List<MdSecType> getSourceMD() {
        if (sourceMD == null) {
            sourceMD = new ArrayList<MdSecType>();
        }
        return this.sourceMD;
    }

    /**
     * Gets the value of the digiprovMD property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the digiprovMD property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getDigiprovMD().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MdSecType }
     *
     *
     */
    public List<MdSecType> getDigiprovMD() {
        if (digiprovMD == null) {
            digiprovMD = new ArrayList<MdSecType>();
        }
        return this.digiprovMD;
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

}
