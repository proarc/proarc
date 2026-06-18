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
import jakarta.xml.bind.annotation.XmlSeeAlso;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.CollapsedStringAdapter;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.ArrayList;
import java.util.List;
import javax.xml.datatype.XMLGregorianCalendar;


/**
 * fileGrpType: Complex Type for File Groups The file group is used to cluster
 * all of the digital files composing a digital library object in a hierarchical
 * arrangement (fileGrp is recursively defined to enable the creation of the
 * hierarchy). Any file group may contain zero or more file elements. File
 * elements in turn can contain one or more FLocat elements (a pointer to a file
 * containing content for this object) and/or a FContent element (the contents
 * of the file, in either XML or Base64 encoding).
 *
 *
 * <p>
 * Java class for fileGrpType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 *
 * <pre>
 * &lt;complexType name="fileGrpType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;choice>
 *         &lt;element name="fileGrp" type="{http://www.loc.gov/METS/}fileGrpType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="file" type="{http://www.loc.gov/METS/}fileType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/choice>
 *       &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *       &lt;attribute name="VERSDATE" type="{http://www.w3.org/2001/XMLSchema}dateTime" />
 *       &lt;attribute name="ADMID" type="{http://www.w3.org/2001/XMLSchema}IDREFS" />
 *       &lt;attribute name="USE" type="{http://www.w3.org/2001/XMLSchema}string" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "fileGrpType", namespace = "http://www.loc.gov/METS/", propOrder = {"fileGrp", "file"})
@XmlSeeAlso({cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp.class})
public class FileGrpType {

    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<FileGrpType> fileGrp;
    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<FileType> file;
    @XmlAttribute(name = "ID")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;
    @XmlAttribute(name = "VERSDATE")
    @XmlSchemaType(name = "dateTime")
    protected XMLGregorianCalendar versdate;
    @XmlAttribute(name = "ADMID")
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Object> admid;
    @XmlAttribute(name = "USE")
    protected String use;

    /**
     * Gets the value of the fileGrp property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the fileGrp property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getFileGrp().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link FileGrpType }
     *
     *
     */
    public List<FileGrpType> getFileGrp() {
        if (fileGrp == null) {
            fileGrp = new ArrayList<FileGrpType>();
        }
        return this.fileGrp;
    }

    /**
     * Gets the value of the file property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the file property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getFile().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list {@link FileType }
     *
     *
     */
    public List<FileType> getFile() {
        if (file == null) {
            file = new ArrayList<FileType>();
        }
        return this.file;
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
     * Gets the value of the versdate property.
     *
     * @return possible object is {@link XMLGregorianCalendar }
     *
     */
    public XMLGregorianCalendar getVERSDATE() {
        return versdate;
    }

    /**
     * Sets the value of the versdate property.
     *
     * @param value allowed object is {@link XMLGregorianCalendar }
     *
     */
    public void setVERSDATE(XMLGregorianCalendar value) {
        this.versdate = value;
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
     * Gets the value of the use property.
     *
     * @return possible object is {@link String }
     *
     */
    public String getUSE() {
        return use;
    }

    /**
     * Sets the value of the use property.
     *
     * @param value allowed object is {@link String }
     *
     */
    public void setUSE(String value) {
        this.use = value;
    }

}
