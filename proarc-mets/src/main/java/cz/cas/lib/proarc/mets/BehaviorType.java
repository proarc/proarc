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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * behaviorType: Complex Type for Behaviors A behavior can be used to associate
 * executable behaviors with content in the METS object. A behavior element has
 * an interface definition element that represents an abstract definition of the
 * set of behaviors represented by a particular behavior. A behavior element
 * also has an behavior mechanism which is a module of executable code that
 * implements and runs the behavior defined abstractly by the interface
 * definition.
 * 
 * 
 * <p>
 * Java class for behaviorType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="behaviorType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="interfaceDef" type="{http://www.loc.gov/METS/}objectType" minOccurs="0"/>
 *         &lt;element name="mechanism" type="{http://www.loc.gov/METS/}objectType"/>
 *       &lt;/sequence>
 *       &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *       &lt;attribute name="STRUCTID" type="{http://www.w3.org/2001/XMLSchema}IDREFS" />
 *       &lt;attribute name="BTYPE" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="CREATED" type="{http://www.w3.org/2001/XMLSchema}dateTime" />
 *       &lt;attribute name="LABEL" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="GROUPID" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="ADMID" type="{http://www.w3.org/2001/XMLSchema}IDREFS" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "behaviorType", namespace = "http://www.loc.gov/METS/", propOrder = { "interfaceDef", "mechanism" })
public class BehaviorType {

    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected ObjectType interfaceDef;
    @XmlElement(namespace = "http://www.loc.gov/METS/", required = true)
    protected ObjectType mechanism;
    @XmlAttribute(name = "ID")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;
    @XmlAttribute(name = "STRUCTID")
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Object> structid;
    @XmlAttribute(name = "BTYPE")
    protected String btype;
    @XmlAttribute(name = "CREATED")
    @XmlSchemaType(name = "dateTime")
    protected XMLGregorianCalendar created;
    @XmlAttribute(name = "LABEL")
    protected String label5;
    @XmlAttribute(name = "GROUPID")
    protected String groupid;
    @XmlAttribute(name = "ADMID")
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Object> admid;

    /**
     * Gets the value of the interfaceDef property.
     * 
     * @return possible object is {@link ObjectType }
     * 
     */
    public ObjectType getInterfaceDef() {
        return interfaceDef;
    }

    /**
     * Sets the value of the interfaceDef property.
     * 
     * @param value
     *            allowed object is {@link ObjectType }
     * 
     */
    public void setInterfaceDef(ObjectType value) {
        this.interfaceDef = value;
    }

    /**
     * Gets the value of the mechanism property.
     * 
     * @return possible object is {@link ObjectType }
     * 
     */
    public ObjectType getMechanism() {
        return mechanism;
    }

    /**
     * Sets the value of the mechanism property.
     * 
     * @param value
     *            allowed object is {@link ObjectType }
     * 
     */
    public void setMechanism(ObjectType value) {
        this.mechanism = value;
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
     * Gets the value of the structid property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the structid property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getSTRUCTID().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list {@link Object }
     * 
     * 
     */
    public List<Object> getSTRUCTID() {
        if (structid == null) {
            structid = new ArrayList<Object>();
        }
        return this.structid;
    }

    /**
     * Gets the value of the btype property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getBTYPE() {
        return btype;
    }

    /**
     * Sets the value of the btype property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setBTYPE(String value) {
        this.btype = value;
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
     * Gets the value of the label5 property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getLabel5() {
        return label5;
    }

    /**
     * Sets the value of the label5 property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setLabel5(String value) {
        this.label5 = value;
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

}
