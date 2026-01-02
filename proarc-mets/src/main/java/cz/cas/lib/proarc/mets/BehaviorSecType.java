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
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * behaviorSecType: Complex Type for Behavior Sections Behaviors are executable
 * code which can be associated with parts of a METS object. The behaviorSec
 * element is used to group individual behaviors within a hierarchical
 * structure. Such grouping can be useful to organize families of behaviors
 * together or to indicate other relationships between particular behaviors.
 *
 *
 * <p>
 * Java class for behaviorSecType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 *
 * <pre>
 * &lt;complexType name="behaviorSecType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="behaviorSec" type="{http://www.loc.gov/METS/}behaviorSecType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="behavior" type="{http://www.loc.gov/METS/}behaviorType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *       &lt;attribute name="CREATED" type="{http://www.w3.org/2001/XMLSchema}dateTime" />
 *       &lt;attribute name="LABEL" type="{http://www.w3.org/2001/XMLSchema}string" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "behaviorSecType", namespace = "http://www.loc.gov/METS/", propOrder = {"behaviorSec", "behavior"})
public class BehaviorSecType {

    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<BehaviorSecType> behaviorSec;
    @XmlElement(namespace = "http://www.loc.gov/METS/")
    protected List<BehaviorType> behavior;
    @XmlAttribute(name = "ID")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;
    @XmlAttribute(name = "CREATED")
    @XmlSchemaType(name = "dateTime")
    protected XMLGregorianCalendar created;
    @XmlAttribute(name = "LABEL")
    protected String label4;

    /**
     * Gets the value of the behaviorSec property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the behaviorSec property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getBehaviorSec().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BehaviorSecType }
     *
     *
     */
    public List<BehaviorSecType> getBehaviorSec() {
        if (behaviorSec == null) {
            behaviorSec = new ArrayList<BehaviorSecType>();
        }
        return this.behaviorSec;
    }

    /**
     * Gets the value of the behavior property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the behavior property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getBehavior().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BehaviorType }
     *
     *
     */
    public List<BehaviorType> getBehavior() {
        if (behavior == null) {
            behavior = new ArrayList<BehaviorType>();
        }
        return this.behavior;
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
     * @param value allowed object is {@link XMLGregorianCalendar }
     *
     */
    public void setCREATED(XMLGregorianCalendar value) {
        this.created = value;
    }

    /**
     * Gets the value of the label4 property.
     *
     * @return possible object is {@link String }
     *
     */
    public String getLabel4() {
        return label4;
    }

    /**
     * Sets the value of the label4 property.
     *
     * @param value allowed object is {@link String }
     *
     */
    public void setLabel4(String value) {
        this.label4 = value;
    }

}
