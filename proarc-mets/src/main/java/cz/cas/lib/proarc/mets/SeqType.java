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
import jakarta.xml.bind.annotation.XmlElements;
import jakarta.xml.bind.annotation.XmlID;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.CollapsedStringAdapter;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.ArrayList;
import java.util.List;

/**
 * seqType: Complex Type for Sequences of Files The seq element should be used
 * to link a div to a set of content files when those files should be
 * played/displayed sequentially to deliver content to a user. Individual <area>
 * subelements within the seq element provide the links to the files or portions
 * thereof.
 *
 *
 * <p>
 * Java class for seqType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 *
 * <pre>
 * &lt;complexType name="seqType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;choice maxOccurs="unbounded">
 *         &lt;element name="area" type="{http://www.loc.gov/METS/}areaType" minOccurs="0"/>
 *         &lt;element name="par" type="{http://www.loc.gov/METS/}parType" minOccurs="0"/>
 *       &lt;/choice>
 *       &lt;attribute name="ID" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "seqType", namespace = "http://www.loc.gov/METS/", propOrder = {
        "areaOrPar"
})
public class SeqType {

    @XmlElements({
            @XmlElement(name = "area", namespace = "http://www.loc.gov/METS/", type = AreaType.class),
            @XmlElement(name = "par", namespace = "http://www.loc.gov/METS/", type = ParType.class)
    })
    protected List<Object> areaOrPar;
    @XmlAttribute(name = "ID")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;

    /**
     * Gets the value of the areaOrPar property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the areaOrPar property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getAreaOrPar().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list {@link AreaType }
     * {@link ParType }
     *
     *
     */
    public List<Object> getAreaOrPar() {
        if (areaOrPar == null) {
            areaOrPar = new ArrayList<Object>();
        }
        return this.areaOrPar;
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
