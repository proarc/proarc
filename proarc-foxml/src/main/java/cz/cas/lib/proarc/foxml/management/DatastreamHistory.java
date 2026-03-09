package cz.cas.lib.proarc.foxml.management;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://www.fedora.info/definitions/1/0/management/}datastreamProfile" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *       &lt;attribute name="pid" use="required">
 *         &lt;simpleType>
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *           &lt;/restriction>
 *         &lt;/simpleType>
 *       &lt;/attribute>
 *       &lt;attribute name="dsID" use="required">
 *         &lt;simpleType>
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *           &lt;/restriction>
 *         &lt;/simpleType>
 *       &lt;/attribute>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "datastreamProfile"
})
@XmlRootElement(name = "datastreamHistory")
public class DatastreamHistory {

    @XmlElement(required = true)
    protected List<DatastreamProfile> datastreamProfile;
    @XmlAttribute(name = "pid", required = true)
    protected String pid;
    @XmlAttribute(name = "dsID", required = true)
    protected String dsID;

    /**
     * Gets the value of the datastreamProfile property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the datastreamProfile property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getDatastreamProfile().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link DatastreamProfile }
     * 
     * 
     */
    public List<DatastreamProfile> getDatastreamProfile() {
        if (datastreamProfile == null) {
            datastreamProfile = new ArrayList<DatastreamProfile>();
        }
        return this.datastreamProfile;
    }

    /**
     * Gets the value of the pid property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPid() {
        return pid;
    }

    /**
     * Sets the value of the pid property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPid(String value) {
        this.pid = value;
    }

    /**
     * Gets the value of the dsID property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsID() {
        return dsID;
    }

    /**
     * Sets the value of the dsID property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsID(String value) {
        this.dsID = value;
    }

}
