package cz.cas.lib.proarc.foxml.management;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;


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
 *         &lt;element ref="{http://www.fedora.info/definitions/1/0/management/}asOfDateTime" minOccurs="0"/>
 *         &lt;element ref="{http://www.fedora.info/definitions/1/0/management/}contentModels"/>
 *         &lt;element ref="{http://www.fedora.info/definitions/1/0/management/}problems"/>
 *         &lt;element ref="{http://www.fedora.info/definitions/1/0/management/}datastreamProblems"/>
 *       &lt;/sequence>
 *       &lt;attribute name="pid" use="required">
 *         &lt;simpleType>
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *           &lt;/restriction>
 *         &lt;/simpleType>
 *       &lt;/attribute>
 *       &lt;attribute name="valid" use="required">
 *         &lt;simpleType>
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}boolean">
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
    "asOfDateTime",
    "contentModels",
    "problems",
    "datastreamProblems"
})
@XmlRootElement(name = "validation")
public class Validation {

    protected XMLGregorianCalendar asOfDateTime;
    @XmlElement(required = true)
    protected ContentModels contentModels;
    @XmlElement(required = true)
    protected Problems problems;
    @XmlElement(required = true)
    protected DatastreamProblems datastreamProblems;
    @XmlAttribute(name = "pid", required = true)
    protected String pid;
    @XmlAttribute(name = "valid", required = true)
    protected boolean valid;

    /**
     * Gets the value of the asOfDateTime property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getAsOfDateTime() {
        return asOfDateTime;
    }

    /**
     * Sets the value of the asOfDateTime property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setAsOfDateTime(XMLGregorianCalendar value) {
        this.asOfDateTime = value;
    }

    /**
     * Gets the value of the contentModels property.
     * 
     * @return
     *     possible object is
     *     {@link ContentModels }
     *     
     */
    public ContentModels getContentModels() {
        return contentModels;
    }

    /**
     * Sets the value of the contentModels property.
     * 
     * @param value
     *     allowed object is
     *     {@link ContentModels }
     *     
     */
    public void setContentModels(ContentModels value) {
        this.contentModels = value;
    }

    /**
     * Gets the value of the problems property.
     * 
     * @return
     *     possible object is
     *     {@link Problems }
     *     
     */
    public Problems getProblems() {
        return problems;
    }

    /**
     * Sets the value of the problems property.
     * 
     * @param value
     *     allowed object is
     *     {@link Problems }
     *     
     */
    public void setProblems(Problems value) {
        this.problems = value;
    }

    /**
     * Gets the value of the datastreamProblems property.
     * 
     * @return
     *     possible object is
     *     {@link DatastreamProblems }
     *     
     */
    public DatastreamProblems getDatastreamProblems() {
        return datastreamProblems;
    }

    /**
     * Sets the value of the datastreamProblems property.
     * 
     * @param value
     *     allowed object is
     *     {@link DatastreamProblems }
     *     
     */
    public void setDatastreamProblems(DatastreamProblems value) {
        this.datastreamProblems = value;
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
     * Gets the value of the valid property.
     * 
     */
    public boolean isValid() {
        return valid;
    }

    /**
     * Sets the value of the valid property.
     * 
     */
    public void setValid(boolean value) {
        this.valid = value;
    }

}
