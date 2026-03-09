package cz.cas.lib.proarc.foxml.management;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import javax.xml.datatype.XMLGregorianCalendar;


/**
 * <p>Java class for datastreamProfileType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="datastreamProfileType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="dsLabel" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="dsVersionID" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="dsCreateDate" type="{http://www.w3.org/2001/XMLSchema}dateTime"/>
 *         &lt;element name="dsState">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;enumeration value="A"/>
 *               &lt;enumeration value="D"/>
 *               &lt;enumeration value="I"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="dsMIME" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="dsFormatURI" type="{http://www.w3.org/2001/XMLSchema}anyURI"/>
 *         &lt;element name="dsControlGroup">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;enumeration value="X"/>
 *               &lt;enumeration value="M"/>
 *               &lt;enumeration value="R"/>
 *               &lt;enumeration value="E"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="dsSize" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *         &lt;element name="dsVersionable">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;enumeration value="true"/>
 *               &lt;enumeration value="false"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="dsInfoType" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="dsLocation" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="dsLocationType" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="dsChecksumType">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;enumeration value="DEFAULT"/>
 *               &lt;enumeration value="DISABLED"/>
 *               &lt;enumeration value="MD5"/>
 *               &lt;enumeration value="SHA-1"/>
 *               &lt;enumeration value="SHA-256"/>
 *               &lt;enumeration value="SHA-385"/>
 *               &lt;enumeration value="SHA-512"/>
 *               &lt;enumeration value="HAVAL"/>
 *               &lt;enumeration value="TIGER"/>
 *               &lt;enumeration value="WHIRLPOOL"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="dsChecksum" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="dsChecksumValid" minOccurs="0">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;enumeration value="true"/>
 *               &lt;enumeration value="false"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="dsAltID" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded" minOccurs="0"/>
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
 *       &lt;attribute name="dateTime">
 *         &lt;simpleType>
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}dateTime">
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
@XmlType(name = "datastreamProfileType", propOrder = {
    "dsLabel",
    "dsVersionID",
    "dsCreateDate",
    "dsState",
    "dsMIME",
    "dsFormatURI",
    "dsControlGroup",
    "dsSize",
    "dsVersionable",
    "dsInfoType",
    "dsLocation",
    "dsLocationType",
    "dsChecksumType",
    "dsChecksum",
    "dsChecksumValid",
    "dsAltID"
})
@XmlRootElement(name = "datastreamProfile")
public class DatastreamProfile {

    @XmlElement(required = true)
    protected String dsLabel;
    @XmlElement(required = true)
    protected String dsVersionID;
    @XmlElement(required = true)
    @XmlSchemaType(name = "dateTime")
    protected XMLGregorianCalendar dsCreateDate;
    @XmlElement(required = true)
    protected String dsState;
    @XmlElement(required = true)
    protected String dsMIME;
    @XmlElement(required = true)
    @XmlSchemaType(name = "anyURI")
    protected String dsFormatURI;
    @XmlElement(required = true)
    protected String dsControlGroup;
    @XmlElement(required = true)
    protected BigInteger dsSize;
    @XmlElement(required = true)
    protected String dsVersionable;
    @XmlElement(required = true)
    protected String dsInfoType;
    @XmlElement(required = true)
    protected String dsLocation;
    @XmlElement(required = true)
    protected String dsLocationType;
    @XmlElement(required = true)
    protected String dsChecksumType;
    @XmlElement(required = true)
    protected String dsChecksum;
    protected String dsChecksumValid;
    protected List<String> dsAltID;
    @XmlAttribute(name = "pid", required = true)
    protected String pid;
    @XmlAttribute(name = "dsID", required = true)
    protected String dsID;
    @XmlAttribute(name = "dateTime")
    protected XMLGregorianCalendar dateTime;

    /**
     * Gets the value of the dsLabel property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsLabel() {
        return dsLabel;
    }

    /**
     * Sets the value of the dsLabel property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsLabel(String value) {
        this.dsLabel = value;
    }

    /**
     * Gets the value of the dsVersionID property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsVersionID() {
        return dsVersionID;
    }

    /**
     * Sets the value of the dsVersionID property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsVersionID(String value) {
        this.dsVersionID = value;
    }

    /**
     * Gets the value of the dsCreateDate property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getDsCreateDate() {
        return dsCreateDate;
    }

    /**
     * Sets the value of the dsCreateDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setDsCreateDate(XMLGregorianCalendar value) {
        this.dsCreateDate = value;
    }

    /**
     * Gets the value of the dsState property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsState() {
        return dsState;
    }

    /**
     * Sets the value of the dsState property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsState(String value) {
        this.dsState = value;
    }

    /**
     * Gets the value of the dsMIME property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsMIME() {
        return dsMIME;
    }

    /**
     * Sets the value of the dsMIME property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsMIME(String value) {
        this.dsMIME = value;
    }

    /**
     * Gets the value of the dsFormatURI property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsFormatURI() {
        return dsFormatURI;
    }

    /**
     * Sets the value of the dsFormatURI property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsFormatURI(String value) {
        this.dsFormatURI = value;
    }

    /**
     * Gets the value of the dsControlGroup property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsControlGroup() {
        return dsControlGroup;
    }

    /**
     * Sets the value of the dsControlGroup property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsControlGroup(String value) {
        this.dsControlGroup = value;
    }

    /**
     * Gets the value of the dsSize property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getDsSize() {
        return dsSize;
    }

    /**
     * Sets the value of the dsSize property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setDsSize(BigInteger value) {
        this.dsSize = value;
    }

    /**
     * Gets the value of the dsVersionable property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsVersionable() {
        return dsVersionable;
    }

    /**
     * Sets the value of the dsVersionable property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsVersionable(String value) {
        this.dsVersionable = value;
    }

    /**
     * Gets the value of the dsInfoType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsInfoType() {
        return dsInfoType;
    }

    /**
     * Sets the value of the dsInfoType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsInfoType(String value) {
        this.dsInfoType = value;
    }

    /**
     * Gets the value of the dsLocation property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsLocation() {
        return dsLocation;
    }

    /**
     * Sets the value of the dsLocation property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsLocation(String value) {
        this.dsLocation = value;
    }

    /**
     * Gets the value of the dsLocationType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsLocationType() {
        return dsLocationType;
    }

    /**
     * Sets the value of the dsLocationType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsLocationType(String value) {
        this.dsLocationType = value;
    }

    /**
     * Gets the value of the dsChecksumType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsChecksumType() {
        return dsChecksumType;
    }

    /**
     * Sets the value of the dsChecksumType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsChecksumType(String value) {
        this.dsChecksumType = value;
    }

    /**
     * Gets the value of the dsChecksum property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsChecksum() {
        return dsChecksum;
    }

    /**
     * Sets the value of the dsChecksum property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsChecksum(String value) {
        this.dsChecksum = value;
    }

    /**
     * Gets the value of the dsChecksumValid property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDsChecksumValid() {
        return dsChecksumValid;
    }

    /**
     * Sets the value of the dsChecksumValid property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDsChecksumValid(String value) {
        this.dsChecksumValid = value;
    }

    /**
     * Gets the value of the dsAltID property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the dsAltID property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getDsAltID().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getDsAltID() {
        if (dsAltID == null) {
            dsAltID = new ArrayList<String>();
        }
        return this.dsAltID;
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

    /**
     * Gets the value of the dateTime property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getDateTime() {
        return dateTime;
    }

    /**
     * Sets the value of the dateTime property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setDateTime(XMLGregorianCalendar value) {
        this.dateTime = value;
    }

}
