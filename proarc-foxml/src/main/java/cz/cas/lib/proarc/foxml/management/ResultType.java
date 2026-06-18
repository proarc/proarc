package cz.cas.lib.proarc.foxml.management;

import jakarta.xml.bind.JAXBElement;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlSeeAlso;
import jakarta.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>Java class for resultType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="resultType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="listSession" type="{http://www.fedora.info/definitions/1/0/types/}listSessionType" minOccurs="0"/>
 *         &lt;element name="resultList">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;sequence>
 *                   &lt;element name="objectFields" type="{http://www.fedora.info/definitions/1/0/types/}objectFieldsType" maxOccurs="unbounded" minOccurs="0"/>
 *                 &lt;/sequence>
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "resultType", namespace = "http://www.fedora.info/definitions/1/0/types/", propOrder = {
    "listSession",
    "resultList"
})
@XmlSeeAlso({
    Result.class
})
public class ResultType {

    @XmlElementRef(name = "listSession", namespace = "http://www.fedora.info/definitions/1/0/types/", type = JAXBElement.class)
    protected JAXBElement<ListSessionType> listSession;
    @XmlElement(required = true)
    protected ResultType.ResultList resultList;

    /**
     * Gets the value of the listSession property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link ListSessionType }{@code >}
     *     
     */
    public JAXBElement<ListSessionType> getListSession() {
        return listSession;
    }

    /**
     * Sets the value of the listSession property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link ListSessionType }{@code >}
     *     
     */
    public void setListSession(JAXBElement<ListSessionType> value) {
        this.listSession = value;
    }

    /**
     * Gets the value of the resultList property.
     * 
     * @return
     *     possible object is
     *     {@link ResultList }
     *     
     */
    public ResultList getResultList() {
        return resultList;
    }

    /**
     * Sets the value of the resultList property.
     * 
     * @param value
     *     allowed object is
     *     {@link ResultList }
     *     
     */
    public void setResultList(ResultList value) {
        this.resultList = value;
    }


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
     *         &lt;element name="objectFields" type="{http://www.fedora.info/definitions/1/0/types/}objectFieldsType" maxOccurs="unbounded" minOccurs="0"/>
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
        "objectFields"
    })
    public static class ResultList {

        @XmlElement(namespace = "http://www.fedora.info/definitions/1/0/types/")
        protected List<ObjectFieldsType> objectFields;

        /**
         * Gets the value of the objectFields property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the objectFields property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getObjectFields().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link ObjectFieldsType }
         * 
         * 
         */
        public List<ObjectFieldsType> getObjectFields() {
            if (objectFields == null) {
                objectFields = new ArrayList<ObjectFieldsType>();
            }
            return this.objectFields;
        }

    }

}
