package cz.cas.lib.proarc.foxml.management;

import jakarta.xml.bind.JAXBElement;
import jakarta.xml.bind.annotation.XmlElementDecl;
import jakarta.xml.bind.annotation.XmlRegistry;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.yourmediashelf.fedora.generated.management package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Pid_QNAME = new QName("http://www.fedora.info/definitions/1/0/management/", "pid");
    private final static QName _DatastreamProfile_QNAME = new QName("http://www.fedora.info/definitions/1/0/management/", "datastreamProfile");
    private final static QName _AsOfDateTime_QNAME = new QName("http://www.fedora.info/definitions/1/0/management/", "asOfDateTime");
    private final static QName _ListSessionTypeExpirationDate_QNAME = new QName("http://www.fedora.info/definitions/1/0/types/", "expirationDate");
    private final static QName _ObjectFieldsTypeOwnerId_QNAME = new QName("http://www.fedora.info/definitions/1/0/types/", "ownerId");
    private final static QName _ObjectFieldsTypeCDate_QNAME = new QName("http://www.fedora.info/definitions/1/0/types/", "cDate");
    private final static QName _ObjectFieldsTypeDcmDate_QNAME = new QName("http://www.fedora.info/definitions/1/0/types/", "dcmDate");
    private final static QName _ObjectFieldsTypeMDate_QNAME = new QName("http://www.fedora.info/definitions/1/0/types/", "mDate");
    private final static QName _ObjectFieldsTypeLabel_QNAME = new QName("http://www.fedora.info/definitions/1/0/types/", "label");
    private final static QName _ObjectFieldsTypePid_QNAME = new QName("http://www.fedora.info/definitions/1/0/types/", "pid");
    private final static QName _ObjectFieldsTypeState_QNAME = new QName("http://www.fedora.info/definitions/1/0/types/", "state");
    private final static QName _ResultTypeListSession_QNAME = new QName("http://www.fedora.info/definitions/1/0/types/", "listSession");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.yourmediashelf.fedora.generated.management
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ResultType }
     * 
     */
    public ResultType createResultType() {
        return new ResultType();
    }

    /**
     * Create an instance of {@link DatastreamProblems }
     * 
     */
    public DatastreamProblems createDatastreamProblems() {
        return new DatastreamProblems();
    }

    /**
     * Create an instance of {@link Datastream }
     * 
     */
    public Datastream createDatastream() {
        return new Datastream();
    }

    /**
     * Create an instance of {@link Problems }
     * 
     */
    public Problems createProblems() {
        return new Problems();
    }

    /**
     * Create an instance of {@link Validation }
     * 
     */
    public Validation createValidation() {
        return new Validation();
    }

    /**
     * Create an instance of {@link ContentModels }
     * 
     */
    public ContentModels createContentModels() {
        return new ContentModels();
    }

    /**
     * Create an instance of {@link PidList }
     * 
     */
    public PidList createPidList() {
        return new PidList();
    }

    /**
     * Create an instance of {@link DatastreamHistory }
     * 
     */
    public DatastreamHistory createDatastreamHistory() {
        return new DatastreamHistory();
    }

    /**
     * Create an instance of {@link DatastreamProfile }
     * 
     */
    public DatastreamProfile createDatastreamProfile() {
        return new DatastreamProfile();
    }

    /**
     * Create an instance of {@link Result }
     * 
     */
    public Result createResult() {
        return new Result();
    }

    /**
     * Create an instance of {@link ListSessionType }
     * 
     */
    public ListSessionType createListSessionType() {
        return new ListSessionType();
    }

    /**
     * Create an instance of {@link ResultType.ResultList }
     * 
     */
    public ResultType.ResultList createResultTypeResultList() {
        return new ResultType.ResultList();
    }

    /**
     * Create an instance of {@link ObjectFieldsType }
     * 
     */
    public ObjectFieldsType createObjectFieldsType() {
        return new ObjectFieldsType();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/management/", name = "pid")
    public JAXBElement<String> createPid(String value) {
        return new JAXBElement<String>(_Pid_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link DatastreamProfile }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/management/", name = "datastreamProfile")
    public JAXBElement<DatastreamProfile> createDatastreamProfile(DatastreamProfile value) {
        return new JAXBElement<DatastreamProfile>(_DatastreamProfile_QNAME, DatastreamProfile.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link XMLGregorianCalendar }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/management/", name = "asOfDateTime")
    public JAXBElement<XMLGregorianCalendar> createAsOfDateTime(XMLGregorianCalendar value) {
        return new JAXBElement<XMLGregorianCalendar>(_AsOfDateTime_QNAME, XMLGregorianCalendar.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/types/", name = "expirationDate", scope = ListSessionType.class)
    public JAXBElement<String> createListSessionTypeExpirationDate(String value) {
        return new JAXBElement<String>(_ListSessionTypeExpirationDate_QNAME, String.class, ListSessionType.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/types/", name = "ownerId", scope = ObjectFieldsType.class)
    public JAXBElement<String> createObjectFieldsTypeOwnerId(String value) {
        return new JAXBElement<String>(_ObjectFieldsTypeOwnerId_QNAME, String.class, ObjectFieldsType.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/types/", name = "cDate", scope = ObjectFieldsType.class)
    public JAXBElement<String> createObjectFieldsTypeCDate(String value) {
        return new JAXBElement<String>(_ObjectFieldsTypeCDate_QNAME, String.class, ObjectFieldsType.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/types/", name = "dcmDate", scope = ObjectFieldsType.class)
    public JAXBElement<String> createObjectFieldsTypeDcmDate(String value) {
        return new JAXBElement<String>(_ObjectFieldsTypeDcmDate_QNAME, String.class, ObjectFieldsType.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/types/", name = "mDate", scope = ObjectFieldsType.class)
    public JAXBElement<String> createObjectFieldsTypeMDate(String value) {
        return new JAXBElement<String>(_ObjectFieldsTypeMDate_QNAME, String.class, ObjectFieldsType.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/types/", name = "label", scope = ObjectFieldsType.class)
    public JAXBElement<String> createObjectFieldsTypeLabel(String value) {
        return new JAXBElement<String>(_ObjectFieldsTypeLabel_QNAME, String.class, ObjectFieldsType.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/types/", name = "pid", scope = ObjectFieldsType.class)
    public JAXBElement<String> createObjectFieldsTypePid(String value) {
        return new JAXBElement<String>(_ObjectFieldsTypePid_QNAME, String.class, ObjectFieldsType.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/types/", name = "state", scope = ObjectFieldsType.class)
    public JAXBElement<String> createObjectFieldsTypeState(String value) {
        return new JAXBElement<String>(_ObjectFieldsTypeState_QNAME, String.class, ObjectFieldsType.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ListSessionType }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.fedora.info/definitions/1/0/types/", name = "listSession", scope = ResultType.class)
    public JAXBElement<ListSessionType> createResultTypeListSession(ListSessionType value) {
        return new JAXBElement<ListSessionType>(_ResultTypeListSession_QNAME, ListSessionType.class, ResultType.class, value);
    }

}
