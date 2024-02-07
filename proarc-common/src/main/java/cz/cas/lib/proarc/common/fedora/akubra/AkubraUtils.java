package cz.cas.lib.proarc.common.fedora.akubra;

import com.yourmediashelf.fedora.generated.foxml.ContentLocationType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import com.yourmediashelf.fedora.util.DateUtility;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage.AkubraObject;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigInteger;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.io.IOUtils;
import org.glassfish.jersey.uri.UriComponent;
import org.w3c.dom.Element;

public class AkubraUtils {

    private static final Logger LOG = Logger.getLogger(AkubraUtils.class.getName());
    private static final String LOCAL_REF_PREFIX = "http://local.fedora.server/fedora/get/";

    public static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'.'SSS'Z'");

    private static Unmarshaller unmarshallerKram = null;
    private static Unmarshaller unmarshallerProArc = null;
    private static Marshaller marshallerKram = null;
    private static Marshaller marshallerProArc = null;

    static {
        try {
            JAXBContext jaxbContextKram = JAXBContext.newInstance(new Class[]{DigitalObject.class});
            unmarshallerKram = jaxbContextKram.createUnmarshaller();
            marshallerKram = jaxbContextKram.createMarshaller();
            JAXBContext jaxbContextPro = JAXBContext.newInstance(new Class[]{com.yourmediashelf.fedora.generated.foxml.DigitalObject.class});
            unmarshallerProArc = jaxbContextPro.createUnmarshaller();
            marshallerProArc = jaxbContextPro.createMarshaller();
        } catch (Exception var8) {
            LOG.log(Level.SEVERE, "Cannot init JAXB", var8);
            throw new RuntimeException(var8);
        }
    }

    public static DigitalObject getDigitalObject(AkubraManager manager, String pid) throws JAXBException {
        try {
            InputStream inputStream = manager.retrieveObject(pid);
            try {
                synchronized(unmarshallerKram) {
                    Object obj = unmarshallerKram.unmarshal(inputStream);
                    DigitalObject digitalObject = (DigitalObject) obj;
                    return digitalObject;
                }
            } catch (Throwable ex) {
                if (inputStream != null) {
                    try {
                        inputStream.close();
                    } catch (Throwable var16) {
                        ex.addSuppressed(var16);
                    }
                }

                throw ex;
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    public static DigitalObject getDigitalObjectProArc(AkubraManager manager, String pid) throws JAXBException {
        try {
            InputStream inputStream = manager.retrieveObject(pid);
            try {
                synchronized(unmarshallerProArc) {
                    Object obj = unmarshallerProArc.unmarshal(inputStream);
                    DigitalObject digitalObject = (DigitalObject) obj;
                    return digitalObject;
                }
            } catch (Throwable ex) {
                if (inputStream != null) {
                    try {
                        inputStream.close();
                    } catch (Throwable var16) {
                        ex.addSuppressed(var16);
                    }
                }

                throw ex;
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }


    // jen pro exporty, protoze s nahrazuji odkazy do datastoru za binaryContent
    public static DigitalObject getDigitalObjectToExport(AkubraManager manager, String pid) throws JAXBException {
        try {
            InputStream inputStream = manager.retrieveObject(pid);
            try {
                synchronized(unmarshallerProArc) {
                    Object obj = unmarshallerProArc.unmarshal(inputStream);
                    DigitalObject digitalObject = (DigitalObject) obj;
                    List<DatastreamType> toDelete = new ArrayList<>();
                    for (DatastreamType datastream : digitalObject.getDatastream()) {
                        for (DatastreamVersionType datastreamVersion : datastream.getDatastreamVersion()) {
                            if (datastreamVersion.getContentLocation() != null) {
                                datastreamVersion.setXmlContent(null);
                                datastreamVersion.setBinaryContent(null);
                                ContentLocationType contentLocationType = datastreamVersion.getContentLocation();
                                if (contentLocationType.getREF().startsWith(FoxmlUtils.LOCAL_FEDORA_OBJ_PATH)) {
                                    toDelete.add(datastream);
                                    continue;
                                } else {
                                    InputStream is = manager.retrieveDatastream(contentLocationType.getREF());
                                    byte[] binaryContent = new byte[is.available()];
                                    is.read(binaryContent);
                                    datastreamVersion.setBinaryContent(binaryContent);
                                    datastreamVersion.setContentLocation(null);
                                }
                            }
                        }
                    }
                    if (!toDelete.isEmpty()) {
                        digitalObject.getDatastream().removeAll(toDelete);
                    }
                    return digitalObject;
                }
            } catch (Throwable ex) {
                if (inputStream != null) {
                    try {
                        inputStream.close();
                    } catch (Throwable var16) {
                        ex.addSuppressed(var16);
                    }
                }

                throw ex;
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static DatastreamVersionType getLastStreamVersion(DigitalObject digitalObject, String dsId) {
        if (dsId == null || dsId.isEmpty()) {
            return null;
        }

        for (DatastreamType datastream : digitalObject.getDatastream()) {
            if (dsId.equals(datastream.getID())) {
                return getLastStreamVersion(datastream);
            }
        }
        return null;
    }

    private static DatastreamVersionType getLastStreamVersion(DatastreamType datastream) {
        List<DatastreamVersionType> datastreamVersionList = datastream.getDatastreamVersion();
        return datastreamVersionList != null && !datastreamVersionList.isEmpty() ? datastreamVersionList.get(datastreamVersionList.size() - 1) : null;
    }

    public static DatastreamProfile createDatastremProfile(DigitalObject digitalObject, String dsId) {
        DatastreamProfile profile = new DatastreamProfile();
        profile.setPid(digitalObject.getPID());

        if (dsId == null || dsId.isEmpty()) {
            return profile;
        }

        for (DatastreamType datastream : digitalObject.getDatastream()) {
            if (dsId.equals(datastream.getID())) {
                profile.setDsControlGroup(datastream.getCONTROLGROUP());
                profile.setDsVersionable(String.valueOf(datastream.isVERSIONABLE()));
                profile.setDsState(datastream.getSTATE().value());
                profile.setDsID(datastream.getID());

                DatastreamVersionType type =  getLastStreamVersion(datastream);
                if (type != null) {
                    profile.setDsVersionID(type.getID());
                    profile.setDsLabel(type.getLABEL());
                    profile.setDateTime(type.getCREATED());
                    profile.setDsCreateDate(type.getCREATED());
                    profile.setDsMIME(type.getMIMETYPE());
                    profile.setDsFormatURI(type.getFORMATURI());
                    profile.setDsSize(BigInteger.valueOf(type.getSIZE()));
                }
                break;
            }
        }
        return normalizeProfile(profile);
    }

    public static List<DatastreamProfile> createDatastremProfiles(DigitalObject digitalObject) {
        List<DatastreamProfile> profileList = new ArrayList<>();

        if (digitalObject == null) {
            return profileList;
        }
        for (DatastreamType datastream : digitalObject.getDatastream()) {
            DatastreamProfile profile = new DatastreamProfile();
            profile.setPid(digitalObject.getPID());
            profile.setDsControlGroup(datastream.getCONTROLGROUP());
            profile.setDsVersionable(String.valueOf(datastream.isVERSIONABLE()));
            profile.setDsState(datastream.getSTATE().value());
            profile.setDsID(datastream.getID());

            DatastreamVersionType type =  getLastStreamVersion(datastream);
            if (type != null) {
                profile.setDsVersionID(type.getID());
                profile.setDsLabel(type.getLABEL());
                profile.setDateTime(type.getCREATED());
                profile.setDsCreateDate(type.getCREATED());
                profile.setDsMIME(type.getMIMETYPE());
                profile.setDsFormatURI(type.getFORMATURI());
                profile.setDsSize(BigInteger.valueOf(type.getSIZE()));
            }
            profile = normalizeProfile(profile);
            profileList.add(profile);
        }
        return profileList;
    }

    public static InputStream getStreamContent(DatastreamVersionType stream, AkubraManager manager) throws TransformerException, IOException {
        if (stream.getXmlContent() != null) {
            StringWriter wrt = new StringWriter();
            for (Element element : stream.getXmlContent().getAny()) {
                print(element, wrt);
            }
            return IOUtils.toInputStream(wrt.toString(), Charset.forName("UTF-8"));
        } else if (stream.getContentLocation() != null) {
            if (stream.getContentLocation().getTYPE().equals("INTERNAL_ID")) {
                return manager.retrieveDatastream(stream.getContentLocation().getREF());
            } else if (stream.getContentLocation().getTYPE().equals("URL")) {
                if (stream.getContentLocation().getREF().startsWith(LOCAL_REF_PREFIX)) {
                    String[] refArray = stream.getContentLocation().getREF().replace(LOCAL_REF_PREFIX, "").split("/");
                    if (refArray.length == 2) {
                        return manager.retrieveDatastream(refArray[0] + "+" + refArray[1] + "+" + refArray[1] + ".0");
                    } else {
                        throw new IOException("Invalid datastream local reference: " + stream.getContentLocation().getREF());
                    }
                } else {
                    return readFromURL(stream.getContentLocation().getREF());
                }
            } else {
                throw new IOException("Unsupported datastream reference type: " + stream.getContentLocation().getTYPE() + "(" + stream.getContentLocation().getREF() + ")");
            }
        } else if (stream.getBinaryContent() != null) {
            LOG.warning("Reading binaryContent from the managed stream.");
            return new ByteArrayInputStream(stream.getBinaryContent());
        } else {
            throw new IOException("Unsupported datastream content type: " + stream.getID());
        }
    }

    public static InputStream getDatastreamDissemination(AkubraObject object, String streamName) throws IOException, TransformerException, JAXBException {
        DigitalObject digitalObject = AkubraUtils.getDigitalObject(object.getManager(), object.getPid());
        for (DatastreamType datastreamType : digitalObject.getDatastream()) {
            if (streamName.equals(datastreamType.getID())) {
                if (datastreamType.getDatastreamVersion() != null && !datastreamType.getDatastreamVersion().isEmpty()) {
                    DatastreamVersionType datastreamVersionType = datastreamType.getDatastreamVersion().get(0);
                    InputStream input = getStreamContent(datastreamVersionType, object.getManager());
                    return input;
                }
            }
        }
        return null;
    }

    private static InputStream readFromURL(String url) throws IOException {
        URL searchURL = new URL(url);
        URLConnection conn = searchURL.openConnection();
        conn.setUseCaches(true);
        conn.connect();
        if ("gzip".equals(conn.getContentEncoding())) {
            return new GZIPInputStream(conn.getInputStream());
        } else {
            return conn.getInputStream();
        }
    }

    public static DatastreamProfile normalizeProfile(DatastreamProfile profile) {
        String format = profile.getDsFormatURI();
        profile.setDsFormatURI(format != null && format.isEmpty() ? null : format);
        return profile;
    }

    protected static DatastreamType getDatastream(DigitalObject object, DatastreamProfile profile) {
        for (DatastreamType datastream : object.getDatastream()) {
            if (datastream.getID().equals(profile.getDsID())) {
                return datastream;
            }
        }
        return null;
    }

    public static XMLGregorianCalendar toXmlGregorian(Date date) throws DatatypeConfigurationException {
        GregorianCalendar c = new GregorianCalendar();
        c.setTime(date);
        return DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
    }

    public static long getLastModified(DigitalObject digitalObject, String dsId) throws IOException {
        DatastreamVersionType stream = AkubraUtils.getLastStreamVersion(digitalObject, dsId);
        if (stream != null) {
            return DateUtility.parseXSDDateTime(stream.getCREATED().toXMLFormat()).toDate().getTime();
        } else {
            throw new IOException("Cannot find stream '" + dsId + "' for pid '" + digitalObject.getPID() + "'");
        }
    }

    protected static String qpEncode(String p) {
        return p == null || p.isEmpty()
                ? p
                : UriComponent.encode(p, UriComponent.Type.QUERY_PARAM_SPACE_ENCODED);
    }

    public static XMLGregorianCalendar createDate() {
        GregorianCalendar c = new GregorianCalendar();
        c.setTime(new Date());
        try {
            return DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
        } catch (DatatypeConfigurationException e) {
            return null;
        }
    }

    public static PropertyType createProperty(String name, String value) {
        PropertyType propertyType = new PropertyType();
        propertyType.setNAME(name);
        propertyType.setVALUE(value);
        return propertyType;
    }

    public static String currentTimeString() {
        return DATE_FORMAT.format(new Date());
    }

    public static XMLGregorianCalendar getCurrentXMLGregorianCalendar() {
        try {
            return DatatypeFactory.newInstance().newXMLGregorianCalendar(DATE_FORMAT.format(new Date()));
        } catch (DatatypeConfigurationException e) {
            LOG.log(Level.SEVERE, e.getMessage(), e);
            throw new RuntimeException(e);
        }
    }

    public static void print(Element elm, Writer out) throws TransformerException {
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();

        DOMSource source = new DOMSource(elm);
        StreamResult result = new StreamResult(out);
        transformer.transform(source, result);
    }
}
