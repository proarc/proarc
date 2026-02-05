/*
 * Copyright (C) 2012 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.storage;

import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.ObjectFactory;
import com.yourmediashelf.fedora.generated.foxml.ObjectPropertiesType;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import com.yourmediashelf.fedora.generated.foxml.StateType;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.xml.bind.DataBindingException;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBElement;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import jakarta.xml.bind.annotation.XmlSchema;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import javax.xml.XMLConstants;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Element;

/**
 *
 * @author Jan Pokorsky
 */
public final class FoxmlUtils {

    public static final String FOXML_NAMESPACE;
    /**
     * The audit data stream ID.
     */
    public static final String DS_AUDIT_ID = "AUDIT";
    public static final String PID_PREFIX = "uuid:";
    public static final String LOCAL_FEDORA_OBJ_PATH = "http://local.fedora.server/fedora/get/";

    static {
        XmlSchema schema = ObjectFactory.class.getPackage().getAnnotation(XmlSchema.class);
        FOXML_NAMESPACE = schema.namespace();
        assert FOXML_NAMESPACE != null;
    }

    public static final String PROPERTY_CREATEDATE = "info:fedora/fedora-system:def/model#createdDate";
    public static final String PROPERTY_LABEL = "info:fedora/fedora-system:def/model#label";
    public static final String PROPERTY_LASTMODIFIED = "info:fedora/fedora-system:def/view#lastModifiedDate";
    public static final String PROPERTY_OWNER = "info:fedora/fedora-system:def/model#ownerId";
    public static final String PROPERTY_STATE = "info:fedora/fedora-system:def/model#state";

    private static final Logger LOG = Logger.getLogger(FoxmlUtils.class.getName());
    private static JAXBContext defaultJaxbContext;
    private static ThreadLocal<Marshaller> defaultMarshaller = new ThreadLocal<Marshaller>();
    private static ThreadLocal<Unmarshaller> defaultUnmarshaller = new ThreadLocal<Unmarshaller>();
    private static final Pattern PID_PATTERN = Pattern.compile(
            "^([A-Za-z0-9]|-|\\.)+:(([A-Za-z0-9])|-|\\.|~|_|(%[0-9A-F]{2}))+$");
    private static final Pattern PID_NEW_PATTERN = Pattern.compile(
            "^uuid:[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$");

    /**
     * Default FOXML context. Oracle JAXB RI's context should be thread safe.
     *
     * @see <a href='http://jaxb.java.net/faq/index.html#threadSafety'>Are the JAXB runtime API's thread safe?</a>
     */
    public static JAXBContext defaultJaxbContext() throws JAXBException {
        if (defaultJaxbContext == null) {
            defaultJaxbContext = JAXBContext.newInstance(ObjectFactory.class, DigitalObject.class);
        }
        return defaultJaxbContext;
    }

    /**
     * Default FOXML marshaller for current thread.
     */
    public static Marshaller defaultMarshaller(boolean indent) throws JAXBException {
        Marshaller m = defaultMarshaller.get();
        if (m == null) {
            // later we could use a pool to minimize Marshaller instances
            m = defaultJaxbContext().createMarshaller();
            defaultMarshaller.set(m);
            m.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
                    FOXML_NAMESPACE + " http://www.fedora.info/definitions/1/0/foxml1-1.xsd");
            m.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
        }
        m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, indent);
        return m;
    }

    /**
     * Default FOXML marshaller for current thread.
     */
    public static Unmarshaller defaultUnmarshaller() throws JAXBException {
        Unmarshaller m = defaultUnmarshaller.get();
        if (m == null) {
            m = defaultJaxbContext().createUnmarshaller();
            defaultUnmarshaller.set(m);
        }
        return m;
    }

    public static void setProperty(DigitalObject dobj, String name, String value) {
        PropertyType propery = findProperty(dobj, name);
        if (value == null) {
            if (propery != null) {
                dobj.getObjectProperties().getProperty().remove(propery);
            }
            return;
        }
        if (propery == null) {
            propery = createProperty(dobj, name);
        }
        propery.setVALUE(value);
    }

    private static PropertyType createProperty(DigitalObject dobj, String name) {
        ObjectPropertiesType objectProperties = dobj.getObjectProperties();
        if (objectProperties == null) {
            objectProperties = new ObjectPropertiesType();
            dobj.setObjectProperties(objectProperties);
        }
        PropertyType property = new PropertyType();
        property.setNAME(name);
        objectProperties.getProperty().add(property);
        return property;
    }

    public static PropertyType findProperty(DigitalObject dobj, String name) {
        ObjectPropertiesType objectProperties = dobj.getObjectProperties();
        if (objectProperties != null) {
            return findProperty(objectProperties, name);
        }
        return null;
    }

    public static PropertyType findProperty(ObjectPropertiesType objectProperties, String name) {
        for (PropertyType property : objectProperties.getProperty()) {
            if (property.getNAME().equals(name)) {
                return property;
            }
        }
        return null;
    }

    public static DatastreamVersionType createDataStreamVersion(DigitalObject dobj,
                                                                String dsId, ControlGroup controlGroup, boolean versionable, StateType state) {

        DatastreamVersionType datastreamVersion = null;
        DatastreamType datastream = findDatastream(dobj, dsId);
        if (datastream == null) {
            datastream = createDatastream(dsId, controlGroup, versionable, state);
            dobj.getDatastream().add(datastream);
        }

        List<DatastreamVersionType> versions = datastream.getDatastreamVersion();
        datastreamVersion = findDatastreamVersion(datastream);
        if (datastreamVersion == null) {
            datastreamVersion = new DatastreamVersionType();
            versions.add(datastreamVersion);
            // for now expect version ordering from oldest to newest
            datastreamVersion.setID(versionNewId(dsId, versions.size() - 1));
        }
        return datastreamVersion;
    }

    public static String versionDefaultId(String dsId) {
        return versionNewId(dsId, 0);
    }

    private static String versionNewId(String dsId, int existingVersionCount) {
        return String.format("%s.%s", dsId, existingVersionCount);
    }

    public static DatastreamVersionType findDataStreamVersion(DigitalObject dobj, String dsId) {
        DatastreamVersionType datastreamVersion = null;
        if (dobj != null) {
            DatastreamType datastream = findDatastream(dobj, dsId);
            if (datastream != null) {
                datastreamVersion = findDatastreamVersion(datastream);
            }
        }
        return datastreamVersion;
    }

    public static DatastreamType findDatastream(DigitalObject dobj, String dsId) {
        for (DatastreamType datastream : dobj.getDatastream()) {
            if (datastream != null) {
                String id = datastream.getID();
                if (dsId.equals(id)) {
                    return datastream;
                }
            }
        }
        return null;
    }

    /**
     * Finds newest version.
     * For now expects versions ordering from oldest to newest.
     */
    private static DatastreamVersionType findDatastreamVersion(DatastreamType datastream) {
        List<DatastreamVersionType> versions = datastream.getDatastreamVersion();
        return versions.isEmpty() ? null : versions.get(versions.size() - 1);
    }

    /**
     * Translates {@link DatastreamType} to {@link DatastreamProfile}. It searches
     * for the newest version.
     * <p>For now it fills only description!
     */
    public static DatastreamProfile toDatastreamProfile(String pid, DatastreamType datastream) {
        DatastreamVersionType dv = findDatastreamVersion(datastream);
        return dv == null ? null : toDatastreamProfile(pid, dv, datastream);
    }

    /**
     * Translates {@link DatastreamType} to {@link DatastreamProfile}.
     * For now it fills only description!
     */
    public static DatastreamProfile toDatastreamProfile(String pid,
                                                        DatastreamVersionType version, DatastreamType datastream) {

        DatastreamProfile profile = new DatastreamProfile();
        profile.setDsID(datastream.getID());
        profile.setDsLabel(version.getLABEL());
        profile.setDsCreateDate(version.getCREATED());
        profile.setDsFormatURI(version.getFORMATURI());
        profile.setDsMIME(version.getMIMETYPE());

        profile.setDateTime(version.getCREATED());
//        profile.setDsChecksum();
        profile.setDsControlGroup(datastream.getCONTROLGROUP());
        profile.setDsState(datastream.getSTATE().value());
//        profile.setDsVersionID();
//        profile.setPid(datastream.);
        return profile;
    }


    /**
     * Translates {@link com.yourmediashelf.fedora.generated.access.DatastreamType}
     * to {@link DatastreamProfile}. It searches
     * for the newest version.
     * <p>For now it fills only description!
     */
    public static DatastreamProfile toDatastreamProfile(String pid,
                                                        com.yourmediashelf.fedora.generated.access.DatastreamType datastream) {

        DatastreamProfile profile = new DatastreamProfile();
        profile.setDsID(datastream.getDsid());
        profile.setDsLabel(datastream.getLabel());
        profile.setDsMIME(datastream.getMimeType());
        return profile;
    }

    /**
     * Dumps FOXML object to XML string.
     */
    public static String toXml(DigitalObject dobj, boolean indent) {
        StringWriter dump = new StringWriter();
        marshal(new StreamResult(dump), dobj, indent);
        return dump.toString();
    }

    public static void marshal(Result target, DigitalObject dobj, boolean indent) {
        try {
            Marshaller m = defaultMarshaller(indent);
            m.marshal(dobj, target);
        } catch (JAXBException ex) {
            throw new DataBindingException(ex);
        }
    }

    public static <T> T unmarshal(String source, Class<T> type) {
        return unmarshal(new StreamSource(new StringReader(source)), type);
    }

    public static <T> T unmarshal(URL source, Class<T> type) {
        return unmarshal(new StreamSource(source.toExternalForm()), type);
    }

    public static <T> T unmarshal(Source source, Class<T> type) {
        try {
            JAXBElement<T> item = defaultUnmarshaller().unmarshal(source, type);
            return item.getValue();
        } catch (JAXBException ex) {
            throw new DataBindingException(ex);
        }
    }

    public static XMLGregorianCalendar createXmlDate() {
        return createXmlDate(new Date());
    }

    public static XMLGregorianCalendar createXmlDate(Date d) {
        try {
            DatatypeFactory xmlDataFactory = DatatypeFactory.newInstance();
            GregorianCalendar gcNow = new GregorianCalendar();
            gcNow.setTime(d);
            return xmlDataFactory.newXMLGregorianCalendar(gcNow);
        } catch (DatatypeConfigurationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Creates new unique PID for digital object.
     *
     * @return PID
     */
    public static String createPid() {
        UUID uuid = UUID.randomUUID();
        return pidFromUuid(uuid.toString());
    }

    /**
     * Converts PID to UUID.
     *
     * @param pid PID of digital object
     * @return UUID
     */
    public static String pidAsUuid(String pid) {
        if (pid == null) {
            throw new NullPointerException("pid");
        } else if (!pid.startsWith(PID_PREFIX)) {
            throw new IllegalArgumentException("Invalid PID format: '" + pid + "'!");
        }
        return pid.substring(PID_PREFIX.length());
    }

    public static String identifierAsPid(IdentifierDefinition identifier) {
        return identifier == null ? null : (identifier.getTypeString() + ":" + identifier.getValue());
    }

    /**
     * Is a valid Fedora PID?
     *
     * @see <a href='https://wiki.duraspace.org/display/FEDORA37/Fedora+Identifiers#FedoraIdentifiers-PIDspids'>Fedora PID</a>
     */
    public static boolean isValidPid(String pid) {
        return pid != null && pid.length() <= 64 && PID_PATTERN.matcher(pid).matches();
    }

    public static boolean validPid(String pid) {
        if (pid == null || pid.length() != 41) {
            return false;
        }
        return PID_NEW_PATTERN.matcher(pid).matches();

    }

    /**
     * Converts UUID to PID.
     *
     * @param uuid UUID
     * @return PID of digital object
     */
    public static String pidFromUuid(String uuid) {
        if (uuid == null) {
            throw new NullPointerException("uuid");
        }
        return PID_PREFIX + uuid;
    }

    public static DigitalObject createFoxml(String pid) {
        DigitalObject digObj = new DigitalObject();
        digObj.setPID(pid);
        digObj.setVERSION("1.1");

        // state property is required by foxml1-1.xsd
        setProperty(digObj, PROPERTY_STATE, StateType.A.value());
        return digObj;
    }

    private static DatastreamType createDatastream(String id, ControlGroup controlGroup, boolean versionable, StateType state) {
        DatastreamType ds = new DatastreamType();
        ds.setID(id);
        ds.setCONTROLGROUP(controlGroup.toExternal());
        ds.setVERSIONABLE(versionable);
        ds.setSTATE(state);
        return ds;
    }

    public static void closeQuietly(Closeable c, String description) {
        if (c != null) {
            try {
                c.close();
            } catch (IOException ex) {
                LOG.log(Level.SEVERE, description, ex);
            }
        }
    }

    public static void copy(InputStream is, OutputStream os) throws IOException {
        byte[] buffer = new byte[2048];
        for (int length; (length = is.read(buffer)) != -1; ) {
            os.write(buffer, 0, length);
        }
    }

    private static DatastreamProfile createProfileTemplate(String dsId, String formatUri, String label, MediaType mimetype, ControlGroup control) {
        DatastreamProfile df = new DatastreamProfile();
        df.setDsMIME(mimetype.toString());
        df.setDsID(dsId);
        df.setDsControlGroup(control.toExternal());
        df.setDsFormatURI(formatUri);
        df.setDsLabel(label);
        df.setDsVersionID(FoxmlUtils.versionDefaultId(dsId));
        df.setDsVersionable(Boolean.FALSE.toString());
        return df;
    }

    /**
     * Use to embed XML content into FOXML. It expects you read and write
     * well formed XML.
     */
    public static DatastreamProfile inlineProfile(String dsId, String formatUri, String label) {
        if (dsId == null || dsId.isEmpty()) {
            throw new IllegalArgumentException();
        }
        return createProfileTemplate(dsId, formatUri, label, MediaType.TEXT_XML_TYPE, ControlGroup.INLINE);
    }

    /**
     * Use to store XML content outside FOXML. It expects you read and write
     * well formed XML.
     */
    public static DatastreamProfile managedProfile(String dsId, String formatUri, String label) {
        if (dsId == null || dsId.isEmpty()) {
            throw new IllegalArgumentException();
        }
        return createProfileTemplate(dsId, formatUri, label, MediaType.TEXT_XML_TYPE, ControlGroup.MANAGED);
    }

    /**
     * Use to store whatever content in repository. I
     */
    public static DatastreamProfile managedProfile(String dsId, MediaType mimetype, String label) {
        if (dsId == null || dsId.isEmpty() || mimetype == null) {
            throw new IllegalArgumentException();
        }
        return createProfileTemplate(dsId, null, label, mimetype, ControlGroup.MANAGED);
    }

    /**
     * Use to store whatever external content. The contents must be accessible
     * when its URL is being written to the stream!
     */
    public static DatastreamProfile externalProfile(String dsId, MediaType mimetype, String label) {
        if (dsId == null || dsId.isEmpty() || mimetype == null) {
            throw new IllegalArgumentException();
        }
        return createProfileTemplate(dsId, null, label, mimetype, ControlGroup.EXTERNAL);
    }

    public static URI localFedoraUri(String pid) throws URISyntaxException {
        return localFedoraUri(pid, null);
    }

    /**
     * Creates an object or datastream URI independent on a Fedora instance.
     * The URI is resolvable just inside a given Fedora context.
     *
     * @param pid  object PID
     * @param dsId datastream ID
     * @return the portable URI
     * @throws URISyntaxException failure
     */
    public static URI localFedoraUri(String pid, String dsId) throws URISyntaxException {
        if (pid == null || pid.isEmpty()) {
            throw new URISyntaxException(pid, "Invalid PID.");
        }
        StringBuilder sb = new StringBuilder(LOCAL_FEDORA_OBJ_PATH.length() + 100);
        sb.append(LOCAL_FEDORA_OBJ_PATH).append(pid);
        if (dsId != null) {
            sb.append('/').append(dsId);
        }
        URI location = new URI(sb.toString());
        return location;
    }

    /**
     * Check whether failure stands for a missing requested datastream.
     */
    public static boolean missingDatastream(FedoraClientException ex) {
        if (ex.getStatus() == Response.Status.NOT_FOUND.getStatusCode()) {
            // Missing datastream message:
            // HTTP 404 Error: No datastream could be found. Either there is no datastream for the digital object "uuid:5c3caa12-1e82-4670-a6aa-3d9ff8a7a3c5" with datastream ID of "TEXT_OCR"  OR  there are no datastreams that match the specified date/time value of "null".
            // Missing object message:
            // HTTP 404 Error: uuid:5c3caa12-1e82-4670-a6aa-3d9ff8a7a3c56
            // To check message see fcrepo-server/src/main/java/org/fcrepo/server/rest/DatastreamResource.java
            return ex.getMessage().contains("No datastream");
        }
        return false;
    }

    public static boolean missingDatastream(IOException ex) {
        return ex.getMessage().contains("Cannot find stream");
    }

    /**
     * Fixes DublinCore exported by Fedora Commons.
     * <p>The {@code schemaLocation} is removed.
     * <br>The namespace declaration is added to the root element.
     *
     * @param dcRoot the root element of DC
     * @return the root element of DC
     */
    public static Element fixFoxmlDc(Element dcRoot) {
        // remove xsi:schemaLocation attribute to make FOXML valid for Fedora ingest
        dcRoot.removeAttributeNS(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "schemaLocation");
        // optimize XML namespace declaration
        dcRoot.setAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI,
                XMLConstants.XMLNS_ATTRIBUTE + ":" + DcConstants.PREFIX_NS_PURL,
                DcConstants.NS_PURL);
        return dcRoot;
    }

    public enum ControlGroup {
        INLINE("X"), MANAGED("M"), EXTERNAL("E"), REDIRECT("R");

        private final String external;

        private ControlGroup(String external) {
            this.external = external;
        }

        public String toExternal() {
            return external;
        }

        public static ControlGroup fromExternal(String external) {
            for (ControlGroup group : values()) {
                if (group.toExternal().equals(external)) {
                    return group;
                }
            }
            return null;
        }

    }

}
