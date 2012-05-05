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
package cz.incad.pas.editor.server.fedora;

import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.ObjectFactory;
import com.yourmediashelf.fedora.generated.foxml.ObjectPropertiesType;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import com.yourmediashelf.fedora.generated.foxml.StateType;
import java.io.Closeable;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.DataBindingException;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlSchema;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 *
 * @author Jan Pokorsky
 */
final class FoxmlUtils {

    public static final String FOXML_NAMESPACE;
    static {
        XmlSchema schema = ObjectFactory.class.getPackage().getAnnotation(XmlSchema.class);
        FOXML_NAMESPACE = schema.namespace();
        assert FOXML_NAMESPACE != null;
    }

    public static final String PROPERTY_STATE = "info:fedora/fedora-system:def/model#state";
    
    private static final Logger LOG = Logger.getLogger(FoxmlUtils.class.getName());
    private static JAXBContext defaultJaxbContext;
    private static ThreadLocal<Marshaller> defaultMarshaller = new ThreadLocal<Marshaller>();
    private static ThreadLocal<Unmarshaller> defaultUnmarshaller = new ThreadLocal<Unmarshaller>();

    /**
     * Default MODS context. Oracle JAXB RI's context should be thread safe.
     * @see <a href='http://jaxb.java.net/faq/index.html#threadSafety'>Are the JAXB runtime API's thread safe?</a>
     */
    public static JAXBContext defaultJaxbContext() throws JAXBException {
        if (defaultJaxbContext == null) {
            defaultJaxbContext = JAXBContext.newInstance(ObjectFactory.class);
        }
        return defaultJaxbContext;
    }

    /**
     * Default MODS marshaller for current thread.
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
     * Default MODS marshaller for current thread.
     */
    public static Unmarshaller defaultUnmarshaller() throws JAXBException {
        Unmarshaller m = defaultUnmarshaller.get();
        if (m == null) {
            m = defaultJaxbContext().createUnmarshaller();
            defaultUnmarshaller.set(m);
        }
        return m;
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
            datastreamVersion.setID(String.format("%s.%s", dsId, versions.size() - 1));
        }
        return datastreamVersion;
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
            String id = datastream.getID();
            if (dsId.equals(id)) {
                return datastream;
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

    public static String createPid() {
        UUID uuid = UUID.randomUUID();
        return "uuid:" + uuid;
    }

    public static DigitalObject createFoxml(String pid) {
        DigitalObject digObj = new DigitalObject();
        digObj.setPID(pid);
        digObj.setVERSION("1.1");

        ObjectPropertiesType props = new ObjectPropertiesType();
        // state property is required by foxml1-1.xsd
        PropertyType state = new PropertyType();
        state.setNAME(PROPERTY_STATE);
        state.setVALUE(StateType.A.value());
        props.getProperty().add(state);
        digObj.setObjectProperties(props);
        
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
