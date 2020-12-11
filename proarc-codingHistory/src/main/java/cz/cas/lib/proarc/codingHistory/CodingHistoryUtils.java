/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.codingHistory;

import edu.harvard.hul.ois.xml.ns.jhove.ObjectFactory;
import edu.harvard.hul.ois.xml.ns.jhove.Property;
import edu.harvard.hul.ois.xml.ns.jhove.PropertyType;
import javax.xml.bind.DataBindingException;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Lukas Sykora
 */
public final class CodingHistoryUtils {

    /**
     * The AES57 namespace {@code http://www.aes.org/audioObject}.
     */
    public static final String NS = "http://hul.harvard.edu/ois/xml/ns/jhove";
    
    private static JAXBContext defaultJaxbContext;
    private static ThreadLocal<Marshaller> defaultMarshaller = new ThreadLocal<Marshaller>();
    private static ThreadLocal<Unmarshaller> defaultUnmarshaller = new ThreadLocal<Unmarshaller>();

    /**
     * Default context. Oracle JAXB RI's context should be thread safe.
     * @see <a href='http://jaxb.java.net/faq/index.html#threadSafety'>Are the JAXB runtime API's thread safe?</a>
     */
    public static JAXBContext defaultJaxbContext() throws JAXBException {
        if (defaultJaxbContext == null) {
            defaultJaxbContext = JAXBContext.newInstance(ObjectFactory.class);
        }
        return defaultJaxbContext;
    }

    public static Marshaller defaultMarshaller(boolean indent) throws JAXBException {
        Marshaller m = defaultMarshaller.get();
        if (m == null) {
            // later we could use a pool to minimize Marshaller instances
            m = defaultJaxbContext().createMarshaller();
            defaultMarshaller.set(m);
        }
        m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, indent);
        return m;
    }

    /**
     * Default marshaller for current thread.
     */
    public static Unmarshaller defaultUnmarshaller() throws JAXBException {
        Unmarshaller m = defaultUnmarshaller.get();
        if (m == null) {
            m = defaultJaxbContext().createUnmarshaller();
            defaultUnmarshaller.set(m);
        }
        return m;
    }
    /**
     * Dumps object to XML string.
     */
    public static String toXml(Property codingHistory, boolean indent) {
        StringWriter dump = new StringWriter();
        marshal(new StreamResult(dump), codingHistory, indent);
        return dump.toString();
    }

    public static void marshal(Result target, Property codingHistory, boolean indent) {
        try {
            Marshaller m = defaultMarshaller(indent);
            m.marshal(codingHistory, target);
        } catch (JAXBException ex) {
            throw new DataBindingException(ex);
        }
    }

    public static <T> T unmarshal(InputStream source, Class<T> type, boolean close) {
        try {
            return unmarshal(new StreamSource(source), type);
        } finally {
            if (close) {
                try {
                    source.close();
                } catch (IOException ex) {
                    Logger.getLogger(CodingHistoryUtils.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
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

    public static Property unmarshalCodingHistory(Source source) {
        try {
            Object unmarshaled = defaultUnmarshaller().unmarshal(source);
            if (unmarshaled instanceof JAXBElement) {
                unmarshaled = ((JAXBElement) unmarshaled).getValue();
            }
            Property codingHistory;
            if (unmarshaled instanceof Property) {
                codingHistory = (Property) unmarshaled;
            } else if (unmarshaled instanceof PropertyType) {
                codingHistory = (Property) unmarshaled;
            } else {
                throw new IllegalStateException(String.valueOf(unmarshaled));
            }
            return codingHistory;
        } catch (JAXBException ex) {
            throw new DataBindingException(ex);
        }
    }
}
