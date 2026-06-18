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
package cz.cas.lib.proarc.common.storage.relation;

import jakarta.xml.bind.DataBindingException;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBElement;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
import javax.xml.namespace.QName;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 *
 * @author Jan Pokorsky
 */
public final class Relations {

    public static final String RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static final String FEDORA_MODEL_NS = "info:fedora/fedora-system:def/model#";
    public static final String FEDORA_EXTERNALS_NS = "info:fedora/fedora-system:def/relations-external#";
    public static final String PROARC_RELS_NS = "http://proarc.lib.cas.cz/relations#";
    public static final QName RDF_QNAME = new QName(RDF_NS, "RDF", "rdf");

    private static JAXBContext defaultJaxbContext;
    private static ThreadLocal<Marshaller> defaultMarshaller = new ThreadLocal<Marshaller>();
    private static ThreadLocal<Unmarshaller> defaultUnmarshaller = new ThreadLocal<Unmarshaller>();

    /**
     * Default RDF context. Oracle JAXB RI's context should be thread safe.
     *
     * @see <a href='http://jaxb.java.net/faq/index.html#threadSafety'>Are the JAXB runtime API's thread safe?</a>
     */
    public static JAXBContext defaultJaxbContext() throws JAXBException {
        if (defaultJaxbContext == null) {
            defaultJaxbContext = JAXBContext.newInstance(Rdf.class);
        }
        return defaultJaxbContext;
    }

    /**
     * Default RDF marshaller for current thread.
     */
    public static Marshaller defaultMarshaller(boolean indent) throws JAXBException {
        Marshaller m = defaultMarshaller.get();
        if (m == null) {
            // later we could use a pool to minimize Marshaller instances
            m = defaultJaxbContext().createMarshaller();
            m.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
            defaultMarshaller.set(m);
        }

        m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, indent);
        return m;
    }

    /**
     * Default RDF marshaller for current thread.
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
     * Dumps RDF object to XML string.
     */
    public static String toXml(Rdf rdf, boolean indent) {
        StringWriter dump = new StringWriter();
        marshal(new StreamResult(dump), rdf, indent);
        return dump.toString();
    }

    public static void marshal(Result target, Rdf rdf, boolean indent) {
        try {
            Marshaller m = defaultMarshaller(indent);
            m.marshal(rdf, target);
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

}
