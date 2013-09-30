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
package cz.incad.pas.editor.server.dublincore;

import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.oaidublincore.ObjectFactory;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
import javax.xml.bind.DataBindingException;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlNs;
import javax.xml.bind.annotation.XmlSchema;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.QName;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 *
 * @author Jan Pokorsky
 */
public final class DcUtils {

    public static final String DC_NAMESPACE;
    public static final String DC_PREFIX;
    public static final String OAI_DC_NAMESPACE;
    static {
        XmlSchema schema = ObjectFactory.class.getPackage().getAnnotation(XmlSchema.class);
        OAI_DC_NAMESPACE = schema.namespace();
        assert OAI_DC_NAMESPACE != null;
        XmlType elmType = ElementType.class.getAnnotation(XmlType.class);
        DC_NAMESPACE = elmType.namespace();
        XmlNs[] xmlns = schema.xmlns();
        DC_PREFIX = findPrefix(DC_NAMESPACE, xmlns);
    }

    private static String findPrefix(String ns, XmlNs[] xmlns) {
        for (XmlNs xns : xmlns) {
            if (ns.equals(xns.namespaceURI())) {
                return xns.prefix();
            }
        }
        throw new IllegalStateException(ns);
    }

    private static JAXBContext defaultJaxbContext;
    private static ThreadLocal<Marshaller> defaultMarshaller = new ThreadLocal<Marshaller>();
    private static ThreadLocal<Unmarshaller> defaultUnmarshaller = new ThreadLocal<Unmarshaller>();
    private static ThreadLocal<Transformer> defaultModsTransformer = new ThreadLocal<Transformer>();
    private static final Templates MODS2DC = createMods2dcTemplate();

    /**
     * Default DC context. Oracle JAXB RI's context should be thread safe.
     * @see <a href='http://jaxb.java.net/faq/index.html#threadSafety'>Are the JAXB runtime API's thread safe?</a>
     */
    public static JAXBContext defaultJaxbContext() throws JAXBException {
        if (defaultJaxbContext == null) {
            defaultJaxbContext = JAXBContext.newInstance(ObjectFactory.class);
        }
        return defaultJaxbContext;
    }

    /**
     * Default DC marshaller for current thread.
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
     * Default DC marshaller for current thread.
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
     * Dumps DC object to XML string.
     */
    public static String toXml(OaiDcType mods, boolean indent) {
        StringWriter dump = new StringWriter();
        marshal(new StreamResult(dump), mods, indent);
        return dump.toString();
    }

    public static void marshal(Result target, OaiDcType oaidc, boolean indent) {
        try {
            Marshaller m = defaultMarshaller(indent);
            m.marshal(new ObjectFactory().createDc(oaidc), target);
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

    /**
     * MODS -> DC transformer.
     * 
     * @param model fedora object model
     * @return transformer
     * @throws TransformerConfigurationException 
     */
    public static Transformer modsTransformer(String model) throws TransformerConfigurationException {
        Transformer t = defaultModsTransformer.get();
        if (t == null) {
            t = MODS2DC.newTransformer();
            defaultModsTransformer.set(t);
        } else {
            t.reset();
        }
        t.setParameter("MODEL", model);
        return t;
    }

    /**
     * Gets short label for DC object
     * @param dc DC
     * @return label
     */
    public static String getLabel(OaiDcType dc) {
        JAXBElement<ElementType> dummyTitle = new ObjectFactory().createTitle(null);
        QName qnTitle = dummyTitle.getName();
        for (JAXBElement<ElementType> elm : dc.getTitleOrCreatorOrSubject()) {
            if (qnTitle.equals(elm.getName()) && elm.getValue() != null) {
                ElementType elmTitle = elm.getValue();
                String title = elmTitle.getValue();
                return title;
            }
        }
        return "?";
    }

    private static Templates createMods2dcTemplate() {
        TransformerFactory tf = TransformerFactory.newInstance();
        String resourcePath = "/xml/MODS3-22simpleDC.xsl";
        URL resource = DcUtils.class.getResource(resourcePath);
        if (resource == null) {
            throw new IllegalStateException("Template not found: " + resourcePath);
        }
        try {
            return tf.newTemplates(new StreamSource(resource.toExternalForm()));
        } catch (TransformerConfigurationException ex) {
            throw new IllegalStateException(resourcePath, ex);
        }
    }

}
