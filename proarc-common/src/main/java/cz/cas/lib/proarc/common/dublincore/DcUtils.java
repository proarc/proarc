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
package cz.cas.lib.proarc.common.dublincore;

import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.oaidublincore.ObjectFactory;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
import java.util.Collection;
import java.util.List;
import javax.xml.bind.DataBindingException;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
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
        List<ElementType> titles = dc.getTitles();
        return titles.isEmpty() ? "?" : titles.get(0).getValue();
    }

    public static OaiDcType merge(OaiDcType dc, OaiDcType add) {
        merge(dc.getContributors(), add.getContributors());
        merge(dc.getCoverages(), add.getCoverages());
        merge(dc.getCreators(), add.getCreators());
        merge(dc.getDates(), add.getDates());
        merge(dc.getDescriptions(), add.getDescriptions());
        merge(dc.getFormats(), add.getFormats());
        merge(dc.getIdentifiers(), add.getIdentifiers());
        merge(dc.getLanguages(), add.getLanguages());
        merge(dc.getPublishers(), add.getPublishers());
        merge(dc.getRelations(), add.getRelations());
        merge(dc.getRights(), add.getRights());
        merge(dc.getSources(), add.getSources());
        merge(dc.getSubjects(), add.getSubjects());
        merge(dc.getTitles(), add.getTitles());
        merge(dc.getTypes(), add.getTypes());
        return dc;
    }

    public static void merge(List<ElementType> elms, List<ElementType> add) {
        if (elms == null) {
            throw new NullPointerException();
        }
        for (ElementType elm : add) {
            merge(elms, elm);
        }
    }

    public static void merge(List<ElementType> elms, ElementType add) {
        for (ElementType elm : elms) {
            String value = elm.getValue();
            String addValue = add.getValue();
            if (value == null ? addValue == null : value.equals(addValue)) {
                String lang = elm.getLang();
                String addLang = add.getLang();
                if (lang == null ? addLang == null : lang.equals(addLang)) {
                    return;
                }
            }
        }
        elms.add(add);
    }

    public static void addPid(OaiDcType dc, String pid) {
        addElementType(dc.getIdentifiers(), pid);
    }

    public static void addModel(OaiDcType dc, String modelId) {
        addElementType(dc.getTypes(), modelId);
    }

    public static void addTitle(OaiDcType dc, String title) {
        addElementType(dc.getTitles(), title);
    }

    public static void addOwner(OaiDcType dc, Collection<String> owners) {
        for (String owner : owners) {
            addOwner(dc, owner);
        }
    }

    public static void addOwner(OaiDcType dc, String owner) {
        addElementType(dc.getRights(), owner);
    }

    static void addElementType(List<ElementType> elms, String value) {
        for (ElementType elm : elms) {
            if (value.equals(elm.getValue())) {
                return;
            }
        }
        elms.add(new ElementType(value, null));
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
