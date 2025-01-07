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
package cz.cas.lib.proarc.common.mods;

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.process.export.mets.MetsLSResolver;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.xml.Transformers;
import cz.cas.lib.proarc.common.xml.Transformers.Format;
import cz.cas.lib.proarc.mods.ModsCollectionDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.XMLConstants;
import javax.xml.bind.DataBindingException;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.util.JAXBSource;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.xml.sax.SAXException;

/**
 * Utilities fro MODS version 3.4 and later.
 *
 * @author Jan Pokorsky
 */
public final class ModsUtils {

    /**
     * The actual MODS version.
     */
    public static final String VERSION = "3.6";

    private static JAXBContext defaultJaxbContext;
    private static ThreadLocal<Marshaller> defaultMarshaller = new ThreadLocal<Marshaller>();
    private static ThreadLocal<Unmarshaller> defaultUnmarshaller = new ThreadLocal<Unmarshaller>();
    private static Schema MODS_SCHEMA;
    private static final String MODS_SCHEMA_PATH = "mods-3-6.xsd";

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
    /**
     * Dumps MODS object to XML string.
     */
    public static String toXml(ModsDefinition mods, boolean indent) {
        StringWriter dump = new StringWriter();
        marshal(new StreamResult(dump), mods, indent);
        return dump.toString();
    }

    /**
     * @see ..proarc-mods.src.main.java.cz.cas.lib.proarc.mods.package-info.java contains name space prefix mapping.
     */
    public static void marshal(Result target, ModsDefinition mods, boolean indent) {
        try {
            Marshaller m = defaultMarshaller(indent);
            m.marshal(new ObjectFactory().createMods(mods), target);
        } catch (JAXBException ex) {
            throw new DataBindingException(ex);
        }
    }

    public static void marshal(Result target, Object modsElement, boolean indent) {
        try {
            Marshaller m = defaultMarshaller(indent);
            m.marshal(modsElement, target);
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
                    Logger.getLogger(ModsUtils.class.getName()).log(Level.SEVERE, null, ex);
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

    public static ModsDefinition unmarshalModsType(Source source) {
        try {
            Object unmarshaled = defaultUnmarshaller().unmarshal(source);
            if (unmarshaled instanceof JAXBElement) {
                unmarshaled = ((JAXBElement) unmarshaled).getValue();
            }
            ModsDefinition mods;
            if (unmarshaled instanceof ModsCollectionDefinition) {
                ModsCollectionDefinition mc = (ModsCollectionDefinition) unmarshaled;
                mods = mc.getMods().get(0);
            } else if (unmarshaled instanceof ModsDefinition) {
                mods = (ModsDefinition) unmarshaled;
            } else {
                throw new IllegalStateException(String.valueOf(unmarshaled));
            }
            return mods;
        } catch (JAXBException ex) {
            throw new DataBindingException(ex);
        }
    }

    /**
     * Parameters for MODS/HTML transformation.
     * @param locale locale to use for descriptions; can be {@code null}
     * @return parameters map
     */
    public static Map<String, Object> modsAsHtmlParameters(Locale locale) {
        if (locale == null) {
            return Collections.emptyMap();
        }
        ResourceBundle.Control control = ResourceBundle.Control.getControl(ResourceBundle.Control.FORMAT_PROPERTIES);
        String baseName = "xml.modsDictionary";
        List<Locale> candidateLocales = control.getCandidateLocales(baseName, locale);
        HashMap<String, Object> params = new HashMap<String, Object>();
        for (Locale candidateLocale : candidateLocales) {
            String toBundleName = control.toBundleName(baseName, candidateLocale);
            String resourceName = '/' + control.toResourceName(toBundleName, "xml");
            URL resource = ModsUtils.class.getResource(resourceName);
            if (resource != null) {
                params.put("MODS_DICTIONARY", resource.toExternalForm());
                break;
            }
        }
        return params;
    }

    public static String getLabel(ModsDefinition mods, String model) {
        try {
            JAXBSource src = new JAXBSource(
                    defaultMarshaller(false), new ObjectFactory().createMods(mods));
            Transformers t = new Transformers(null);
            byte[] contents = t.transformAsBytes(src, Format.ModsAsFedoraLabel, Collections.<String, Object>singletonMap("MODEL", model));
            int threshold = Math.min(contents.length, 2000);
            String label = contents.length == 0 ? "?" : new String(contents, 0, threshold, "UTF-8");
            return label;
        } catch (JAXBException ex) {
            throw new IllegalStateException(model, ex);
        } catch (TransformerException ex) {
            throw new IllegalStateException(model, ex);
        } catch (UnsupportedEncodingException ex) {
            throw new IllegalStateException(model, ex);
        }
    }

    public static Schema getSchema() throws SAXException {
        if (MODS_SCHEMA == null) {
            SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            schemaFactory.setResourceResolver(MetsLSResolver.getInstance());
            MODS_SCHEMA = schemaFactory.newSchema(ModsDefinition.class.getResource(MODS_SCHEMA_PATH));
        }
        return MODS_SCHEMA;
    }

    public static ModsDefinition overrideDescriptionStandard(ModsDefinition mods, String standard) {
        for (RecordInfoDefinition recordInfo : mods.getRecordInfo()) {
            for (StringPlusLanguagePlusAuthority descriptionStandard : recordInfo.getDescriptionStandard()) {
                descriptionStandard.setValue(standard);
            }
        }
        return mods;
    }

    public static ModsDefinition getMods(String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fo = dom.find(pid, null);
        XmlStreamEditor streamEditorOld = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditorOld = new ModsStreamEditor(streamEditorOld, fo);
        return modsStreamEditorOld.read();
    }
}
