/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.urnnbn;

import cz.cas.lib.proarc.urnnbn.model.registration.Import;
import jakarta.xml.bind.DataBindingException;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.util.JAXBSource;
import java.io.IOException;
import java.io.StringWriter;
import javax.xml.XMLConstants;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;

/**
 * The XML helper.
 *
 * @author Jan Pokorsky
 */
public final class ResolverXmlUtils {

    private static Schema REGISTRATION_SCHEMA;
    private static JAXBContext REGISTRATION_CONTEXT;

    public static void validate(Import imp, ErrorHandler status) throws SAXException {
        try {
            if (REGISTRATION_SCHEMA == null) {
                SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
                REGISTRATION_SCHEMA = schemaFactory.newSchema(ResolverXmlUtils.class.getResource(
                        "registration/digDocRegistration.xsd"));
            }
            Validator validator = REGISTRATION_SCHEMA.newValidator();
            validator.setErrorHandler(status);
            validator.validate(new JAXBSource(defaultRegistrationContext(), imp));
        } catch (IOException ex) {
            throw new SAXException(ex);
        } catch (JAXBException ex) {
            throw new SAXException(ex);
        }
    }

    /**
     * Gets the default registration context. Oracle JAXB RI's context should be thread safe.
     *
     * @see <a href='http://jaxb.java.net/faq/index.html#threadSafety'>Are the JAXB runtime API's thread safe?</a>
     */
    public static JAXBContext defaultRegistrationContext() throws JAXBException {
        if (REGISTRATION_CONTEXT == null) {
            REGISTRATION_CONTEXT = JAXBContext.newInstance(Import.class);
        }
        return REGISTRATION_CONTEXT;
    }

    public static String toString(Import imp) {
        StringWriter dump = new StringWriter();
        try {
            defaultRegistrationContext().createMarshaller().marshal(imp, dump);
        } catch (JAXBException ex) {
            throw new DataBindingException(ex);
        }
        return dump.toString();
    }

}
