/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.workflow.profile;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.UnmarshalException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.ValidationEvent;
import javax.xml.bind.util.ValidationEventCollector;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.apache.commons.io.FileUtils;
import org.xml.sax.SAXException;

/**
 * Reads actual configuration of workflow profiles.
 *
 * @author Jan Pokorsky
 */
public class WorkflowProfiles {

    private static final Logger LOG = Logger.getLogger(WorkflowProfiles.class.getName());
    private static WorkflowProfiles INSTANCE;

    private final File file;
    private long lastModified;
    private WorkflowDefinition profiles;

    public static WorkflowProfiles getInstance() {
        return INSTANCE;
    }

    public static void setInstance(WorkflowProfiles wp) {
        INSTANCE = wp;
    }

    /**
     * Creates a new workflow definition file if not exists.
     * @param target
     * @throws IOException error
     */
    public static void copyDefaultFile(File target) throws IOException {
        if (target.exists()) {
            return ;
        }
        FileUtils.copyURLToFile(WorkflowProfiles.class.getResource("workflow.xml"), target);
    }

    public WorkflowProfiles(File file) {
        if (file == null) {
            throw new NullPointerException();
        }
        this.file = file;
    }

    /**
     * Gets actual profiles or {@code null} if in case of an error.
     */
    public synchronized WorkflowDefinition getProfiles() {
        try {
            read();
            return profiles;
        } catch (JAXBException ex) {
            LOG.log(Level.SEVERE, file.toString(), ex);
            return null;
        }
    }

    private synchronized void setProfiles(WorkflowDefinition profiles, long time) {
        if (time > lastModified) {
            this.profiles = profiles;
            this.lastModified = time;
        }
    }

    private void read() throws JAXBException {
        long currentTime = file.lastModified();
        if (currentTime == lastModified) {
            return ;
        }
        Unmarshaller unmarshaller = getUnmarshaller();
        ValidationEventCollector errors = (ValidationEventCollector) unmarshaller.getEventHandler();
        WorkflowDefinition wf = null;
        try {
            wf = (WorkflowDefinition) unmarshaller.unmarshal(file);
            wf = errors.hasEvents() ? null : wf;
        } catch (UnmarshalException ex) {
            if (!errors.hasEvents()) {
                throw ex;
            }
        } finally {
            setProfiles(wf, currentTime);
        }
        if (errors.hasEvents()) {
            StringBuilder err = new StringBuilder();
            for (ValidationEvent event : errors.getEvents()) {
                err.append(event).append('\n');
            }
            throw new JAXBException(err.toString());
        }
    }

    private Unmarshaller getUnmarshaller() throws JAXBException {
        JAXBContext jctx = JAXBContext.newInstance(WorkflowDefinition.class);
        Unmarshaller unmarshaller = jctx.createUnmarshaller();
        SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        URL schemaUrl = WorkflowDefinition.class.getResource("workflow.xsd");
        Schema schema = null;
        try {
            schema = sf.newSchema(new StreamSource(schemaUrl.toExternalForm()));
        } catch (SAXException ex) {
            throw new JAXBException("Missing schema workflow.xsd!", ex);
        }
        unmarshaller.setSchema(schema);
        ValidationEventCollector errors = new ValidationEventCollector() {

            @Override
            public boolean handleEvent(ValidationEvent event) {
                super.handleEvent(event);
                return true;
            }

        };
        unmarshaller.setEventHandler(errors);
        return unmarshaller;
    }

}
