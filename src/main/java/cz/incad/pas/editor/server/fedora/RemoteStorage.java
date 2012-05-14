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

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.FedoraCredentials;
import com.yourmediashelf.fedora.client.response.FedoraResponse;
import com.yourmediashelf.fedora.client.response.GetDatastreamResponse;
import com.yourmediashelf.fedora.client.response.IngestResponse;
import com.yourmediashelf.fedora.client.response.ModifyDatastreamResponse;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import com.yourmediashelf.fedora.util.DateUtility;
import cz.incad.pas.editor.server.config.PasConfiguration;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.XmlStreamEditor.EditorResult;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ConcurrentModificationException;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 * Fedora remote storage.
 *
 * @author Jan Pokorsky
 */
public final class RemoteStorage {

    private static final Logger LOG = Logger.getLogger(RemoteStorage.class.getName());

    private static RemoteStorage INSTANCE;

    private final FedoraClient client;

    public RemoteStorage(FedoraClient client) {
        this.client = client;
    }

    public static RemoteStorage getInstance(PasConfiguration conf) throws IOException {
        if (INSTANCE == null) {
            INSTANCE = new RemoteStorage(new FedoraClient(new FedoraCredentials(
                    conf.getFedoraUrl(), conf.getFedoraUsername(), conf.getFedoraPassword())));
        }
        return INSTANCE;
    }

    public RemoteObject find(String pid) {
        return new RemoteObject(pid, client);
    }

    public SearchView getSearch() {
        return new SearchView(client);
    }

    public void ingest(File foxml, String pid, String ingestUser, String log) throws FedoraClientException {
        if (ingestUser == null || ingestUser.isEmpty()) {
            throw new IllegalArgumentException("ingestUser");
        }
        IngestResponse response = FedoraClient.ingest(pid)
                .format("info:fedora/fedora-system:FOXML-1.1")
                .logMessage(log)
                .content(foxml)
                .ownerId(ingestUser)
                .execute(client);
        if (response.getStatus() != 201) {
            // XXX
        }
        LOG.log(Level.INFO, "{0}, {1}", new Object[]{response.getPid(), response.getLocation()});
    }

    public void ingest(LocalObject object, String ingestUser) throws FedoraClientException {
        ingest(object, ingestUser, "Ingested locally");
    }

    /**
     * see https://wiki.duraspace.org/display/FEDORA35/Using+File+URIs to reference external files for ingest
     */
    public void ingest(LocalObject object, String ingestUser, String log) throws FedoraClientException {
        if (ingestUser == null || ingestUser.isEmpty()) {
            throw new IllegalArgumentException("ingestUser");
        }
        if (log == null || ingestUser.isEmpty()) {
            throw new IllegalArgumentException("log");
        }
        DigitalObject digitalObject = object.getDigitalObject();

        String xml = FoxmlUtils.toXml(digitalObject, false);
        IngestResponse response = FedoraClient.ingest(object.getPid())
                .format("info:fedora/fedora-system:FOXML-1.1")
                .logMessage(log)
//                .namespace("")
//                .xParam("", "")
                .content(xml)
                .ownerId(ingestUser)
                .execute(client);
        if (response.getStatus() != 201) {
            // XXX
        }
        LOG.log(Level.INFO, "{0}, {1}", new Object[]{response.getPid(), response.getLocation()});
    }

    public static final class RemoteObject extends AbstractFedoraObject {

        private final FedoraClient client;
        private String label;

        public RemoteObject(String pid, FedoraClient client) {
            super(pid);
            this.client = client;
        }

        public FedoraClient getClient() {
            return client;
        }

        @Override
        public void setLabel(String label) {
            this.label = label;
        }

        @Override
        public void flush() {
            super.flush();
            try {
                if (label != null) {
                    FedoraClient.modifyObject(getPid()).label(label).execute(client);
                }
            } catch (FedoraClientException ex) {
                throw new IllegalStateException(getPid(), ex);
            }
        }

    }

    /**
     * XXX implement support for missing data stream (404 - FedoraClientException)
     */
    public static final class RemoteXmlStreamEditor implements XmlStreamEditor {

        private final RemoteObject object;
        private final String dsId;
        private long lastModified;
        private DatastreamProfile profile;
        private String data;
        private boolean modified;

        public RemoteXmlStreamEditor(RemoteObject object, String dsId) {
            this.object = object;
            this.dsId = dsId;
        }

        @Override
        public Source read() {
            try {
                fetchData();
                return new StreamSource(new StringReader(data));
            } catch (Exception ex) {
                throw new IllegalStateException(toLogString(), ex);
            }
        }

        @Override
        public long getLastModified() {
            try {
                fetchProfile();
                return lastModified;
            } catch (FedoraClientException ex) {
                throw new IllegalStateException(toLogString(), ex);
            }
        }

        @Override
        public String getMimetype() {
            try {
                fetchProfile();
                return profile.getDsMIME();
            } catch (FedoraClientException ex) {
                throw new IllegalStateException(toLogString(), ex);
            }
        }

        @Override
        public void write(EditorResult data, long timestamp) {
            if (!(data instanceof EditorStreamResult)) {
                throw new IllegalArgumentException("Unsupported data: " + data);
            }

            if (timestamp != getLastModified()) {
                throw new ConcurrentModificationException(String.format(
                        "%s, timestamp: %s (%s)",
                        toLogString(), timestamp,
                        DateUtility.getXSDDateTime(new Date(timestamp))
                        ));
            }
            EditorStreamResult result = (EditorStreamResult) data;
            this.data = result.asString();
            object.register(this);
            modified = true;
        }

        @Override
        public EditorResult createResult() {
            return new EditorStreamResult();
        }

        private void fetchProfile() throws FedoraClientException {
            if (profile != null) {
                return ;
            }
            GetDatastreamResponse response = FedoraClient.getDatastream(object.getPid(), dsId)
                    .format("xml").execute(object.getClient());
            if (response.getStatus() != 200) {
                throw new FedoraClientException(response.getStatus(), toLogString());
            }
            profile = response.getDatastreamProfile();
            lastModified = response.getLastModifiedDate().getTime();
        }

        private void fetchData() throws FedoraClientException, IOException {
            if (data != null) {
                return ;
            }
            fetchProfile();
            FedoraResponse response = FedoraClient.getDatastreamDissemination(object.getPid(), dsId)
                    // ensure that it is content for given profile
                    .asOfDateTime(DateUtility.getXSDDateTime(new Date(lastModified)))
                    .execute(object.getClient());
            if (response.getStatus() != 200) {
                throw new FedoraClientException(response.getStatus(), toLogString());
            }
            data = response.getEntity(String.class);
        }

        @Override
        public void flush() {
            if (!modified) {
                return ;
            }
            try {
                ModifyDatastreamResponse response = FedoraClient.modifyDatastream(object.getPid(), dsId)
                        .dsLabel(profile.getDsLabel())
                        .formatURI(profile.getDsFormatURI())
                        .lastModifiedDate(new Date(lastModified))
                        .content(data)
                        .execute(object.getClient());
                if (response.getStatus() != 200) {
                    throw new FedoraClientException(response.getStatus(), toLogString());
                }
                modified = false;
                profile = response.getDatastreamProfile();
                lastModified = response.getLastModifiedDate().getTime();
            } catch (FedoraClientException ex) {
                // HTTP 409 - conflict with the current state of the resource
                if (ex.getStatus() == 409) {
                    throw new ConcurrentModificationException(ex.getMessage());
                }
                throw new IllegalStateException(toLogString(), ex);
            }
        }

        private String toLogString() {
            return String.format("%s/%s, lastModified: %s (%s)", object.getPid(), dsId, lastModified,
                    DateUtility.getXSDDateTime(new Date(lastModified)));
        }

    }

    private static final class EditorStreamResult extends StreamResult implements EditorResult {

        public EditorStreamResult() {
            super(new StringWriter());
        }

        public String asString() {
            return ((StringWriter) getWriter()).toString();
        }

    }

}
