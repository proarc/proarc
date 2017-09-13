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
package cz.cas.lib.proarc.common.fedora;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.FedoraCredentials;
import com.yourmediashelf.fedora.client.request.AddDatastream;
import com.yourmediashelf.fedora.client.request.ModifyDatastream;
import com.yourmediashelf.fedora.client.response.AddDatastreamResponse;
import com.yourmediashelf.fedora.client.response.DatastreamProfileResponse;
import com.yourmediashelf.fedora.client.response.DescribeRepositoryResponse;
import com.yourmediashelf.fedora.client.response.FedoraResponse;
import com.yourmediashelf.fedora.client.response.GetDatastreamResponse;
import com.yourmediashelf.fedora.client.response.GetDatastreamsResponse;
import com.yourmediashelf.fedora.client.response.IngestResponse;
import com.yourmediashelf.fedora.client.response.ListDatastreamsResponse;
import com.yourmediashelf.fedora.client.response.ModifyDatastreamResponse;
import com.yourmediashelf.fedora.client.response.PurgeDatastreamResponse;
import com.yourmediashelf.fedora.generated.access.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.StateType;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import com.yourmediashelf.fedora.util.DateUtility;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils.ControlGroup;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.object.DigitalObjectExistException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import javax.ws.rs.core.Response.Status;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.glassfish.jersey.uri.UriComponent;

/**
 * Fedora remote storage.
 *
 * @author Jan Pokorsky
 */
public final class RemoteStorage {

    private static final Logger LOG = Logger.getLogger(RemoteStorage.class.getName());
    private static final Pattern RE_OBJECT_EXISTS = Pattern.compile("WebApplicationException.*status: 500, message:.*already exists");
    private static RemoteStorage INSTANCE;

    private final FedoraClient client;
    private DescribeRepositoryResponse fedoraDescription;

    public RemoteStorage(FedoraClient client) {
        this.client = client;
    }

    public static void setInstance(RemoteStorage rs) {
        INSTANCE = rs;
    }

    public static RemoteStorage getInstance() {
        if (INSTANCE == null) {
            throw new IllegalStateException("RemoteStorage not initialized!");
        }
        return INSTANCE;
    }

    public static RemoteStorage getInstance(AppConfiguration conf) throws IOException {
        if (INSTANCE == null) {
            INSTANCE = new RemoteStorage(new FedoraClient(new FedoraCredentials(
                    conf.getFedoraUrl(), conf.getFedoraUsername(), conf.getFedoraPassword())));
        }
        return INSTANCE;
    }

    public RemoteObject find(String pid) {
        return new RemoteObject(pid, client);
    }

    public boolean exist(String pid) throws DigitalObjectException {
        try {
            getClient().getLastModifiedDate(pid);
            return true;
        } catch (FedoraClientException ex) {
            if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                return false;
            }
            throw new DigitalObjectException(pid, ex);
        }
    }

    public SearchView getSearch(Locale locale) {
        SearchView sv = new SearchView(this);
        if (locale != null) {
            sv.setLocale(locale);
        }
        return sv;
    }

    public SearchView getSearch() {
        return getSearch(null);
    }

    /**
     * Ingests a digital object stored in a file.
     * @param foxml persisted digital object
     * @param pid PID of the digital object
     * @param ingestUser ignored in case the owner is already set for the object
     * @param log message describing the action
     * @throws DigitalObjectException failure
     */
    public void ingest(File foxml, String pid, String ingestUser, String log) throws DigitalObjectException, DigitalObjectExistException {
        if (ingestUser == null || ingestUser.isEmpty()) {
            throw new IllegalArgumentException("ingestUser");
        }
        try {
            IngestResponse response = FedoraClient.ingest(pid)
                    .format("info:fedora/fedora-system:FOXML-1.1")
                    .logMessage(qpEncode(log))
                    .content(foxml)
                    .ownerId(ingestUser)
                    .execute(client);
            if (response.getStatus() != 201) {
                // XXX
            }
            LOG.log(Level.FINE, "{0}, {1}", new Object[]{response.getPid(), response.getLocation()});
        } catch (FedoraClientException ex) {
            checkObjectExistException(ex, pid);
            throw new DigitalObjectException(pid, null, null, null, ex);
        }
    }

    /**
     * Ingests a digital object with default log message.
     * @param object digital object to ingest
     * @param ingestUser ignored in case the owner is already set for the object
     * @throws DigitalObjectException failure
     */
    public void ingest(LocalObject object, String ingestUser) throws DigitalObjectException, DigitalObjectExistException {
        ingest(object, ingestUser, "Ingested locally");
    }

    /**
     * Ingests a digital object with default log message.
     * <p>See https://wiki.duraspace.org/display/FEDORA35/Using+File+URIs to reference external files for ingest.
     * @param object digital object to ingest
     * @param ingestUser ignored in case the owner is already set for the object
     * @param log message describing the action
     * @throws DigitalObjectException failure
     */
    public void ingest(LocalObject object, String ingestUser, String log) throws DigitalObjectException, DigitalObjectExistException {
        if (ingestUser == null || ingestUser.isEmpty()) {
            throw new IllegalArgumentException("ingestUser");
        }
        if (log == null || ingestUser.isEmpty()) {
            throw new IllegalArgumentException("log");
        }
        if (object.getOwner() == null) {
            // use the ownerId property as fedora ignores REST param ownerId
            // in case of ingesting an object WITH contents
            object.setOwner(ingestUser);
        }
        DigitalObject digitalObject = object.getDigitalObject();

        String xml = FoxmlUtils.toXml(digitalObject, false);
        try {
            IngestResponse response = FedoraClient.ingest(object.getPid())
                    .format("info:fedora/fedora-system:FOXML-1.1")
                    .logMessage(qpEncode(log))
//                    .namespace("")
//                    .xParam("", "")
                    .content(xml)
                    .execute(client);
//            if (response.getStatus() != 201) {
//                // XXX
//            }
            LOG.log(Level.FINE, "{0}, {1}", new Object[]{response.getPid(), response.getLocation()});
        } catch (FedoraClientException ex) {
            checkObjectExistException(ex, object.getPid());
            throw new DigitalObjectException(object.getPid(), null, null, ex.getMessage(), ex);
        }
    }

    /**
     * Is the storage compatible with a version?
     * @param version the requested version
     * @return {@code true} if the repository is compatible.
     * @throws FedoraClientException failure
     */
    public boolean isCompatible(String version) throws FedoraClientException {
        DescribeRepositoryResponse desc = getRepositoryDescription(true);
        String repositoryVersion = desc.getRepositoryVersion();
        return repositoryVersion.compareTo(version) >= 0;
    }

    FedoraClient getClient() {
        return client;
    }

    private static void checkObjectExistException(FedoraClientException ex, String pid) throws DigitalObjectExistException {
        // XXX hack: Fedora server does not notify existing object conflict with HTTP 409.
        // The workaround parses an error message.
        // Requires to add org.apache.cxf.jaxrs.impl.WebApplicationExceptionMapper/addMessageToResponse=true
        // in server/config/spring/web/jaxrs/objects-jaxrs.xml
        // Check for existence before ingest would be insufficient as Fedora does not yet support transactions.
        String errMsg = ex.getMessage();
        if (errMsg != null && RE_OBJECT_EXISTS.matcher(errMsg).find()) {
            throw new DigitalObjectExistException(pid, null, "Object already exists!", ex);
        }
    }

    DescribeRepositoryResponse getRepositoryDescription(boolean cache) throws FedoraClientException {
        if (fedoraDescription == null || !cache) {
            DescribeRepositoryResponse response = FedoraClient.describeRepository().execute(client);
            fedoraDescription = response;
        }
        return fedoraDescription;
    }

    /**
     * Encodes query parameter as Jersey 2 uses query templates by default.
     */
    private static String qpEncode(String p) {
        return p == null || p.isEmpty()
                ? p
                : UriComponent.encode(p, UriComponent.Type.QUERY_PARAM_SPACE_ENCODED);
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
        public XmlStreamEditor getEditor(DatastreamProfile datastream) {
            return new RemoteXmlStreamEditor(this, datastream);
        }

        @Override
        public void setLabel(String label) {
            if (label == null) {
                throw new NullPointerException();
            } else if (label.length() > 255) {
                // length 255 is Fedora limit
                label = label.substring(0, 255);
            }
            this.label = label;
        }

        @Override
        public void flush() throws DigitalObjectException {
            super.flush();
            try {
                if (label != null) {
                    FedoraClient.modifyObject(getPid()).label(qpEncode(label)).execute(client);
                }
            } catch (FedoraClientException ex) {
                throw new IllegalStateException(getPid(), ex);
            }
        }

        public void delete(String logMessage) throws DigitalObjectException {
            try {
                FedoraClient.modifyObject(getPid()).state(StateType.D.value())
                        .logMessage(qpEncode(logMessage))
                        .execute(client);
            } catch (FedoraClientException ex) {
                if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                    throw new DigitalObjectNotFoundException(getPid(), ex);
                } else {
                    throw new DigitalObjectException(getPid(), ex);
                }
            }
        }

        public void purge(String logMessage) throws DigitalObjectException {
            try {
                FedoraClient.purgeObject(getPid()).logMessage(qpEncode(logMessage)).execute(client);
            } catch (FedoraClientException ex) {
                if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                    throw new DigitalObjectNotFoundException(getPid(), ex);
                } else {
                    throw new DigitalObjectException(getPid(), ex);
                }
            }
        }

        @Override
        public void purgeDatastream(String datastream ,String logMessage) throws DigitalObjectException {
            try {
                FedoraClient.purgeDatastream(getPid(), datastream).logMessage(qpEncode(logMessage)).execute(client);
            } catch (FedoraClientException ex) {
                //if submitted pid was invalid, then 404 was received and client set NOT_FOUND status
                //datastream presence is not checked and 200 is returned by Fedora whether datastream existed or not prior to purging

                if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                    throw new DigitalObjectNotFoundException(getPid(), ex);
                } else {
                    throw new DigitalObjectException(getPid(), ex);
                }
            }
        }

        @Override
        public String asText() throws DigitalObjectException {
            try {
                FedoraResponse response = FedoraClient.getObjectXML(getPid()).execute(client);
                return response.getEntity(String.class);
            } catch (FedoraClientException ex) {
                if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                    throw new DigitalObjectNotFoundException(getPid(), ex);
                }
                throw new DigitalObjectException(getPid(), ex);
            }
        }

        @Override
        public List<DatastreamProfile> getStreamProfile(String dsId) throws DigitalObjectException {
            if (dsId == null) {
                return getDatastreams();
            } else {
                return getDatastreamImpl(dsId);
            }
        }

        private List<DatastreamProfile> getDatastreamImpl(String dsId) throws DigitalObjectException {
            try {
                GetDatastreamResponse response = FedoraClient.getDatastream(getPid(), dsId)
                        .format("xml").execute(client);
                DatastreamProfile profile = response.getDatastreamProfile();
                return Collections.singletonList(profile);
            } catch (FedoraClientException ex) {
                if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                    if (FoxmlUtils.missingDatastream(ex)) {
                        // log
                        return Collections.emptyList();
                    }
                    throw new DigitalObjectNotFoundException(getPid(), ex);
                }
                throw new DigitalObjectException(getPid(), null, dsId, null, ex);
            }
        }

        /**
         * API-M getDatastreams is not implemented in Fedora 3.5 and older.
         */
        private List<DatastreamProfile> getDatastreams3_5() throws DigitalObjectException {
            try {
//                FedoraResponse r = FedoraClient.getObjectXML(getPid()).execute(client);
                ListDatastreamsResponse response = FedoraClient.listDatastreams(getPid()).execute(client);
                List<DatastreamType> datastreams = response.getDatastreams();
                ArrayList<DatastreamProfile> profiles = new ArrayList<DatastreamProfile>(datastreams.size());
                for (DatastreamType datastream : datastreams) {
                    profiles.add(FoxmlUtils.toDatastreamProfile(getPid(), datastream));
                }
                return profiles;
            } catch (FedoraClientException ex) {
                if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                    throw new DigitalObjectNotFoundException(getPid(), ex);
                }
                throw new DigitalObjectException(getPid(), ex);
            }
        }

        public List<DatastreamProfile> getDatastreams() throws DigitalObjectException {
            try {
                if (!RemoteStorage.getInstance().isCompatible("3.6")) {
                    return getDatastreams3_5();
                }
                GetDatastreamsResponse response = FedoraClient.getDatastreams(getPid()).execute(client);
                List<DatastreamProfile> profiles = response.getDatastreamProfiles();
                return profiles;
            } catch (FedoraClientException ex) {
                if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                    throw new DigitalObjectNotFoundException(getPid(), ex);
                }
                throw new DigitalObjectException(getPid(), ex);
            }
        }

    }

    /**
     * The editor to access remote streams.
     */
    public static final class RemoteXmlStreamEditor implements XmlStreamEditor {

        private final RemoteObject object;
        private final String dsId;
        private long lastModified;
        private DatastreamProfile profile;
        private DatastreamProfile newProfile;
        private DatastreamContent data;
        private boolean modified;
        private boolean missingDataStream;
        private final DatastreamProfile defaultProfile;
        private String logMessage;

        /**
         * Constructor.
         * @param object remote object
         * @param defaultProfile optional profile to create new stream. {@code null}
         *          means the editor can just edit existing stream.
         */
        public RemoteXmlStreamEditor(RemoteObject object, DatastreamProfile defaultProfile) {
            if (object == null) {
                throw new NullPointerException("object");
            }
            this.object = object;
            defaultProfile.setPid(object.getPid());
            this.defaultProfile = defaultProfile;
            this.dsId = defaultProfile.getDsID();
        }

        /**
         * Creates editor that will fail in case the stream not exist.
         * @param object remote object
         * @param dsId data stream ID
         */
        public RemoteXmlStreamEditor(RemoteObject object, String dsId) {
            this.object = object;
            this.dsId = dsId;
            this.defaultProfile = null;
        }

        @Override
        public Source read() throws DigitalObjectException {
            try {
                fetchData();
                return data == null ? null : data.asSource();
            } catch (DigitalObjectException ex) {
                throw ex;
            } catch (Exception ex) {
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            }
        }

        @Override
        public InputStream readStream() throws DigitalObjectException {
            try {
                fetchData();
                return data == null ? null : data.asInputStream();
            } catch (DigitalObjectException ex) {
                throw ex;
            } catch (Exception ex) {
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            }
        }

        @Override
        public long getLastModified() throws DigitalObjectException {
            try {
                fetchProfile();
                return lastModified;
            } catch (DigitalObjectException ex) {
                throw ex;
            } catch (Exception ex) {
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            }
        }

        @Override
        public DatastreamProfile getProfile() throws DigitalObjectException {
            fetchProfile();
            return newProfile != null ? newProfile : profile;
        }

        @Override
        public void setProfile(DatastreamProfile profile) throws DigitalObjectException {
            this.newProfile = profile;
            object.register(this);
            modified = true;
        }

        @Override
        public void write(EditorResult data, long timestamp, String message) throws DigitalObjectException {
            if (!(data instanceof EditorStreamResult)) {
                throw new IllegalArgumentException("Unsupported data: " + data);
            }

            EditorStreamResult result = (EditorStreamResult) data;
            write(new DatastreamContent(result.asBytes()), timestamp, message);
        }

        @Override
        public void write(byte[] data, long timestamp, String message) throws DigitalObjectException {
            write(new DatastreamContent(data), timestamp, message);
        }

        @Override
        public void write(InputStream data, long timestamp, String message) throws DigitalObjectException {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            try {
                FoxmlUtils.copy(data, buf);
            } catch (IOException ex) {
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            } finally {
                FoxmlUtils.closeQuietly(data, toLogString());
            }
            write(new DatastreamContent(buf.toByteArray()), timestamp, message);
        }

        @Override
        public void write(URI data, long timestamp, String message) throws DigitalObjectException {
            write(new DatastreamContent(data), timestamp, message);
        }

        private void write(DatastreamContent data, long timestamp, String message) throws DigitalObjectException {
            if (timestamp != getLastModified()) {
                String msg = String.format(
                        "%s, timestamp: %s (%s)",
                        toLogString(), timestamp,
                        DateUtility.getXSDDateTime(new Date(timestamp)));
                throw new DigitalObjectConcurrentModificationException(object.getPid(), msg);
            }
            this.data = data;
            this.logMessage = message;
            object.register(this);
            modified = true;
        }

        @Override
        public EditorResult createResult() {
            return new EditorStreamResult();
        }

        private void fetchProfile() throws DigitalObjectException {
            if (profile != null || missingDataStream) {
                return ;
            }
            try {
                GetDatastreamResponse response = FedoraClient.getDatastream(object.getPid(), dsId)
                        .format("xml").execute(object.getClient());
                profile = response.getDatastreamProfile();
                profile = normalizeProfile(profile);
                lastModified = response.getLastModifiedDate().getTime();
                missingDataStream = false;
            } catch (FedoraClientException ex) {
                if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                    // Missing datastream message:
                    // HTTP 404 Error: No datastream could be found. Either there is no datastream for the digital object "uuid:5c3caa12-1e82-4670-a6aa-3d9ff8a7a3c5" with datastream ID of "TEXT_OCR"  OR  there are no datastreams that match the specified date/time value of "null".
                    // Missing object message:
                    // HTTP 404 Error: uuid:5c3caa12-1e82-4670-a6aa-3d9ff8a7a3c56
                    // To check message see fcrepo-server/src/main/java/org/fcrepo/server/rest/DatastreamResource.java
                    missingDataStream = FoxmlUtils.missingDatastream(ex);
                    lastModified = -1;
                    if (missingDataStream) {
                        if (defaultProfile != null) {
                            profile = defaultProfile;
                        } else {
                            throw new DigitalObjectException(object.getPid(),
                                    "Missing default profile! " + toLogString());
                        }
                    } else {
                        throw new DigitalObjectNotFoundException(object.getPid());
                    }
                } else {
                    throw new DigitalObjectException(object.getPid(), toLogString(), ex);
                }
            }
        }

        private DatastreamProfile normalizeProfile(DatastreamProfile profile) {
            // set empty format to null
            String format = profile.getDsFormatURI();
            profile.setDsFormatURI(format != null && format.isEmpty() ? null : format);
            return profile;
        }

        private void fetchData() throws DigitalObjectException {
            if (data != null || missingDataStream) {
                return ;
            }
            fetchProfile();
            if (missingDataStream) {
                return ;
            }
            try {
                FedoraResponse response = FedoraClient.getDatastreamDissemination(object.getPid(), dsId)
                        // ensure that it is content for given profile
                        .asOfDateTime(DateUtility.getXSDDateTime(new Date(lastModified)))
                        .execute(object.getClient());
                InputStream is = response.getEntity(InputStream.class);
                try {
                   ByteArrayOutputStream buffer = new ByteArrayOutputStream();
                   FoxmlUtils.copy(is, buffer);
                   this.data = new DatastreamContent(buffer.toByteArray());
                } catch (IOException ex) {
                    throw new DigitalObjectException(object.getPid(), ex);
                } finally {
                    FoxmlUtils.closeQuietly(is, toLogString());
                }
            } catch (FedoraClientException ex) {
                if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                }
                throw new DigitalObjectException(object.getPid(), ex);
            }
        }

        @Override
        public void flush() throws DigitalObjectException {
            if (!modified) {
                return ;
            }
            try {
                if (newProfile != null && !newProfile.getDsControlGroup().equals(profile.getDsControlGroup())) {
                    // It seems the fedora implementation cannot change the control group (3.5-3.8).
                    // Purge the stream to change the control group.
                    purgeDataStream(profile);
                    missingDataStream = true;
                }
                DatastreamProfileResponse response = missingDataStream
                        ? addDataStream() : modifyDataStream();
                missingDataStream = false;
                modified = false;
                logMessage = null;
                newProfile = null;
                profile = response.getDatastreamProfile();
                profile = normalizeProfile(profile);
                lastModified = response.getLastModifiedDate().getTime();
            } catch (IOException ex) {
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            } catch (FedoraClientException ex) {
                // HTTP 409 - conflict with the current state of the resource
                if (ex.getStatus() == Status.CONFLICT.getStatusCode()) {
                    throw new DigitalObjectConcurrentModificationException(object.getPid(), ex.getMessage());
                }
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            }
        }

        private void purgeDataStream(DatastreamProfile p) throws FedoraClientException, DigitalObjectConcurrentModificationException {
            PurgeDatastreamResponse response = FedoraClient.purgeDatastream(p.getPid(), p.getDsID())
                    .logMessage(qpEncode(logMessage))
                    // null would purge the entire history
                    .endDT(new Date(lastModified))
                    .execute(object.getClient());
            if (response.getPurgedDates().isEmpty()) {
                throw new DigitalObjectConcurrentModificationException(p.getPid(), toLogString());
            }
        }

        private DatastreamProfileResponse addDataStream() throws FedoraClientException, IOException {
            DatastreamProfile profile = newProfile != null ? newProfile : this.profile;
            AddDatastream request = FedoraClient.addDatastream(object.getPid(), profile.getDsID())
                    .controlGroup(profile.getDsControlGroup())
                    .dsLabel(profile.getDsLabel())
                    .dsState("A")
                    .formatURI(profile.getDsFormatURI())
                    .logMessage(qpEncode(logMessage))
                    .mimeType(profile.getDsMIME())
                    .versionable(Boolean.parseBoolean(profile.getDsVersionable()));

            ControlGroup control = ControlGroup.fromExternal(profile.getDsControlGroup());
            if (control == ControlGroup.INLINE || control == ControlGroup.MANAGED) {
                request.content(data.asInputStream());
            } else if (control == ControlGroup.EXTERNAL) {
                request.dsLocation(data.reference.toASCIIString());
            } else {
                throw new UnsupportedOperationException("DsControlGroup: " + control + "; " + toLogString());
            }
            AddDatastreamResponse response = request.execute(object.getClient());
            return response;
        }

        private DatastreamProfileResponse modifyDataStream() throws FedoraClientException, IOException {
            DatastreamProfile profile = newProfile != null ? newProfile : this.profile;
            ModifyDatastream request = FedoraClient.modifyDatastream(object.getPid(), dsId)
                    .controlGroup(profile.getDsControlGroup())
                    .dsLabel(profile.getDsLabel())
                    .formatURI(profile.getDsFormatURI())
                    .lastModifiedDate(new Date(lastModified))
                    .logMessage(qpEncode(logMessage))
                    .mimeType(profile.getDsMIME())
                    // enforce change with query parameter
                    .xParam("mimeType", profile.getDsMIME());

            // some profile changes (MIME) cannot be written without contents!
            if (data != null) {
                ControlGroup control = ControlGroup.fromExternal(profile.getDsControlGroup());
                if (control == ControlGroup.INLINE || control == ControlGroup.MANAGED) {
                    request.content(data.asInputStream());
                } else if (control == ControlGroup.EXTERNAL) {
                    request.dsLocation(data.reference.toASCIIString());
                } else {
                    throw new UnsupportedOperationException("DsControlGroup: " + control + "; " + toLogString());
                }
            }
            ModifyDatastreamResponse response = request.execute(object.getClient());
            return response;
        }

        private String toLogString() {
            return String.format("%s/%s, lastModified: %s (%s)", object.getPid(), dsId, lastModified,
                    DateUtility.getXSDDateTime(new Date(lastModified)));
        }

        private static final class DatastreamContent {

            private byte[] bytes;
            private URI reference;

            public DatastreamContent(byte[] bytes) {
                this.bytes = bytes;
            }

            public DatastreamContent(URI reference) {
                this.reference = reference;
            }

            public Source asSource() {
                if (bytes != null) {
                    return new StreamSource(new ByteArrayInputStream(bytes));
                } else if (reference != null) {
                    return new StreamSource(reference.toASCIIString());
                } else {
                    return null;
                }
            }

            public InputStream asInputStream() throws IOException {
                if (bytes != null) {
                    return new ByteArrayInputStream(bytes);
                } else if (reference != null) {
                    return reference.toURL().openStream();
                } else {
                    return null;
                }
            }

        }

        private static final class EditorStreamResult extends StreamResult implements EditorResult {

            public EditorStreamResult() {
                super(new ByteArrayOutputStream());
            }

            public byte[] asBytes() {
                return ((ByteArrayOutputStream) getOutputStream()).toByteArray();
            }

        }

    }

}
