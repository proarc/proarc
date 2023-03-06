package cz.cas.lib.proarc.common.fedora.akubra;


import com.qbizm.kramerius.imp.jaxb.ContentLocationType;
import com.qbizm.kramerius.imp.jaxb.DatastreamType;
import com.qbizm.kramerius.imp.jaxb.DatastreamVersionType;
import com.qbizm.kramerius.imp.jaxb.DigitalObject;
import com.qbizm.kramerius.imp.jaxb.ObjectPropertiesType;
import com.qbizm.kramerius.imp.jaxb.PropertyType;
import com.qbizm.kramerius.imp.jaxb.StateType;
import com.qbizm.kramerius.imp.jaxb.XmlContentType;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import com.yourmediashelf.fedora.util.DateUtility;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.fedora.AbstractFedoraObject;
import cz.cas.lib.proarc.common.fedora.DigitalObjectConcurrentModificationException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectExistException;
import cz.incad.kramerius.fedora.om.Repository;
import cz.incad.kramerius.fedora.om.RepositoryException;
import cz.incad.kramerius.fedora.om.impl.AkubraDOManager;
import cz.incad.kramerius.fedora.om.impl.AkubraRepository;
import cz.incad.kramerius.utils.conf.KConfiguration;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPathFactory;
import org.apache.commons.io.IOUtils;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrClient;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.ehcache.CacheManager;
import org.ehcache.config.builders.CacheManagerBuilder;
import org.fcrepo.server.errors.LowlevelStorageException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_CREATEDATE;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_LABEL;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_LASTMODIFIED;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_STATE;
import static cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage.AkubraObject.getActualDateAsString;
import static cz.cas.lib.proarc.common.fedora.akubra.AkubraUtils.getDatastream;
import static cz.cas.lib.proarc.common.fedora.akubra.AkubraUtils.toXmlGregorian;


public class AkubraStorage {

    private static final Logger LOG = Logger.getLogger(AkubraStorage.class.getName());
    private XPathFactory xPathFactory;
    private KConfiguration configuration;
    private static SolrClient solrClient;
    private static AkubraStorage INSTANCE;
    private AkubraDOManager manager;
    private Repository repository;
    private SolrFeeder feeder;

    public static AkubraStorage getInstance() {
        if (INSTANCE == null) {
            throw new IllegalStateException("Akubra Storage not initialized!");
        }
        return INSTANCE;
    }

    public static AkubraStorage getInstance(AkubraConfiguration conf) throws IOException {
        if (INSTANCE == null) {
            String processingSolrHost = conf.getSolrProcessingHost();
            SolrClient solrClient = new ConcurrentUpdateSolrClient.Builder(processingSolrHost).withQueueSize(100).build();
            SolrFeeder feeder = new SolrFeeder(solrClient);
            CacheManager cacheManager = CacheManagerBuilder.newCacheManagerBuilder().build();
            cacheManager.init();

            INSTANCE = new AkubraStorage(conf, feeder, cacheManager);
        }
        return INSTANCE;
    }

    public AkubraStorage(AkubraConfiguration configuration,
                         SolrFeeder feeder,
                         CacheManager cacheManager) throws IOException {
        this.configuration = configuration;
        this.xPathFactory = XPathFactory.newInstance();
        try {
            String proccessingSolrHost = configuration.getSolrProcessingHost();
            SolrClient solrClient = new HttpSolrClient.Builder(proccessingSolrHost).build();
            this.solrClient = solrClient;
            this.manager = new AkubraDOManager(configuration, cacheManager);
            this.feeder = feeder;
            this.repository = AkubraRepository.build(feeder, this.manager);
        } catch (Exception e) {
            throw new IOException(e);
        }
    }

    public AkubraObject find(String pid) {
        return new AkubraObject(manager, feeder, pid);
    }

    public boolean exist(String pid) throws DigitalObjectException {
        try {
            return this.repository.objectExists(pid);
        } catch (RepositoryException e) {
            throw new DigitalObjectException(pid, e);
        }
    }


    public SolrSearchView getSearch(Locale locale) {
        SolrSearchView sv = new SolrSearchView(this, this.solrClient);
        if (locale != null) {
            sv.setLocale(locale);
        }
        return sv;
    }

    public SolrSearchView getSearch() {
        return getSearch(null);
    }


    public void ingest(File foxml, String pid, String ingestUser, String log) throws DigitalObjectException {
        if (ingestUser == null || ingestUser.isEmpty()) {
            throw new IllegalArgumentException("ingestUser");
        }
        if (pid == null || pid.isEmpty()) {
            throw new IllegalArgumentException("PID is null or does not exists.");
        }
        if (foxml == null || !foxml.exists()) {
            throw new IllegalArgumentException("Foxml is null or does not exists.");
        }
        try {
            if (exist(pid)) {
                throw new DigitalObjectExistException(pid, null, "Object with PID " + pid + " already exists!", null);
            }


            InputStream inputStream = new FileInputStream(foxml);
            this.manager.addOrReplaceObject(pid, inputStream);

            LOG.log(Level.FINE, "Object with PID {0} added to repository.", pid);
        } catch (FileNotFoundException | LowlevelStorageException e) {
            throw new DigitalObjectException(pid, "Error during adding new object", e);
        }
    }

    public void ingest(LocalObject object, String ingestUser) throws DigitalObjectException, DigitalObjectExistException {
        ingest(object, ingestUser, "Ingested locally");
    }

    public void ingest(LocalObject object, String ingestUser, String log) throws DigitalObjectException, DigitalObjectExistException {
        if (ingestUser == null || ingestUser.isEmpty()) {
            throw new IllegalArgumentException("ingestUser");
        }
        if (log == null || log.isEmpty()) {
            throw new IllegalArgumentException("log");
        }
        if (object == null) {
            throw new IllegalArgumentException("Local Object is null or does not exists.");
        }
        if (object.getOwner() == null) {
            object.setOwner(ingestUser);
        }
        try {
            if (exist(object.getPid())) {
                throw new DigitalObjectExistException(object.getPid(), null, "Object with PID " + object.getPid() + " already exists!", null);
            }

            com.yourmediashelf.fedora.generated.foxml.DigitalObject digitalObject = object.getDigitalObject();
            updateProperties(digitalObject.getObjectProperties());
            String xml = FoxmlUtils.toXml(digitalObject, false);
            InputStream inputStream = new ByteArrayInputStream(xml.getBytes());
            this.manager.addOrReplaceObject(object.getPid(), inputStream);
            indexDocument(object.getPid());

            LOG.log(Level.FINE, "Object with PID {0} added to repository.", object.getPid());
        } catch (LowlevelStorageException | IOException e) {
            throw new DigitalObjectException(object.getPid(), "Error during adding new object", e);
        }
    }

    private void updateProperties(com.yourmediashelf.fedora.generated.foxml.ObjectPropertiesType objectProperties) {
        boolean updateLastModified = false;
        boolean updateState = false;
        boolean updateCreated = false;
        for (com.yourmediashelf.fedora.generated.foxml.PropertyType property : objectProperties.getProperty()) {
            if (PROPERTY_LASTMODIFIED.equals(property.getNAME())) {
                property.setVALUE(getActualDateAsString());
                updateLastModified = true;
            } else if (PROPERTY_STATE.equals(property.getNAME())) {
                property.setVALUE(SolrUtils.PROPERTY_STATE_ACTIVE);
                updateState = true;
            } else if (PROPERTY_CREATEDATE.equals(property.getNAME())) {
                property.setVALUE(getActualDateAsString());
                updateCreated = true;
            }
        }
        if (!updateLastModified) {
            addProperty(objectProperties.getProperty(), PROPERTY_LASTMODIFIED, getActualDateAsString());
        }
        if (!updateCreated) {
            addProperty(objectProperties.getProperty(), PROPERTY_CREATEDATE, getActualDateAsString());
        }
        if (!updateState) {
            addProperty(objectProperties.getProperty(), PROPERTY_STATE, SolrUtils.PROPERTY_STATE_ACTIVE);
        }
    }

    private void addProperty(List<com.yourmediashelf.fedora.generated.foxml.PropertyType> properties, String key, String value) {
        com.yourmediashelf.fedora.generated.foxml.PropertyType property = new com.yourmediashelf.fedora.generated.foxml.PropertyType();
        property.setNAME(key);
        property.setVALUE(value);
        properties.add(property);
    }

    public void indexDocument(String pid) throws IOException, DigitalObjectException {
        AkubraObject aObject = find(pid);
        DigitalObject dObject = this.manager.readObjectFromStorage(pid);
        this.feeder.feedDescriptionDocument(dObject, aObject, true);
    }

    public static final class AkubraObject extends AbstractFedoraObject {

        private String label;
        private AkubraDOManager manager;
        private SolrFeeder feeder;


        public AkubraObject(AkubraDOManager manager, SolrFeeder feeder, String pid) {
            super(pid);
            this.manager = manager;
            this.feeder = feeder;
        }

        @Override
        public XmlStreamEditor getEditor(DatastreamProfile datastream) {
            return new AkubraXmlStreamEditor(this, datastream);
        }

        @Override
        public void setLabel(String label) {
            if (label == null) {
                throw new NullPointerException();
            } else if (label.length() > 255) {
                label = label.substring(0, 255);
            }
            this.label = label;
        }

        @Override
        public void flush() throws DigitalObjectException {
            super.flush();
            try {
                if (label != null) {
                    DigitalObject object = this.manager.readObjectFromStorage(getPid());
                    object = updateLabel(object, label);
                    object = updateModifiedDate(object);
                    if (object == null) {
                        throw new DigitalObjectException(getPid(), "Object " + getPid() + "is can not be flushed to Low-Level storage.");
                    } else {
                        InputStream inputStream = this.manager.marshallObject(object);
                        this.manager.addOrReplaceObject(object.getPID(), inputStream);
                        //this.manager.commit(object, null);
                        this.feeder.feedDescriptionDocument(object, this, true);
                    }
                }
            } catch (Exception ex) {
                throw new DigitalObjectException(getPid(), ex);
            }
        }

        private DigitalObject updateModifiedDate(DigitalObject object) {
            if (object != null) {
                ObjectPropertiesType propertiesType = object.getObjectProperties();
                if (propertiesType != null) {
                    for (PropertyType property : propertiesType.getProperty()) {
                        if (PROPERTY_LASTMODIFIED.equals(property.getNAME())) {
                            property.setVALUE(getActualDateAsString());
                        }
                    }
                }
                return object;
            }
            return null;
        }

        private DigitalObject updateState(DigitalObject object, String state) {
            if (object != null) {
                ObjectPropertiesType propertiesType = object.getObjectProperties();
                if (propertiesType != null) {
                    for (PropertyType property : propertiesType.getProperty()) {
                        if (PROPERTY_STATE.equals(property.getNAME())) {
                            property.setVALUE(state);
                        }
                    }
                }
                return object;
            }
            return null;
        }

        private DigitalObject updateLabel(DigitalObject object, String label) {
            if (object != null) {
                ObjectPropertiesType propertiesType = object.getObjectProperties();
                if (propertiesType != null) {
                    for (PropertyType property : propertiesType.getProperty()) {
                        if (PROPERTY_LABEL.equals(property.getNAME())) {
                            property.setVALUE(label);
                        }
                    }
                }
                return object;
            }
            return null;
        }

        public static String getActualDateAsString() {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
            return LocalDateTime.now().format(formatter);
//            DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
//            return formatter.format(LocalDateTime.now());
        }

        public void delete(String logMessage) throws DigitalObjectException {
            try {
                DigitalObject object = this.manager.readObjectFromStorage(getPid());
                object = updateState(object, SolrUtils.PROPERTY_STATE_DEACTIVE);
                object = updateModifiedDate(object);
                if (object == null) {
                    throw new DigitalObjectException(getPid(), "Object " + getPid() + "is can not be flushed to Low-Level storage.");
                } else {
                    InputStream inputStream = this.manager.marshallObject(object);
                    this.manager.addOrReplaceObject(object.getPID(), inputStream);
                    //this.manager.commit(object, null);
                    this.feeder.feedDescriptionDocument(object, this, true);

                }
            } catch (Exception ex) {
                throw new DigitalObjectException(getPid(), ex);
            }
        }

        public void restore(String logMessage) throws DigitalObjectException {
            try {
                DigitalObject object = this.manager.readObjectFromStorage(getPid());
                object = updateState(object, SolrUtils.PROPERTY_STATE_ACTIVE);
                object = updateModifiedDate(object);
                if (object == null) {
                    throw new DigitalObjectException(getPid(), "Object " + getPid() + "is can not be flushed to Low-Level storage.");
                } else {
                    InputStream inputStream = this.manager.marshallObject(object);
                    this.manager.addOrReplaceObject(object.getPID(), inputStream);
                    //this.manager.commit(object, null);
                    this.feeder.feedDescriptionDocument(object, this, true);
                }
            } catch (Exception ex) {
                throw new DigitalObjectException(getPid(), ex);
            }
        }

        public void purge(String logMessage) throws DigitalObjectException {
            try {
                this.manager.deleteObject(getPid(), true);
                this.feeder.deleteByPid(getPid());
            } catch (IOException | SolrServerException ex) {
                throw new DigitalObjectException(getPid(), ex);
            }
        }

        @Override
        public void purgeDatastream(String datastream, String logMessage) throws DigitalObjectException {
            try {
                this.manager.deleteStream(getPid(), datastream);
            } catch (IOException ex) {
                throw new DigitalObjectException(getPid(), ex);
            }
        }

        @Override
        public String asText() throws DigitalObjectException {
            try {
                InputStream stream = this.manager.retrieveObject(getPid());
                String text = new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8)).lines().collect(Collectors.joining("\n"));
                return text;
            } catch (IOException ex) {
                throw new DigitalObjectException(getPid(), ex);
            }
        }

        @Override
        public List<DatastreamProfile> getStreamProfile(String dsId) throws DigitalObjectException {
            if (dsId == null) {
                return getDatastreamProfiles();
            } else {
                return getDatastreamProfile(dsId);
            }
        }

        private List<DatastreamProfile> getDatastreamProfile(String dsId) throws DigitalObjectException {
            try {
                DigitalObject object = this.manager.readObjectFromStorage(getPid());
                DatastreamProfile profile = AkubraUtils.createDatastremProfile(object, dsId);
                return Collections.singletonList(profile);
            } catch (IOException ex) {
                throw new DigitalObjectException(getPid(), ex);
            }
        }

        private List<DatastreamProfile> getDatastreamProfiles() throws DigitalObjectException {
            try {
                DigitalObject object = this.manager.readObjectFromStorage(getPid());
                return AkubraUtils.createDatastremProfiles(object);
            } catch (IOException ex) {
                throw new DigitalObjectException(getPid(), ex);
            }
        }

        public AkubraDOManager getManager() {
            return manager;
        }

        public SolrFeeder getFeeder() {
            return feeder;
        }
    }

    public static final class AkubraXmlStreamEditor implements XmlStreamEditor {

        private final AkubraObject object;
        private final AkubraDOManager manager;
        private final SolrFeeder solrFeeder;
        private final String dsId;
        private long lastModified;
        private DatastreamProfile profile;
        private DatastreamProfile newProfile;
        private AkubraXmlStreamEditor.DatastreamContent data;
        private boolean modified;
        private boolean missingDataStream;
        private final DatastreamProfile defaultProfile;
        private String logMessage;

        public AkubraXmlStreamEditor(AkubraObject object, DatastreamProfile defaultProfile) {
            if (object == null) {
                throw new NullPointerException("object");
            }
            this.object = object;
            this.manager = object.getManager();
            this.solrFeeder = object.getFeeder();
            defaultProfile.setPid(object.getPid());
            this.defaultProfile = defaultProfile;
            this.dsId = defaultProfile.getDsID();
        }

        public AkubraXmlStreamEditor(AkubraObject object, String dsId, DatastreamProfile defaultProfile) {
            this.object = object;
            this.manager = object.getManager();
            this.solrFeeder = object.getFeeder();
            this.dsId = dsId;
            this.defaultProfile = defaultProfile;
        }


        private String toLogString() {
            return String.format("%s/%s, lastModified: %s (%s)", object.getPid(), dsId, lastModified,
                    DateUtility.getXSDDateTime(new Date(lastModified)));
        }

        private String toLogString(String message) {
            return String.format("%s/%s, lastModified: %s (%s), %s", object.getPid(), dsId, lastModified,
                    DateUtility.getXSDDateTime(new Date(lastModified)), message);
        }

        @Override
        public Source read() throws DigitalObjectException {
            try {
                fetchData();
                return data == null ? null : data.asSource();
            } catch (Exception ex) {
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            }
        }

        @Override
        public InputStream readStream() throws DigitalObjectException {
            try {
                fetchData();
                return data == null ? null : data.asInputStream();
            } catch (Exception ex) {
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            }
        }

        @Override
        public long getLastModified() throws DigitalObjectException {
            try {
                fetchProfile();
                return lastModified;
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
            if (data instanceof EditorStreamResult) {
                EditorStreamResult result = (EditorStreamResult) data;
                write(new DatastreamContent(result.asBytes()), timestamp, message);
            } else if (data instanceof EditorDomResult) {
                write(new DatastreamContent((EditorDomResult) data), timestamp, message);
            } else {
                throw new IllegalArgumentException("Unsupported data: " + data);
            }
        }

        @Override
        public void write(byte[] data, long timestamp, String message) throws DigitalObjectException {
            write(new DatastreamContent(data), timestamp, message);

        }

        @Override
        public void write(URI data, long timestamp, String message) throws DigitalObjectException {
            write(new DatastreamContent(data), timestamp, message);

        }

        @Override
        public void write(InputStream data, long timestamp, String message) throws DigitalObjectException {
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            try {
                FoxmlUtils.copy(data, byteArrayOutputStream);
            } catch (IOException ex) {
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            } finally {
                FoxmlUtils.closeQuietly(data, toLogString());
            }
            write(new DatastreamContent(byteArrayOutputStream.toByteArray()), timestamp, message);
        }

        @Override
        public EditorResult createResult() {
            if (ModsStreamEditor.DATASTREAM_ID.equals(dsId) || DcStreamEditor.DATASTREAM_ID.equals(dsId)) {
                return new EditorDomResult();
            }
            return new EditorStreamResult();
        }

        @Override
        public void flush() throws DigitalObjectException {
            if (!modified) {
                return;
            }
            try {

                if (newProfile != null && !newProfile.getDsControlGroup().equals(profile.getDsControlGroup())) {
                    purgeDatastream(profile);
                    missingDataStream = true;
                }
                if (missingDataStream) {
                    addDatastream();
                } else {
                    modifyDatastream();
                }
                missingDataStream = false;
                modified = false;
                logMessage = null;
                newProfile = null;
                DigitalObject digitalObject = this.manager.readObjectFromStorage(this.object.getPid());
                if (!(ModsStreamEditor.DATASTREAM_ID.equals(dsId) || DcStreamEditor.DATASTREAM_ID.equals(dsId))) {
                    this.solrFeeder.feedDescriptionDocument(digitalObject, this.object, true);
                }
                profile = AkubraUtils.createDatastremProfile(digitalObject, dsId);
                lastModified = AkubraUtils.getLastModified(digitalObject, dsId);
            } catch (Exception ex) {
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            }
        }

        private void modifyDatastream() throws Exception, DigitalObjectException {
            DatastreamProfile profile = newProfile != null ? newProfile : this.profile;
            DigitalObject object = this.manager.readObjectFromStorage(this.object.getPid());

            DatastreamType datastreamType = getDatastream(object, profile);
            if (datastreamType == null) {
                throw new DigitalObjectException(object.getPID(), toLogString("Missing datastream."));
            }

            datastreamType = modifyDatastream(datastreamType, profile);

            object = replaceDatastream(object, datastreamType);

            InputStream inputStream = this.manager.marshallObject(object);
            this.manager.addOrReplaceObject(object.getPID(), inputStream);
            //this.manager.commit(object, profile.getDsID());
        }

        private DigitalObject replaceDatastream(DigitalObject object, DatastreamType datastreamType) {

            ListIterator<DatastreamType> iterator = object.getDatastream().listIterator();
            while (iterator.hasNext()) {
                if (iterator.next().getID().equals(datastreamType.getID())) {
                    iterator.remove();
                    break;
                }
            }
            object.getDatastream().add(datastreamType);
            return object;
        }

        private void addDatastream() throws Exception {
            DatastreamProfile profile = newProfile != null ? newProfile : this.profile;

            DigitalObject object = this.manager.readObjectFromStorage(this.object.getPid());
            DatastreamType datastreamType = createNewDatastream(profile);
            object.getDatastream().add(datastreamType);
            InputStream inputStream = this.manager.marshallObject(object);
            this.manager.addOrReplaceObject(object.getPID(), inputStream);
            //this.manager.commit(object, profile.getDsID());
        }

        private DatastreamType modifyDatastream(DatastreamType datastreamType, DatastreamProfile profile) throws IOException, DigitalObjectException, ParserConfigurationException, SAXException {
            datastreamType.setCONTROLGROUP(profile.getDsControlGroup());
            datastreamType.setID(profile.getDsID());

            if (datastreamType.getDatastreamVersion().isEmpty() || datastreamType.getDatastreamVersion().size() > 1) {
                throw new DigitalObjectException(object.getPid(), toLogString("Missing or more than 1 datastreamVersionType"));
            }

            DatastreamVersionType datastreamVersionType = datastreamType.getDatastreamVersion().get(0);

            datastreamVersionType.setID(profile.getDsVersionID());
            datastreamVersionType.setLABEL(profile.getDsLabel());
            try {
                if (datastreamVersionType.getCREATED() == null) {
                    datastreamVersionType.setCREATED(toXmlGregorian(new Date(this.lastModified)));
                }
            } catch (DatatypeConfigurationException ex) {
                throw new DigitalObjectException(this.object.getPid(), toLogString(), ex);
            }
            datastreamVersionType.setFORMATURI(profile.getDsFormatURI());
            datastreamVersionType.setMIMETYPE(profile.getDsMIME());

            if (this.data != null) {
                FoxmlUtils.ControlGroup controlGroup = FoxmlUtils.ControlGroup.fromExternal(profile.getDsControlGroup());
                if (controlGroup == FoxmlUtils.ControlGroup.INLINE) {
                    datastreamVersionType.setXmlContent(data.asXmlContent());
                } else if (controlGroup == FoxmlUtils.ControlGroup.MANAGED) {
                    datastreamVersionType.setBinaryContent(IOUtils.toByteArray(this.data.asInputStream()));
                } else if (controlGroup == FoxmlUtils.ControlGroup.EXTERNAL) {
                    ContentLocationType contentLocation = new ContentLocationType();
                    datastreamVersionType.setContentLocation(contentLocation);
                    contentLocation.setREF(this.data.reference.toASCIIString());
                    contentLocation.setTYPE("URL");
                } else {
                    throw new UnsupportedOperationException("DsControlGroup: " + controlGroup + "; " + toLogString());
                }
            }
            return datastreamType;
        }

        private DatastreamType createNewDatastream(DatastreamProfile profile) throws IOException, ParserConfigurationException, SAXException {
            DatastreamType datastreamType = new DatastreamType();
            datastreamType.setCONTROLGROUP(profile.getDsControlGroup());
            datastreamType.setID(profile.getDsID());
            datastreamType.setSTATE(StateType.A);
            datastreamType.setVERSIONABLE(Boolean.parseBoolean(profile.getDsVersionable()));

            DatastreamVersionType datastreamVersionType = new DatastreamVersionType();
            datastreamType.getDatastreamVersion().add(datastreamVersionType);

            datastreamVersionType.setID(profile.getDsVersionID());
            datastreamVersionType.setLABEL(profile.getDsLabel());
            datastreamVersionType.setCREATED(profile.getDsCreateDate());
            datastreamVersionType.setMIMETYPE(profile.getDsMIME());
            datastreamVersionType.setFORMATURI(profile.getDsFormatURI());

            if (this.data != null) {
                FoxmlUtils.ControlGroup controlGroup = FoxmlUtils.ControlGroup.fromExternal(profile.getDsControlGroup());
                if (controlGroup == FoxmlUtils.ControlGroup.INLINE) {
                    datastreamVersionType.setXmlContent(data.asXmlContent());
                } else if (controlGroup == FoxmlUtils.ControlGroup.MANAGED) {
                    datastreamVersionType.setBinaryContent(IOUtils.toByteArray(this.data.asInputStream()));
                } else if (controlGroup == FoxmlUtils.ControlGroup.EXTERNAL) {
                    ContentLocationType contentLocation = new ContentLocationType();
                    datastreamVersionType.setContentLocation(contentLocation);
                    contentLocation.setREF(this.data.reference.toASCIIString());
                    contentLocation.setTYPE("URL");
                } else {
                    throw new UnsupportedOperationException("DsControlGroup: " + controlGroup + "; " + toLogString());
                }
            }
            return datastreamType;
        }


        private void purgeDatastream(DatastreamProfile profile) throws IOException {
            manager.deleteStream(this.object.getPid(), profile.getDsID());
        }

        private void fetchProfile() throws DigitalObjectException {
            if (profile != null || missingDataStream) {
                return;
            }
            try {
                DigitalObject digitalObject = this.manager.readObjectFromStorage(this.object.getPid());
                if (object != null) {
                    this.lastModified = AkubraUtils.getLastModified(digitalObject, dsId);
                    this.profile = AkubraUtils.createDatastremProfile(digitalObject, dsId);
                    this.missingDataStream = false;
                } else {
                    throw new IOException("cannot find pid '" + this.object.getPid() + "'");
                }
            } catch (IOException ex) {
//                profile = defaultProfile;
//                profile = normalizeProfile(profile);
//                missingDataStream = false;
                throw new DigitalObjectException(object.getPid(), toLogString(), ex);
            }
        }

        private void write(DatastreamContent data, long timestamp, String message) throws DigitalObjectException {
            if (timestamp != getLastModified()) {
                String msg = String.format("%s, timestamp: %s (%s)", toLogString(), timestamp,
                        DateUtility.getXSDDateTime(new Date(timestamp)));
                throw new DigitalObjectConcurrentModificationException(object.getPid(), msg);
            }
            this.data = data;
            this.logMessage = message;
            object.register(this);
            modified = true;
        }

        private void fetchData() throws DigitalObjectException {
            try {
                DigitalObject object = manager.readObjectFromStorage(this.object.getPid());
                for (DatastreamType datastreamType : object.getDatastream()) {
                    if (dsId.equals(datastreamType.getID())) {
                        if (datastreamType.getDatastreamVersion() != null && !datastreamType.getDatastreamVersion().isEmpty() &&
                        datastreamType.getDatastreamVersion().get(0) != null) {
                            DatastreamVersionType datastreamVersionType = datastreamType.getDatastreamVersion().get(0);
                            if (datastreamVersionType.getXmlContent() != null && datastreamVersionType.getXmlContent().getAny() != null && !datastreamVersionType.getXmlContent().getAny().isEmpty()) {
                                Element node = datastreamVersionType.getXmlContent().getAny().get(0);
                                if (node != null) {
                                    LOG.fine("Created note from xmlContent");
                                    Source xmlSource = new DOMSource(node);
                                    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                                    Result outputTarget = new StreamResult(outputStream);
                                    TransformerFactory.newInstance().newTransformer().transform(xmlSource, outputTarget);
                                    this.data = new AkubraXmlStreamEditor.DatastreamContent(outputStream.toByteArray());
                                    break;
                                }
                            } else if (datastreamVersionType.getBinaryContent() != null) {
                                byte[] binaryContent = datastreamVersionType.getBinaryContent();
                                if (binaryContent != null) {
                                    this.data = new AkubraXmlStreamEditor.DatastreamContent(binaryContent);
                                    break;
                                }
                            } else if (datastreamVersionType.getContentLocation() != null) {
                                ContentLocationType contentLocationType = datastreamVersionType.getContentLocation();
                                if (contentLocationType != null) {
                                    String ref = contentLocationType.getREF();
                                    if (ref != null) {

                                        InputStream inputStream = this.manager.retrieveDatastream(ref);
                                        if (inputStream != null) {
                                            this.data = new AkubraXmlStreamEditor.DatastreamContent(IOUtils.toByteArray(inputStream));
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
//                InputStream is = manager.retrieveObject(this.object.getPid());
//                try {
//
//                    ByteArrayOutputStream buffer = new ByteArrayOutputStream();
//                    FoxmlUtils.copy(is, buffer);
//                    this.data = new AkubraXmlStreamEditor.DatastreamContent(buffer.toByteArray());
//                } catch (IOException ex) {
//                    throw new DigitalObjectException(this.object.getPid(), ex);
//                } finally {
//                    FoxmlUtils.closeQuietly(is, toLogString());
//                }
            } catch (Exception e) {
                throw new DigitalObjectException(object.getPid(), e);
            }
        }

        private static final class DatastreamContent {

            private byte[] bytes;
            private URI reference;
            private Element xmlElement;

            public DatastreamContent(byte[] bytes) {
                this.bytes = bytes;
            }

            public DatastreamContent(URI reference) {
                this.reference = reference;
            }

            public DatastreamContent(EditorDomResult data) {
                Node root = data.getNode();
                Document doc = root.getOwnerDocument() == null ? (Document) root : root.getOwnerDocument();
                this.xmlElement = doc.getDocumentElement();
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

            public XmlContentType asXmlContent() throws ParserConfigurationException, IOException, SAXException {
                if (xmlElement != null) {
                    XmlContentType xmlContentType = new XmlContentType();
                    xmlContentType.getAny().add(xmlElement);
                    return xmlContentType;
                }
                return null;
            }
        }

        private static final class EditorStreamResult extends StreamResult implements XmlStreamEditor.EditorResult {

            public EditorStreamResult() {
                super(new ByteArrayOutputStream());
            }

            public byte[] asBytes() {
                return ((ByteArrayOutputStream) getOutputStream()).toByteArray();
            }

        }

        private static final class EditorDomResult extends DOMResult implements EditorResult {

        }
    }
}
