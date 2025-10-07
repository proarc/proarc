package cz.cas.lib.proarc.webapp.server.rest.v1;

import com.google.gwt.http.client.Request;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.SolrObjectFeeder;
import cz.cas.lib.proarc.common.storage.akubra.SolrUtils;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.server.ServerMessages;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.shared.rest.IndexerResourceApi;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrClient;

import static cz.cas.lib.proarc.webapp.server.rest.UserPermission.checkPermission;
import static cz.cas.lib.proarc.webapp.server.rest.v1.DigitalObjectResourceV1.returnFunctionSuccess;

@Deprecated
@Path(RestConfig.URL_API_VERSION_1 + "/" + IndexerResourceApi.PATH)
public class IndexerResourceV1 {

    private static final Logger LOG = Logger.getLogger(IndexerResourceV1.class.getName());

    private final AppConfiguration appConfiguration;
    private final AkubraConfiguration akubraConfiguration;
    private final Request httpRequest;
    private final HttpHeaders httpHeaders;
    protected final UserProfile user;
    private final SessionContext session;
    private final BatchManager importManager;


    private static Unmarshaller unmarshaller;

    int filesCount = 0;
    int objectCount = 0;

    public IndexerResourceV1(
            @Context Request httpRequest,
            @Context SecurityContext securityContext,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpServletRequest) throws AppConfigurationException {
        this.httpRequest = httpRequest;
        this.httpHeaders = httpHeaders;
        this.appConfiguration = AppConfigurationFactory.getInstance().defaultInstance();
        if (Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfiguration.getConfigHome());
        } else {
            this.akubraConfiguration = null;
        }
        this.importManager = BatchManager.getInstance(appConfiguration);
        this.session = SessionContext.from(httpServletRequest);
        this.user = this.session.getUser();
        LOG.fine(user.toString());

        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(DigitalObject.class);
            this.unmarshaller = jaxbContext.createUnmarshaller();
        } catch (JAXBException e) {
            LOG.log(Level.SEVERE, "Cannot init JAXB", e);
            throw new RuntimeException(e);
        }
    }

    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> indexObjects () throws SolrServerException, IOException {

        checkPermission(user,  UserRole.PERMISSION_FUNCTION_SOLR, UserRole.PERMISSION_FUNCTION_SYS_ADMIN);

        if (!Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
            throw new UnsupportedOperationException("This function is possible only with AKUBRA storage. / Funkce je dostupná jen s uložištěm AKUBRA.");
        }

        BatchParams params = new BatchParams(Collections.singletonList("INDEXACE OBJEKTŮ"));
        Batch batch = BatchUtils.addNewBatch(this.importManager, Collections.singletonList("INDEXACE OBJEKTŮ"), user, Batch.INTERNAL_INDEX_OBJECTS_TO_SOLR, Batch.State.INTERNAL_RUNNING, Batch.State.INTERNAL_FAILED, params);

        try {
            String objectStorePath = this.akubraConfiguration.getObjectStorePath();
            String datastreamStorePath = this.akubraConfiguration.getDatastreamStorePath();

            String searchSolrHost = this.akubraConfiguration.getSolrSearchHost();
            SolrClient solrClient = new ConcurrentUpdateSolrClient.Builder(searchSolrHost).withQueueSize(100).build();
            SolrObjectFeeder feeder = new SolrObjectFeeder(solrClient);

            feeder.deleteProcessingIndex();
            feeder.commit();
            //processRoot(feeder, datastreamStorePath, false);
            processRoot(feeder, objectStorePath, true);
            processParentPid(feeder, objectStorePath);

            BatchUtils.finishedSuccessfully(this.importManager, batch, batch.getFolder(), null, Batch.State.INTERNAL_DONE);
            return returnFunctionSuccess();
        } catch (Exception ex) {
            BatchUtils.finishedWithError(this.importManager, batch, batch.getFolder(), BatchManager.toString(ex), Batch.State.INTERNAL_FAILED);
            throw ex;
        }
    }

    @POST
    @Path(IndexerResourceApi.INDEX_PARENT_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> setParentsPid () throws SolrServerException, IOException {

        checkPermission(user, UserRole.PERMISSION_FUNCTION_SYS_ADMIN);

        if (!Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
            throw new UnsupportedOperationException("This function is possible only with AKUBRA storage. / Funkce je dostupná jen s uložištěm AKUBRA.");
        }

        BatchParams params = new BatchParams(Collections.singletonList("INDEXACE NADŘAZENÝCH OBJEKTŮ"));
        Batch batch = BatchUtils.addNewBatch(this.importManager, Collections.singletonList("INDEXACE NADŘAZENÝCH OBJEKTŮ"), user, Batch.INTERNAL_INDEX_PARENTS_TO_SOLR, Batch.State.INTERNAL_RUNNING, Batch.State.INTERNAL_FAILED, params);

        try {
            String objectStorePath = this.akubraConfiguration.getObjectStorePath();

            String searchSolrHost = this.akubraConfiguration.getSolrSearchHost();
            SolrClient solrClient = new ConcurrentUpdateSolrClient.Builder(searchSolrHost).withQueueSize(100).build();
            SolrObjectFeeder feeder = new SolrObjectFeeder(solrClient);

            processParentPid(feeder, objectStorePath);

            BatchUtils.finishedSuccessfully(this.importManager, batch, batch.getFolder(), null, Batch.State.INTERNAL_DONE);
            return returnFunctionSuccess();
        } catch (Exception ex) {
            BatchUtils.finishedWithError(this.importManager, batch, batch.getFolder(), BatchManager.toString(ex), Batch.State.INTERNAL_FAILED);
            throw ex;
        }
    }

    private void processRoot(SolrObjectFeeder feeder, String storePath, boolean rebuildIndex) throws IOException, SolrServerException {
        StringBuilder errors = new StringBuilder();
        try {
            LOG.info("Indexing documents started.");
            java.nio.file.Path storeRoot = Paths.get(storePath);
            File rootFile = new File(storeRoot.toUri());
            final AkubraStorage storage = AkubraStorage.getInstance(akubraConfiguration);
            long start = System.currentTimeMillis();
            filesCount = 0;
            objectCount = 0;
            processFile(feeder, storage, rootFile, rebuildIndex);
            LOG.log(Level.INFO, "Indexed time: " + (System.currentTimeMillis() - start) / 1000 + " s, object count " + filesCount);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Error in files!", ex);
        } finally {
            if (!errors.toString().isEmpty()) {
                LOG.severe("Nepodarilo se zaindexovat: \n" + errors.toString());
            }
            if (feeder != null) {
                feeder.commit();
                LOG.info("Feeder commited.");
            }
        }
    }

    private void processParentPid(SolrObjectFeeder feeder, String storePath) throws SolrServerException, IOException {
        StringBuilder errors = new StringBuilder();
        try {
            LOG.info("Indexing documents started.");
            SearchView search = null;
            if (Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
                search = AkubraStorage.getInstance(akubraConfiguration).getSearch(session.getLocale(httpHeaders));
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfiguration.getTypeOfStorage());
            }

            java.nio.file.Path storeRoot = Paths.get(storePath);
            File rootFile = new File(storeRoot.toUri());
            final AkubraStorage storage = AkubraStorage.getInstance(akubraConfiguration);
            long start = System.currentTimeMillis();
            filesCount = 0;
            setParentPid(feeder, storage, rootFile, search);
            LOG.log(Level.INFO, "Indexed time: " + (System.currentTimeMillis() - start) / 1000 + " s, object count " + filesCount);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Error in files!", ex);
        } finally {
            if (!errors.toString().isEmpty()) {
                LOG.severe("Nepodarilo se zaindexovat: \n" + errors.toString());
            }
            if (feeder != null) {
                feeder.commit();
                LOG.info("Feeder commited.");
            }
        }
    }

    private void setParentPid(SolrObjectFeeder feeder, AkubraStorage storage, File file, SearchView search) {
        if (file.isDirectory()) {
            for (File childFile : file.listFiles()) {
                setParentPid(feeder, storage, childFile, search);
            }
        } else {
            try {
                FileInputStream inputStream = new FileInputStream(file);
                DigitalObject digitalObject = createDigitalObject(inputStream);
                ProArcObject proArcObject = new LocalStorage().load(digitalObject.getPID(), file);
                // indexovat jen objekty s proarcu - modely z pluginu a zarizeni
                if (proArcObject.getPid().startsWith("uuid")) {
                    List<SearchViewItem> parents = search.findReferrers(proArcObject.getPid());

                    if (parents.isEmpty()) {
                        feeder.feedParentPid(proArcObject.getPid(), SolrUtils.PROPERTY_PARENTPID_NO_PARENT, false);
                    } else {
                        feeder.feedParentPid(proArcObject.getPid(), parents.get(0).getPid(), false);
                    }
                    this.filesCount++;
                    if (filesCount % 50 == 0) {
                        feeder.commit();
                        LOG.info("Updated " + filesCount + " of " + objectCount + " objects.");
                    }
                }
            } catch (Throwable throwable) {
                LOG.log(Level.SEVERE, "Error in proccesing file: " + file.getAbsolutePath(), throwable);
            }
        }
    }

    private void processFile(SolrObjectFeeder feeder, AkubraStorage storage, File file, boolean rebuildIndex) {
        if (file.isDirectory()) {
            for (File childFile : file.listFiles()) {
                processFile(feeder, storage, childFile, rebuildIndex);
            }
        } else {
            try {
                FileInputStream inputStream = new FileInputStream(file);
                DigitalObject digitalObject = createDigitalObject(inputStream);
                ProArcObject proArcObject = new LocalStorage().load(digitalObject.getPID(), file);
                // indexovat jen objekty s proarcu - modely z pluginu a zarizeni
                if (proArcObject.getPid().startsWith("uuid")) {
                    if (rebuildIndex) {
                        try {
                            feeder.insertDescriptionDocument(digitalObject, proArcObject, false);
                            this.filesCount++;
                            this.objectCount++;
                            if (filesCount % 50 == 0) {
                                LOG.info("Proccessed " + filesCount + " objects");
                                feeder.commit();
                            }
                        } catch (Exception exception) {
                            if ("URI is not hierarchical".equals(exception.getMessage())) {
                                try {
                                    proArcObject = storage.find(digitalObject.getPID());
                                    filesCount++;
                                    this.objectCount++;
                                    feeder.insertDescriptionDocument(digitalObject, proArcObject, false);
                                    if (filesCount % 50 == 0) {
                                        LOG.info("Proccessed " + filesCount + " objects");
                                        feeder.commit();
                                    }
                                } catch (Exception ex) {
                                    LOG.warning(proArcObject.getPid() + " - " + ((LocalStorage.LocalObject) proArcObject).getFoxml().getPath());
                                }
                            } else {
                                LOG.warning(proArcObject.getPid() + " - " + ((LocalStorage.LocalObject) proArcObject).getFoxml().getPath());
                            }
                        }
                    }
                } else if (proArcObject.getPid().startsWith("device")) {
                    if (rebuildIndex) {
                        try {
                            feeder.feedDescriptionDevice(digitalObject, proArcObject, false);
                            filesCount++;
                            if (filesCount % 50 == 0) {
                                LOG.info("Proccessed " + filesCount + " objects");
                                feeder.commit();
                            }
                        } catch (Exception exception) {
                            if ("URI is not hierarchical".equals(exception.getMessage())) {
                                try {
                                    proArcObject = storage.find(digitalObject.getPID());
                                    filesCount++;
                                    feeder.feedDescriptionDevice(digitalObject, proArcObject, false);
                                    if (filesCount % 50 == 0) {
                                        LOG.info("Proccessed " + filesCount + " objects");
                                        feeder.commit();
                                    }
                                } catch (Exception ex) {
                                    LOG.warning(proArcObject.getPid() + " - " + ((LocalStorage.LocalObject) proArcObject).getFoxml().getPath());
                                }
                            } else {
                                LOG.warning(proArcObject.getPid() + " - " + ((LocalStorage.LocalObject) proArcObject).getFoxml().getPath());
                            }
                        }
                    }
                } else if (proArcObject.getPid().startsWith("software")) {
                    if (rebuildIndex) {
                        try {
                            feeder.feedDescriptionSoftware(digitalObject, proArcObject, false);
                            filesCount++;
                            if (filesCount % 50 == 0) {
                                LOG.info("Proccessed " + filesCount + " objects");
                                feeder.commit();
                            }
                        } catch (Exception exception) {
                            if ("URI is not hierarchical".equals(exception.getMessage())) {
                                try {
                                    proArcObject = storage.find(digitalObject.getPID());
                                    filesCount++;
                                    feeder.feedDescriptionSoftware(digitalObject, proArcObject, false);
                                    if (filesCount % 50 == 0) {
                                        LOG.info("Proccessed " + filesCount + " objects");
                                        feeder.commit();
                                    }
                                } catch (Exception ex) {
                                    LOG.warning(proArcObject.getPid() + " - " + ((LocalStorage.LocalObject) proArcObject).getFoxml().getPath());
                                }
                            } else {
                                LOG.warning(proArcObject.getPid() + " - " + ((LocalStorage.LocalObject) proArcObject).getFoxml().getPath());
                            }
                        }
                    }
                }
            } catch (Throwable throwable) {
                LOG.log(Level.SEVERE, "Error in proccesing file: " + file.getAbsolutePath(), throwable);
            }
        }
    }

    static DigitalObject createDigitalObject(InputStream inputStream) {
        DigitalObject obj = null;
        try {
            synchronized (unmarshaller) {
                obj = (DigitalObject) unmarshaller.unmarshal(inputStream);
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        return obj;
    }


    @POST
    @Path(IndexerResourceApi.OBJECT_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> indexDocument (
            @FormParam(IndexerResourceApi.DIGITALOBJECT_PID) String pid
    ) {
        if (!Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
            throw new UnsupportedOperationException("This function is possible only with AKUBRA storage. / Funkce je dostupná jen s uložištěm AKUBRA.");
        }

        checkPermission(user, UserRole.PERMISSION_FUNCTION_SOLR);

        LOG.info("Indexing document with pid started");
        return new SmartGwtResponse<>();
    }

    protected String returnLocalizedMessage(String key, Object... arguments) {
        Locale locale = session.getLocale(httpHeaders);
        ServerMessages msgs = ServerMessages.get(locale);
        return msgs.getFormattedMessage(key, arguments);
    }
}
