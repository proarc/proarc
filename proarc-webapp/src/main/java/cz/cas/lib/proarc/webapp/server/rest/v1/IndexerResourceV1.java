package cz.cas.lib.proarc.webapp.server.rest.v1;

import com.google.common.net.HttpHeaders;
import com.google.gwt.http.client.Request;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.SolrObjectFeeder;
import cz.cas.lib.proarc.common.user.Permissions;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.shared.rest.IndexerResourceApi;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
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

@Deprecated
@Path(RestConfig.URL_API_VERSION_1 + "/" + IndexerResourceApi.PATH)
public class IndexerResourceV1 {

    private static final Logger LOG = Logger.getLogger(IndexerResourceV1.class.getName());

    private final AppConfiguration appConfiguration;
    private final AkubraConfiguration akubraConfiguration;
    private final Request httpRequest;
    private final HttpHeaders httpHeaders;
    private final UserProfile user;
    private final SessionContext session;
    private static Unmarshaller unmarshaller;

    int filesCount = 0;

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

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN);

        if (!Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
            throw new UnsupportedOperationException("This function is possible only with AKUBRA storage. / Funkce je dostupná jen s uložištěm AKUBRA.");
        }

        String objectStorePath = this.akubraConfiguration.getObjectStorePath();
        String datastreamStorePath = this.akubraConfiguration.getDatastreamStorePath();

        String searchSolrHost = this.akubraConfiguration.getSolrSearchHost();
        SolrClient solrClient = new ConcurrentUpdateSolrClient.Builder(searchSolrHost).withQueueSize(100).build();
        SolrObjectFeeder feeder = new SolrObjectFeeder(solrClient);

        feeder.deleteProcessingIndex();
        feeder.commit();
        //processRoot(feeder, datastreamStorePath, false);
        processRoot(feeder, objectStorePath, true);


        return new SmartGwtResponse<>();
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
                            feeder.feedDescriptionDocument(digitalObject, proArcObject, false);
                            this.filesCount++;
                            if (filesCount % 50 == 0) {
                                LOG.info("Proccessed " + filesCount + " objects");
                                feeder.commit();
                            }
                        } catch (Exception exception) {
                            if ("URI is not hierarchical".equals(exception.getMessage())) {
                                try {
                                    proArcObject = storage.find(digitalObject.getPID());
                                    filesCount++;
                                    feeder.feedDescriptionDocument(digitalObject, proArcObject, false);
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

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN);

        LOG.info("Indexing document with pid started");
        return new SmartGwtResponse<>();
    }
}
