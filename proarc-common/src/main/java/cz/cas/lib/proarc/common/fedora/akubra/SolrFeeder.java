package cz.cas.lib.proarc.common.fedora.akubra;

import com.qbizm.kramerius.imp.jaxb.DigitalObject;
import com.qbizm.kramerius.imp.jaxb.ObjectPropertiesType;
import com.qbizm.kramerius.imp.jaxb.PropertyType;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.incad.kramerius.resourceindex.ProcessingIndexFeeder;
import java.io.IOException;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrInputDocument;

import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_CREATEDATE;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_LABEL;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_LASTMODIFIED;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_OWNER;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_STATE;

public class SolrFeeder extends ProcessingIndexFeeder {

    public SolrFeeder(SolrClient solrClient) {
        super(solrClient);
    }

    public void feedDescriptionDocument(DigitalObject object, FedoraObject fedoraObject) throws DigitalObjectException {
        RelationEditor relationEditor = new RelationEditor(fedoraObject);
        String pid = fedoraObject.getPid();
        String model = relationEditor.getModel();
        String owner = getProperties(object, PROPERTY_OWNER);
        String label = getProperties(object, PROPERTY_LABEL);
        String state = getProperties(object, PROPERTY_STATE);
        String created = getProperties(object, PROPERTY_CREATEDATE);
        String modified = getProperties(object, PROPERTY_LASTMODIFIED);


        String organization = relationEditor.getOrganization();
        String user = relationEditor.getUser();
        String status = relationEditor.getStatus();
        String ndkExport = relationEditor.getNdkExportResult();
        String krameriusExport = relationEditor.getKrameriusExportResult();
        String archiveExport = relationEditor.getArchiveExportResult();
        String crossrefExport = relationEditor.getCrossrefExportResult();

        Boolean isLocked = relationEditor.isLocked();

        try {
            feedDescriptionDocument(pid, model, owner, label, state, created, modified, organization, user, status, ndkExport, krameriusExport, archiveExport, crossrefExport, isLocked);
        } catch (SolrServerException | IOException ex) {
            throw new DigitalObjectException(pid, "Nepodarilo se zaindexovat objekt " + pid + " do SOLRu.");
        }
    }

    private UpdateResponse feedDescriptionDocument(String pid, String model, String owner, String label, String state, String created, String modified, String organization, String user, String status, String ndkExport, String krameriusExport, String archiveExport, String crossrefExport, Boolean isLocked) throws SolrServerException, IOException {
        SolrInputDocument sdoc = new SolrInputDocument();
        sdoc.addField("source", pid);
        sdoc.addField("pid", pid);
        sdoc.addField("model", model);
        sdoc.addField("owner", owner);
        sdoc.addField("label", label);
        sdoc.addField("state", state);
        sdoc.addField("created", created);
        sdoc.addField("modified", modified);
        sdoc.addField("organization", organization);
        sdoc.addField("user", user);
        sdoc.addField("status", status);
        sdoc.addField("ndkExport", ndkExport);
        sdoc.addField("krameriusExport", krameriusExport);
        sdoc.addField("archiveExport", archiveExport);
        sdoc.addField("crossrefExport", crossrefExport);
        sdoc.addField("isLocked", isLocked);

        return feedDescriptionDocument(sdoc);
    }

    private String getProperties(DigitalObject object, String key) {
        if (object != null) {
            ObjectPropertiesType propertiesType = object.getObjectProperties();
            if (propertiesType != null) {
                for (PropertyType property : propertiesType.getProperty()) {
                    if (key.equals(property.getNAME())) {
                        return property.getVALUE();
                    }
                }
            }
        }
        return null;
    }
}
