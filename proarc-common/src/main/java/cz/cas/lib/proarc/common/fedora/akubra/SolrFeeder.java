package cz.cas.lib.proarc.common.fedora.akubra;

import com.qbizm.kramerius.imp.jaxb.DigitalObject;
import com.qbizm.kramerius.imp.jaxb.ObjectPropertiesType;
import com.qbizm.kramerius.imp.jaxb.PropertyType;
import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.incad.kramerius.resourceindex.ProcessingIndexFeeder;
import java.io.IOException;
import java.util.List;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrInputDocument;

import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_CREATEDATE;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_LABEL;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_LASTMODIFIED;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_OWNER;
import static cz.cas.lib.proarc.common.fedora.FoxmlUtils.PROPERTY_STATE;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_CREATED;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_DEVICE;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_EXPORT_ARCHIVE;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_EXPORT_CROSSREF;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_EXPORT_KRAMERIUS;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_EXPORT_NDK;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_LABEL;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_LOCKED;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_MEMBERS;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_MODEL;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_MODIFIED;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_ORGANIZATION;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_OWNER;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_PAGE_INDEX;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_PAGE_NUMBER;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_PAGE_TYPE;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_PID;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_SOURCE;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_STATE;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_STATUS;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_USER;

public class SolrFeeder extends ProcessingIndexFeeder {

    public SolrFeeder(SolrClient solrClient) {
        super(solrClient);
    }

    public void feedDescriptionDocument(DigitalObject object, FedoraObject fedoraObject, boolean commit) throws DigitalObjectException {
        RelationEditor relationEditor = new RelationEditor(fedoraObject);
        String pid = fedoraObject.getPid();
        String model = relationEditor.getModel();
        String owner = getProperties(object, PROPERTY_OWNER);
        String label = getProperties(object, PROPERTY_LABEL);
        String state = getProperties(object, PROPERTY_STATE);
        String created = getProperties(object, PROPERTY_CREATEDATE);
        String modified = getProperties(object, PROPERTY_LASTMODIFIED);

        String device = relationEditor.getDevice();
        String organization = relationEditor.getOrganization();
        String user = relationEditor.getUser();
        String status = relationEditor.getStatus();
        String ndkExport = relationEditor.getNdkExportResult();
        String krameriusExport = relationEditor.getKrameriusExportResult();
        String archiveExport = relationEditor.getArchiveExportResult();
        String crossrefExport = relationEditor.getCrossrefExportResult();

        ModsDefinition mods = getModsDefinition(fedoraObject);
        String pageIndex = ExportUtils.getPageIndexAsString(mods);
        String pageType = ExportUtils.getPageType(mods);
        String pageNumber = ExportUtils.getPageNumber(mods);

        Boolean isLocked = relationEditor.isLocked();

        List<String> members = relationEditor.getMembers();

        try {
            feedDescriptionDocument(pid, model, owner, label, state, created, modified, organization, user, status,
                    ndkExport, krameriusExport, archiveExport, crossrefExport, isLocked, device, members,
                    pageIndex, pageType, pageNumber);
            if (commit) {
                commit();
            }
        } catch (SolrServerException | IOException ex) {
            throw new DigitalObjectException(pid, "Nepodarilo se zaindexovat objekt " + pid + " do SOLRu.");
        }
    }

    private ModsDefinition getModsDefinition(FedoraObject object) throws DigitalObjectException {
        XmlStreamEditor xml = object.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, object);
        return modsStreamEditor.read();
    }

    private UpdateResponse feedDescriptionDocument(String pid, String model, String owner, String label, String state, String created, String modified, String organization, String user, String status, String ndkExport, String krameriusExport, String archiveExport, String crossrefExport, Boolean isLocked, String device, List<String> members, String pageIndex, String pageType, String pageNumber) throws SolrServerException, IOException {
        SolrInputDocument sdoc = new SolrInputDocument();
        sdoc.addField(FIELD_SOURCE, pid);
        sdoc.addField(FIELD_PID, pid);
        sdoc.addField(FIELD_MODEL, model);
        sdoc.addField(FIELD_OWNER, owner);
        sdoc.addField(FIELD_LABEL, label);
        sdoc.addField(FIELD_STATE, state);
        sdoc.addField(FIELD_CREATED, created);
        sdoc.addField(FIELD_MODIFIED, modified);
        sdoc.addField(FIELD_ORGANIZATION, organization);
        sdoc.addField(FIELD_USER, user);
        sdoc.addField(FIELD_STATUS, status);
        sdoc.addField(FIELD_EXPORT_NDK, ndkExport);
        sdoc.addField(FIELD_EXPORT_KRAMERIUS, krameriusExport);
        sdoc.addField(FIELD_EXPORT_ARCHIVE, archiveExport);
        sdoc.addField(FIELD_EXPORT_CROSSREF, crossrefExport);
        sdoc.addField(FIELD_LOCKED, isLocked);
        sdoc.addField(FIELD_DEVICE, device);
        sdoc.addField(FIELD_MEMBERS, members);
        sdoc.addField(FIELD_PAGE_INDEX, pageIndex);
        sdoc.addField(FIELD_PAGE_NUMBER, pageNumber);
        sdoc.addField(FIELD_PAGE_TYPE, pageType);

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
