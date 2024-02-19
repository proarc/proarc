package cz.cas.lib.proarc.common.storage.akubra;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.ObjectPropertiesType;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
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

import static cz.cas.lib.proarc.common.storage.FoxmlUtils.PROPERTY_CREATEDATE;
import static cz.cas.lib.proarc.common.storage.FoxmlUtils.PROPERTY_LABEL;
import static cz.cas.lib.proarc.common.storage.FoxmlUtils.PROPERTY_LASTMODIFIED;
import static cz.cas.lib.proarc.common.storage.FoxmlUtils.PROPERTY_OWNER;
import static cz.cas.lib.proarc.common.storage.FoxmlUtils.PROPERTY_STATE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_CREATED;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_DEVICE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_EXPORT_ARCHIVE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_EXPORT_CROSSREF;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_EXPORT_KRAMERIUS;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_EXPORT_NDK;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_LABEL;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_LOCKED;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_MEMBERS;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_MODEL;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_MODIFIED;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_ORGANIZATION;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_OWNER;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PAGE_INDEX;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PAGE_NUMBER;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PAGE_POSITION;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PAGE_TYPE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PID;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_SOURCE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_STATE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_STATUS;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_USER;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.PROPERTY_STATE_DEACTIVE;

public class SolrFeeder extends ProcessingIndexFeeder {

    public SolrFeeder(SolrClient solrClient) {
        super(solrClient);
    }

    public void feedDescriptionDocument(DigitalObject object, ProArcObject proArcObject, boolean commit) throws DigitalObjectException {
        RelationEditor relationEditor = new RelationEditor(proArcObject);
        String pid = proArcObject.getPid();
        String model = relationEditor.getModel();
        String owner = getProperties(object, PROPERTY_OWNER);
        String label = getProperties(object, PROPERTY_LABEL);
        String state = updateState(getProperties(object, PROPERTY_STATE));
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

        ModsDefinition mods = getModsDefinition(proArcObject);
        String pageIndex = ExportUtils.getPageIndexAsString(mods);
        String pageType = ExportUtils.getPageType(mods);
        String pageNumber = ExportUtils.getPageNumber(mods);
        String pagePosition = ExportUtils.getPagePosition(mods);

        Boolean isLocked = relationEditor.isLocked();

        List<String> members = relationEditor.getMembers();

        try {
            feedDescriptionDocument(pid, model, owner, label, state, created, modified, organization, user, status,
                    ndkExport, krameriusExport, archiveExport, crossrefExport, isLocked, device, members,
                    pageIndex, pageType, pageNumber, pagePosition);
            if (commit) {
                commit();
            }
        } catch (SolrServerException | IOException ex) {
            throw new DigitalObjectException(pid, "Nepodarilo se zaindexovat objekt " + pid + " do SOLRu.");
        }
    }

    private ModsDefinition getModsDefinition(ProArcObject object) throws DigitalObjectException {
        XmlStreamEditor xml = object.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, object);
        return modsStreamEditor.read();
    }

    private UpdateResponse feedDescriptionDocument(String pid, String model, String owner, String label, String state, String created, String modified, String organization, String user, String status, String ndkExport, String krameriusExport, String archiveExport, String crossrefExport, Boolean isLocked, String device, List<String> members, String pageIndex, String pageType, String pageNumber, String pagePosition) throws SolrServerException, IOException {
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
        sdoc.addField(FIELD_MEMBERS, members.toArray());
        sdoc.addField(FIELD_PAGE_INDEX, pageIndex);
        sdoc.addField(FIELD_PAGE_NUMBER, pageNumber);
        sdoc.addField(FIELD_PAGE_TYPE, pageType);
        sdoc.addField(FIELD_PAGE_POSITION, pagePosition);

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

    public void feedDescriptionDevice(DigitalObject object, ProArcObject proArcObject, boolean commit) throws DigitalObjectException {
        RelationEditor relationEditor = new RelationEditor(proArcObject);
        String pid = proArcObject.getPid();
        String model = relationEditor.getModel();
        String owner = getProperties(object, PROPERTY_OWNER);
        String label = getProperties(object, PROPERTY_LABEL);
        String state = updateState(getProperties(object, PROPERTY_STATE));
        String created = getProperties(object, PROPERTY_CREATEDATE);
        String modified = getProperties(object, PROPERTY_LASTMODIFIED);

        try {
            feedDescriptionDevice(pid, model, owner, label, state, created, modified);
            if (commit) {
                commit();
            }
        } catch (SolrServerException | IOException ex) {
            throw new DigitalObjectException(pid, "Nepodarilo se zaindexovat objekt " + pid + " do SOLRu.");
        }
    }

    private UpdateResponse feedDescriptionDevice(String pid, String model, String owner, String label, String state, String created, String modified) throws SolrServerException, IOException {
        SolrInputDocument sdoc = new SolrInputDocument();
        sdoc.addField(FIELD_SOURCE, pid);
        sdoc.addField(FIELD_PID, pid);
        sdoc.addField(FIELD_MODEL, model);
        sdoc.addField(FIELD_OWNER, owner);
        sdoc.addField(FIELD_LABEL, label);
        sdoc.addField(FIELD_STATE, state);
        sdoc.addField(FIELD_CREATED, created);
        sdoc.addField(FIELD_MODIFIED, modified);
        return feedDescriptionDocument(sdoc);
    }

    private String updateState(String state) {
        if ("Deleted".equals(state)) {
            return PROPERTY_STATE_DEACTIVE;
        }
        return state;
    }
}
