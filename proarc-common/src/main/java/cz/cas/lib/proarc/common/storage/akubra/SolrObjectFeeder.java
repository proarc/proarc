package cz.cas.lib.proarc.common.storage.akubra;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.ObjectPropertiesType;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.incad.kramerius.resourceindex.ProcessingIndexFeeder;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_DESCRIPTION_STANDARD;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_DEVICE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_EXPORT_ARCHIVE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_EXPORT_CROSSREF;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_EXPORT_KRAMERIUS;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_EXPORT_NDK;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_GENRE;
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
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_URNNBN;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_USER;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_VALIDATION_PROCES;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_VALIDATION_STATUS;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.PROPERTY_STATE_DEACTIVE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.VALIDATION_STATUS_UNKNOWN;

public class SolrObjectFeeder extends ProcessingIndexFeeder {

    public SolrObjectFeeder(SolrClient solrClient) {
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
        String genre = ExportUtils.getGenre(mods);
        String urnNbn = getUrnNbn(mods);
        String descriptionStandard = getDescriptionStandatd(mods);

        Boolean isLocked = relationEditor.isLocked();

        List<String> members = relationEditor.getMembers();

        try {
            feedDescriptionDocument(pid, model, owner, label, state, created, modified, organization, user, status,
                    ndkExport, krameriusExport, archiveExport, crossrefExport, isLocked, device, members,
                    pageIndex, pageType, pageNumber, pagePosition, genre, urnNbn, descriptionStandard);
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

    private String getUrnNbn(ModsDefinition mods) {
        for (IdentifierDefinition identifier : mods.getIdentifier()) {
            if ("urnnbn".equals(identifier.getType()) && (identifier.getInvalid() == null || "".equals(identifier.getInvalid()) || "false".equals(identifier.getInvalid()))) {
                return identifier.getValue();
            }
        }
        return null;
    }

    public static String getDescriptionStandatd(ModsDefinition mods) {
        for (RecordInfoDefinition recordInfo : mods.getRecordInfo()) {
            for (StringPlusLanguagePlusAuthority description : recordInfo.getDescriptionStandard()) {
                if (description != null && description.getValue() != null && !description.getValue().isEmpty()) {
                    return description.getValue();
                }
            }
        }
        return null;
    }

    private UpdateResponse feedDescriptionDocument(String pid, String model, String owner, String label, String state, String created, String modified, String organization, String user,
            String status, String ndkExport, String krameriusExport, String archiveExport, String crossrefExport, Boolean isLocked, String device, List<String> members, String pageIndex,
            String pageType, String pageNumber, String pagePosition, String genre, String urnNbn, String descriptionStandatd) throws SolrServerException, IOException {
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
        sdoc.addField(FIELD_GENRE, genre);
        sdoc.addField(FIELD_URNNBN, urnNbn);
        sdoc.addField(FIELD_DESCRIPTION_STANDARD, descriptionStandatd);
        sdoc.addField(FIELD_VALIDATION_STATUS, VALIDATION_STATUS_UNKNOWN);
        sdoc.addField(FIELD_VALIDATION_PROCES, 0);

        return feedDescriptionDocument(sdoc);
    }

    public void feedValidationResult(String pid, Integer procesId, String status) throws DigitalObjectException {
        SolrInputDocument sdoc = new SolrInputDocument();
        sdoc.addField(FIELD_SOURCE, pid);
        sdoc.addField(FIELD_PID, pid);

        Map<String, Object> updateStatus = new HashMap<>();
        updateStatus.put("set", status);
        sdoc.addField(FIELD_VALIDATION_STATUS, updateStatus);

        Map<String, Object> updateProces = new HashMap<>();
        updateProces.put("set", procesId == null ? 0 : procesId);
        sdoc.addField(FIELD_VALIDATION_PROCES, updateProces);

        try {
            UpdateResponse response = feedDescriptionDocument(sdoc);
            commit();
        } catch (SolrServerException | IOException ex) {
            throw new DigitalObjectException(pid, "Nepodarilo se zapsat stav validace pro pid " + pid + " do SOLRu.");
        }
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
