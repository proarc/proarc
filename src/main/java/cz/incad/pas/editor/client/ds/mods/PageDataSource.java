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
package cz.incad.pas.editor.client.ds.mods;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DSProtocol;
import com.smartgwt.client.types.FieldType;
import cz.fi.muni.xkremser.editor.client.mods.BaseDateTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.ClassificationTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.CodeOrTextClient;
import cz.fi.muni.xkremser.editor.client.mods.DateTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.DetailTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.IdentifierTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.LanguageTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.LocationTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.ModsCollectionClient;
import cz.fi.muni.xkremser.editor.client.mods.ModsTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.NamePartTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.NameTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.OriginInfoTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.PartTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.PhysicalDescriptionTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.PhysicalLocationTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.PlaceTermTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.PlaceTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.RecordInfoTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.RoleTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.RoleTypeClient.RoleTermClient;
import cz.fi.muni.xkremser.editor.client.mods.StringPlusAuthorityClient;
import cz.fi.muni.xkremser.editor.client.mods.SubjectTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.TitleInfoTypeClient;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.ClientUtils.DataSourceFieldBuilder;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.rpc.ModsGwtRecord;
import cz.incad.pas.editor.client.rpc.ModsGwtServiceAsync;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * XXX impossible to reuse RPCManager UI prompting with GWT-RPC. Requires custom impl.
 * XXX ModsCollectionClient is useless for simple forms.
 * XXX Implement ModsCollection/Map transformation as a REST service.
 *
 * <p>See reasons for NOT using GWT-RPC http://forums.smartclient.com/showthread.php?t=8159#aGWTRPC,
 * http://code.google.com/p/smartgwt/issues/detail?id=303
 *
 * @author Jan Pokorsky
 */
public final class PageDataSource extends DataSource {

    private static final Logger LOG = Logger.getLogger(PageDataSource.class.getName());
    public static final String ID = "PageDataSource";
    public static final String FIELD_PID = "pid";
    public static final String FIELD_MODEL_PID = "model";
    /** original object */
    public static final String FIELD_MODS_TRANSPORT_OBJECT = "ModsGwtRecord";
    public static final String FIELD_STRING_VALUE = "value";
    private static final String FIELD_NAME_OBJECT = "NameTypeClient";
    public static final String FIELD_PAGE_TYPE = "pageType";
    public static final String FIELD_PAGE_INDEX = "pageIndex";
    public static final String FIELD_PAGE_NUMBER = "pageNumber";
    public static final String FIELD_IDENTIFIERS = "identifiers";
    public static final String FIELD_NOTE = "note";
    public static final String FIELD_XML_SRC = "xmlSource";
    //periodical
    public static final String FIELD_PERIODICITY = "periodicity";
    public static final String FIELD_PERIODICITY_VALUE = "periodicityValue";
    public static final String FIELD_SIGLA = "sigla";
    public static final String FIELD_SHELF_LOCATORS = "shelfLocators";
    public static final String FIELD_AUTHORS = "authors";
    public static final String FIELD_CONTRIBUTORS = "contributors";
    public static final String FIELD_PUBLISHERS = "publishers";
    public static final String FIELD_PRINTER_PUBLISHER_NAME = "publisherName";
    public static final String FIELD_PRINTER_PUBLISHER_DATE = "publisherDate";
    public static final String FIELD_PRINTER_PUBLISHER_PLACE = "publisherPlace";
    public static final String FIELD_PRINTERS = "printers";
    public static final String FIELD_TITLES = "titles";
    public static final String FIELD_SUBTITLES = "subtitles";
    public static final String FIELD_ALTERNATIVE_TITLES = "alternativeTitles";
    public static final String FIELD_KEY_TITLES = "keyTitles";
    public static final String FIELD_KEYWORDS = "keywords";
    public static final String FIELD_LANGUAGES = "languages";
    public static final String FIELD_CLASSIFICATIONS = "classifications";
    public static final String FIELD_CLASSIFICATION_UDC = "classificationsUDC";
    public static final String FIELD_CLASSIFICATION_DDC = "classificationsDDC";
    public static final String FIELD_PHYSICAL_DESCRIPTIONS = "physicalDescriptions";
    public static final String FIELD_PHYSICAL_DESCRIPTIONS_EXTENT = "physicalDescriptionsExtent";
    public static final String FIELD_PHYSICAL_DESCRIPTIONS_SIZE = "physicalDescriptionsSize";
    public static final String FIELD_RECORD_ORIGIN = "recordOrigin";
    // periodical volume
    public static final String FIELD_PER_VOLUME_NUMBER = "periodicalVolumeNumber";
    public static final String FIELD_PER_VOLUME_YEAR = "periodicalVolumeYear";
    // periodical issue
    public static final String FIELD_PER_ISSUE_NUMBER = "PeriodicalItemNumber";
    public static final String FIELD_PER_ISSUE_NUMBER_SORTING = "PeriodicalItemNumberSorting";
    public static final String FIELD_PER_ISSUE_DATE = "periodicalItemDate";

    public PageDataSource() {
        setID(ID);
        setDataProtocol(DSProtocol.CLIENTCUSTOM);
        setDataFormat(DSDataFormat.CUSTOM);
        setClientOnly(false);
        DataSourceField pid = new DataSourceFieldBuilder<DataSourceField>(new DataSourceField(FIELD_PID, FieldType.TEXT))
                .primaryKey().build();
        DataSourceField modsObject = new DataSourceFieldBuilder<DataSourceField>(new DataSourceField(FIELD_MODS_TRANSPORT_OBJECT, FieldType.ANY))
                .hidden().required().build();
        modsObject.setIgnore(true);
        DataSourceField pageType = new DataSourceField(FIELD_PAGE_TYPE, FieldType.TEXT, "Page Type");
        DataSourceField pageIndex = new DataSourceField(FIELD_PAGE_INDEX, FieldType.INTEGER, "Page Index");
        DataSourceField pageNumber = new DataSourceField(FIELD_PAGE_NUMBER, FieldType.TEXT, "Page Number");
        DataSourceField identifiers = new DataSourceField(FIELD_IDENTIFIERS, FieldType.ANY);
        // value treated as Record[]
        identifiers.setTypeAsDataSource(new IdentifierDataSource());
        // XXX while using own GWT transport it is not necessary to declare all fields
        DataSourceField note = new DataSourceField(FIELD_NOTE, FieldType.TEXT, "Note");
        setFields(pid, modsObject, pageType, pageIndex, pageNumber, identifiers, note);
    }

    public static PageDataSource getInstance() {
        PageDataSource ds = (PageDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new PageDataSource();
        return ds;
    }

    public static ModsGwtRecord getMods(Record r) {
        return (ModsGwtRecord) r.getAttributeAsObject(FIELD_MODS_TRANSPORT_OBJECT);
    }

    @Override
    protected Object transformRequest(DSRequest dsRequest) {
        DSOperationType operationType = dsRequest.getOperationType();
        switch (operationType) {
            case FETCH:
                fetchRequest(dsRequest);
                break;
            case UPDATE:
                updateRequest(dsRequest);
                break;
            case ADD:
                updateRequest(dsRequest);
                break;
        }
        return dsRequest.getData();
    }

    private void updateRequest(final DSRequest dsRequest) {
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        final DSResponse dsResponse = new DSResponse();
        JavaScriptObject dataJso = dsRequest.getData();
        final Record record = new Record(dataJso);
        final String pidAttr = record.getAttribute(FIELD_PID);
        ModsGwtRecord modsGwtRecord = convert(record);
        service.write(pidAttr, modsGwtRecord, new AsyncCallback<String>() {

            @Override
            public void onFailure(Throwable caught) {
                dsResponse.setStatus(RPCResponse.STATUS_FAILURE);
                processResponse(dsRequest.getRequestId(), dsResponse);
            }

            @Override
            public void onSuccess(String pid) {
                if (pidAttr == null) {
                    record.setAttribute(FIELD_PID, pid);
                }
                Record[] data = new Record[]{record};
                dsResponse.setData(data);
                processResponse(dsRequest.getRequestId(), dsResponse);
            }
        });
    }

    private void fetchRequest(final DSRequest dsRequest) {
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        final DSResponse dsResponse = new DSResponse();
        final String pid = dsRequest.getCriteria().getAttribute(FIELD_PID);
        final String modelEditor = dsRequest.getCriteria().getAttribute(MetaModelDataSource.FIELD_EDITOR);
        if (pid == null || pid.isEmpty()) {
            ClientUtils.severe(LOG, "missing pid");
            dsResponse.setStatus(RPCResponse.STATUS_VALIDATION_ERROR);
            dsResponse.getErrors().put(FIELD_PID, "Missing pid");
            processResponse(dsRequest.getRequestId(), dsResponse);
            return ;
        }
        ClientUtils.info(LOG, "ModsGwtServiceAsync.read: pid: %s, model editor: %s", pid, modelEditor);
        service.read(pid, new AsyncCallback<ModsGwtRecord>() {

            @Override
            public void onFailure(Throwable caught) {
                ClientUtils.severe(LOG, "read failed: " + caught.getMessage());
                LOG.log(Level.SEVERE, "fetchRequest.onFailure: " + pid, caught);
                dsResponse.setStatus(RPCResponse.STATUS_FAILURE);
                dsResponse.getErrors().put(FIELD_PID, caught.getMessage());
                processResponse(dsRequest.getRequestId(), dsResponse);
            }

            @Override
            public void onSuccess(ModsGwtRecord modsTransport) {
                Record record = convert(pid, modelEditor, modsTransport);
                Record[] data = new Record[] {record};
                ClientUtils.fine(LOG, "read.onSuccess: %s", ClientUtils.dump(data));
                dsResponse.setData(data);
                dsResponse.setStatus(RPCResponse.STATUS_SUCCESS);
                processResponse(dsRequest.getRequestId(), dsResponse);
            }
        });
    }

    public ModsGwtRecord convert(Record record) {
        ModsGwtRecord modsTransport = getMods(record);
        ModsCollectionClient modsCollection = modsTransport.getMods();
        if (modsCollection == null) {
            modsCollection = new ModsCollectionClient();
            modsTransport.setMods(modsCollection);
        }
        String pidAttr = record.getAttribute(FIELD_PID);
        String pageTypeAttr = record.getAttribute(FIELD_PAGE_TYPE);
        String pageNumberAttr = record.getAttribute(FIELD_PAGE_NUMBER);
        String pageIndexAttr = record.getAttribute(FIELD_PAGE_INDEX);
        String noteAttr = record.getAttribute(FIELD_NOTE);
        Record[] identifiersAttr = record.getAttributeAsRecordArray(FIELD_IDENTIFIERS);
        LOG.info(ClientUtils.format("convert Record -> ModsCollectionClient: "
                + "pid: %s, pageType: %s, pageNumber: %s, pageIndex: %s,"
                + " identifiersAttr: %s, note: '%s'",
                pidAttr, pageTypeAttr, pageNumberAttr, pageIndexAttr, identifiersAttr, noteAttr));
        List<ModsTypeClient> modsTypes = modsCollection.getMods();
        if (modsTypes == null) {
            modsTypes = new ArrayList<ModsTypeClient>();
        }
        if (modsTypes.isEmpty()) {
            modsTypes.add(new ModsTypeClient());
        }
        ModsTypeClient mods = modsTypes.get(0);
        mods.setIdentifier(writeIdentifiers(identifiersAttr));
        mods.setPart(writePart(mods.getPart(), normalizeAttr(pageTypeAttr),
                normalizeAttr(pageIndexAttr), normalizeAttr(pageNumberAttr),
                normalizeAttr(noteAttr)));
        return modsTransport;
    }

    private List<IdentifierTypeClient> writeIdentifiers(Object o) {
        Record[] records = new Record[0];
        if (o instanceof Record[]) {
            records = (Record[]) o;
        } else if (o instanceof RecordList) {
            records = ((RecordList) o).toArray();
        } else {
            String msg = "";
            if (o instanceof JavaScriptObject) {
                msg = ClientUtils.dump((JavaScriptObject) o);
            }
            throw new IllegalStateException("PageDataSource.writeIdentifiers: unsupported value type: " + o.getClass() + ", dump: \n" + msg);
        }
        return IdentifierDataSource.convert(records);
    }

    /**
     * Translates MODS object to records acceptable by particular model editor.
     *
     * @param pid digital object's id
     * @param modelEditor simplified model editor
     * @param modsRecord digital object
     * @return array with translated record
     */
    public Record convert(String pid, String modelEditor, ModsGwtRecord modsRecord) {
        Record record = createModsRecord(pid, modsRecord);
        ModsCollectionClient mods = modsRecord.getMods();
        if (MetaModelDataSource.EDITOR_PAGE.equals(modelEditor)) {
            convertPage(mods, record);
        } else if (MetaModelDataSource.EDITOR_PERIODICAL.equals(modelEditor)) {
            convertPeriodical(mods, record);
        } else if (MetaModelDataSource.EDITOR_PERIODICAL_VOLUME.equals(modelEditor)) {
            convertPeriodicalVolume(mods, record);
        } else if (MetaModelDataSource.EDITOR_PERIODICAL_ISSUE.equals(modelEditor)) {
            record = convertPeriodicalIssue(mods, record);
        }
        return record;
    }

    private Record createModsRecord(String pid, ModsGwtRecord modsRecord) {
        Record record = new Record();
        record.setAttribute(FIELD_PID, pid);
        record.setAttribute(FIELD_MODS_TRANSPORT_OBJECT, modsRecord);
        return record;
    }

    public Record convertPeriodicalIssue(ModsCollectionClient modsCollection, Record record) {
        List<ModsTypeClient> modsTypes = modsCollection.getMods();
        if (modsTypes != null && !modsTypes.isEmpty()) {
            ModsTypeClient mods = modsTypes.get(0);
            record.setAttribute(FIELD_IDENTIFIERS, IdentifierDataSource.convert(mods.getIdentifier()));
            fetchPeriodicalIssuePart(mods.getPart(), record);
        }
        return record;
    }

    /**
     * reads issue date, issue number, issue sorting number and note
     */
    private void fetchPeriodicalIssuePart(List<PartTypeClient> parts, Record record) {
        if (parts != null && !parts.isEmpty()) {
            PartTypeClient part = parts.get(0);
            List<BaseDateTypeClient> dates = part.getDate();
            if (dates != null && !dates.isEmpty()) {
                BaseDateTypeClient date = dates.get(0);
                date.getValue();
                record.setAttribute(FIELD_PER_ISSUE_DATE, date.getValue());
            }

            List<String> notes = part.getText();
            if (notes != null && ! notes.isEmpty()) {
                record.setAttribute(FIELD_NOTE, notes.get(0));
            }

            List<DetailTypeClient> details = nonNullList(part.getDetail());
            for (DetailTypeClient detail : details) {
                if ("issue".equals(detail.getType())) {
                    List<String> numbers = detail.getNumber();
                    if (numbers != null && !numbers.isEmpty()) {
                        record.setAttribute(FIELD_PER_ISSUE_NUMBER, numbers.get(0));
                    }
                    List<String> captions = detail.getCaption();
                    if (captions != null && !captions.isEmpty()) {
                        record.setAttribute(FIELD_PER_ISSUE_NUMBER_SORTING, numbers.get(0));
                    }
                }
            }
        }
    }

    public Record convertPeriodicalVolume(ModsCollectionClient modsCollection, Record record) {
        List<ModsTypeClient> modsTypes = modsCollection.getMods();
        if (modsTypes != null && !modsTypes.isEmpty()) {
            ModsTypeClient mods = modsTypes.get(0);
            record.setAttribute(FIELD_IDENTIFIERS, IdentifierDataSource.convert(mods.getIdentifier()));
            fetchPeriodicalVolumePart(mods.getPart(), record);
            // K3 stores volume notes in part/text
//            fetchNote(mods.getPart(), record);
        }
        return record;
    }

    private void fetchPeriodicalVolumePart(List<PartTypeClient> parts, Record record) {
        if (parts != null && !parts.isEmpty()) {
            PartTypeClient part = parts.get(0);
            List<BaseDateTypeClient> dates = part.getDate();
            if (dates != null && !dates.isEmpty()) {
                BaseDateTypeClient date = dates.get(0);
                date.getValue();
                record.setAttribute(FIELD_PER_VOLUME_YEAR, date.getValue());
            }

            List<String> notes = part.getText();
            if (notes != null && ! notes.isEmpty()) {
                record.setAttribute(FIELD_NOTE, notes.get(0));
            }

            List<DetailTypeClient> details = nonNullList(part.getDetail());
            for (DetailTypeClient detail : details) {
                if ("volume".equals(detail.getType())) {
                    List<String> numbers = detail.getNumber();
                    if (numbers != null && !numbers.isEmpty()) {
                        record.setAttribute(FIELD_PER_VOLUME_NUMBER, numbers.get(0));
                    }
                }
            }
        }
    }

    public Record convertPeriodical(ModsCollectionClient modsCollection, Record record) {
        List<ModsTypeClient> modsTypes = modsCollection.getMods();
        if (modsTypes != null && !modsTypes.isEmpty()) {
            ModsTypeClient mods = modsTypes.get(0);
            record.setAttribute(FIELD_IDENTIFIERS, IdentifierDataSource.convert(mods.getIdentifier()));
            fetchLocations(mods.getLocation(), record);
            fetchPeriodicity(mods.getOriginInfo(), record);
            fetchTitles(mods.getTitleInfo(), record);
            fetchNames(mods.getName(), record);
            fetchPrinterPublisher(mods.getOriginInfo(), record);
            fetchLanguages(mods.getLanguage(), record);
            fetchClassifications(mods.getClassification(), record);
            // keywords
            fetchSubjects(mods.getSubject(), record);
            fetchPhysicalDescription(mods.getPhysicalDescription(), record);
            fetchRecordInfo(mods.getRecordInfo(), record);
            fetchNote(mods.getPart(), record);
        }
        return record;
    }

    /**
     * This should fetch record origin info from first /recordInfo/recordOrigin
     * <p/><b>NOTE: KNAV Kramerius 3 format /Periodical/DescriptionBasedIssue</b>
     * @see <a href='http://code.google.com/p/kramerius/source/browse/trunk/import-cmdtool/src/main/resources/model_periodical_MODS.xsl'>model_periodical_MODS.xsl</a>
     */
    private void fetchRecordInfo(List<RecordInfoTypeClient> recordInfos, Record record) {
        if (recordInfos != null && !recordInfos.isEmpty()) {
            List<String> recordOrigins = recordInfos.get(0).getRecordOrigin();
            if (recordOrigins != null && ! recordOrigins.isEmpty()) {
                record.setAttribute(FIELD_RECORD_ORIGIN, recordOrigins.get(0));
            }
        }

    }

    /**
     * This should fetch keywords from first subject/topic*
     * <p/><b>NOTE: KNAV Kramerius 3 format</b>
     * @see <a href='http://code.google.com/p/kramerius/source/browse/trunk/import-cmdtool/src/main/resources/model_periodical_MODS.xsl'>model_periodical_MODS.xsl</a>
     */
    private void fetchSubjects(List<SubjectTypeClient> subjects, Record record) {
        if (subjects != null && !subjects.isEmpty()) {
            SubjectTypeClient subject = subjects.get(0);
            record.setAttribute(FIELD_KEYWORDS, convertStrings(subject.getTopic(), FIELD_STRING_VALUE));
        }
    }
    /**
     * This should fetch pair of extents under each physicalDescription.
     * <p/><b>NOTE: KNAV Kramerius 3 format</b>
     * @see <a href='http://code.google.com/p/kramerius/source/browse/trunk/import-cmdtool/src/main/resources/model_periodical_MODS.xsl'>model_periodical_MODS.xsl</a>
     */
    private void fetchPhysicalDescription(List<PhysicalDescriptionTypeClient> physicalDescriptions, Record record) {
        physicalDescriptions = nonNullList(physicalDescriptions);
        ArrayList<Record> result = new ArrayList<Record>();
        for (PhysicalDescriptionTypeClient physicalDescription : physicalDescriptions) {
            Iterator<String> extents = nonNullList(physicalDescription.getExtent()).iterator();
            if (extents.hasNext()) {
                Record pair = new Record();
                result.add(pair);
                pair.setAttribute(FIELD_PHYSICAL_DESCRIPTIONS_EXTENT, extents.next());
                pair.setAttribute("object", physicalDescription);
                if (extents.hasNext()) {
                    pair.setAttribute(FIELD_PHYSICAL_DESCRIPTIONS_SIZE, extents.next());
                }
            }
        }
        record.setAttribute(FIELD_PHYSICAL_DESCRIPTIONS, toRecords(result));
    }

    /**
     * <pre>{@code
        <classification authority="ddc">123</classification>
        <classification authority="udc">321</classification>
       }</pre>
     * This should fetch pairs of DDC and UDC classifications in arbitrary order
     */
    private void fetchClassifications(List<ClassificationTypeClient> classifications, Record record) {
        classifications = nonNullList(classifications);
        ArrayList<Record> result = new ArrayList<Record>();
        for (int i = 0; i < classifications.size(); i++) {
            ClassificationTypeClient classification = classifications.get(i);
            String authority = classification.getAuthority();
            if (authority == null) {
                continue;
            }
            authority = authority.toLowerCase();
            if ("ddc".equals(authority)) {
                Record r = new Record();
                result.add(r);
                r.setAttribute(FIELD_CLASSIFICATION_DDC, classification.getValue());
                if (i + 1 < classifications.size()) {
                    ClassificationTypeClient next = classifications.get(i + 1);
                    if ("udc".equals(next.getAuthority())) {
                        r.setAttribute(FIELD_CLASSIFICATION_UDC, next.getValue());
                        i+=2;
                    }
                }
            } else if ("udc".equals(authority)) {
                Record r = new Record();
                result.add(r);
                r.setAttribute(FIELD_CLASSIFICATION_UDC, classification.getValue());
                if (i + 1 < classifications.size()) {
                    ClassificationTypeClient next = classifications.get(i + 1);
                    if ("ddc".equals(next.getAuthority())) {
                        r.setAttribute(FIELD_CLASSIFICATION_DDC, next.getValue());
                        i+=2;
                    }
                }
            }
        }
        record.setAttribute(FIELD_CLASSIFICATIONS, toRecords(result));
    }

    /**
     * Example:<br/>{@code
        <language objectPart="summary">
            <languageTerm type= "code" authority="iso639-2b">spa</languageTerm>
        </language>
     *
     * @see <a href='http://www.loc.gov/standards/iso639-2/ISO-639-2_utf-8.txt'>ISO-639-2 CSV list</a>
     * @see <a href='http://www.loc.gov/standards/iso639-2/php/code_list.php'>ISO-639-2 list</a>
     * @see <a href='http://www.loc.gov/standards/mods/userguide/language.html#languageterm'>MODS user guide</a>
     */
    private void fetchLanguages(List<LanguageTypeClient> languages, Record record) {
        languages = nonNullList(languages);
        ArrayList<Record> result = new ArrayList<Record>();
        for (LanguageTypeClient language : languages) {
            for (LanguageTypeClient.LanguageTermClient langTerm : language.getLanguageTerm()) {
                CodeOrTextClient type = langTerm.getType();
                String authority = langTerm.getAuthority();
                String value = langTerm.getValue();
                // XXX for now it supports only codes
                if (type == CodeOrTextClient.CODE && "iso639-2b".equals(authority)) {
                    Record langRecord = new Record();
                    langRecord.setAttribute("languageCode", value);
                    langRecord.setAttribute("languageObject", language);
                    result.add(langRecord);
                    break;
                }
            }
        }
        record.setAttribute(FIELD_LANGUAGES, toRecords(result));
    }

    private void fetchNames(List<NameTypeClient> names, Record record) {
        if (names != null && !names.isEmpty()) {
            ArrayList<Record> authors = new ArrayList<Record>();
            ArrayList<Record> contributors = new ArrayList<Record>();
            for (NameTypeClient name : names) {
                List<NamePartTypeClient> nameParts = nonNullList(name.getNamePart());
                Record nameRec = new Record();
                nameRec.setAttribute(FIELD_NAME_OBJECT, name);
                for (NamePartTypeClient namePart : nameParts) {
                    String type = namePart.getType();
                    String value = namePart.getValue();
                    if ("family".equals(type)) {
                        nameRec.setAttribute("family", value);
                    } else if ("given".equals(type)) {
                        nameRec.setAttribute("given", value);
                    }
                }
                NameRole role = findRole(name.getRole());
                switch(role) {
                    case AUTHOR: authors.add(nameRec); break;
                    case CONTRIBUTOR: contributors.add(nameRec); break;
                }
            }
            record.setAttribute(FIELD_AUTHORS, toRecords(authors));
            record.setAttribute(FIELD_CONTRIBUTORS, toRecords(contributors));
        }
    }

    private enum NameRole {
        AUTHOR, CONTRIBUTOR, OTHER, NONE;
    }

    private static NameRole findRole(List<RoleTypeClient> roles) {
        roles = nonNullList(roles);
        NameRole result = NameRole.NONE;
        for (RoleTypeClient role : roles) {
            List<RoleTermClient> roleTerms = role.getRoleTerm();
            for (RoleTermClient roleTerm : roleTerms) {
                CodeOrTextClient type = roleTerm.getType(); // code
                String value = roleTerm.getValue();
                if (type == CodeOrTextClient.CODE) {
                    if ("ctb".equals(value)) {
                        return NameRole.CONTRIBUTOR;
                    } else if ("cre".equals(value)) {
                        return NameRole.AUTHOR;
                    } else {
                        result = NameRole.OTHER;
                    }
                } else if (type == CodeOrTextClient.TEXT) {
                    if ("Contributor".equals(value)) {
                        return NameRole.CONTRIBUTOR;
                    } else if ("Author".equals(value)) {
                        return NameRole.AUTHOR;
                    } else {
                        result = NameRole.OTHER;
                    }
                }
            }
        }
        return result;
    }
    
    private void fetchLocations(List<LocationTypeClient> locations, Record record) {
        if (locations != null && !locations.isEmpty()) {
            LocationTypeClient location = locations.get(0);
            List<PhysicalLocationTypeClient> physicalLocations = location.getPhysicalLocation();
            if (physicalLocations != null && !physicalLocations.isEmpty()) {
                record.setAttribute(FIELD_SIGLA, physicalLocations.get(0).getValue());
            }
            record.setAttribute(FIELD_SHELF_LOCATORS, convertStrings(location.getShelfLocator(), FIELD_STRING_VALUE));
        }
    }

    private void fetchPeriodicity(List<OriginInfoTypeClient> originInfos, Record record) {
        originInfos = nonNullList(originInfos);
        for (OriginInfoTypeClient originInfo : originInfos) {
            String transliteration = originInfo.getTransliteration();
            if (transliteration == null) {
                continue;
            }
            List<StringPlusAuthorityClient> frequencies = nonNullList(originInfo.getFrequency());
            ArrayList<Record> list = new ArrayList<Record>();
            for (StringPlusAuthorityClient frequency : frequencies) {
                record.setAttribute(FIELD_PERIODICITY_VALUE, frequency.getValue());
                record.setAttribute(FIELD_PERIODICITY_VALUE + "Object", frequency);
                list.add(record);
            }
            record.setAttribute(FIELD_PERIODICITY, toRecords(list));
            break;
        }

    }

    private void fetchPrinterPublisher(List<OriginInfoTypeClient> originInfos, Record record) {
        originInfos = nonNullList(originInfos);
        ArrayList<Record> printers = new ArrayList<Record>();
        ArrayList<Record> publishers = new ArrayList<Record>();

        for (OriginInfoTypeClient originInfo : originInfos) {
            String transliteration = originInfo.getTransliteration();
            Record r;
            List<DateTypeClient> dates;
            if ("printer".equals(transliteration)) {
                r = new Record();
                dates = originInfo.getDateCreated();
                printers.add(r);
            } else if ("publisher".equals(transliteration)) {
                r = new Record();
                dates = originInfo.getDateIssued();
                publishers.add(r);
            } else {
                continue;
            }
            // object
            r.setAttribute("object", originInfo);
            // date
            if (dates != null && !dates.isEmpty()) {
                r.setAttribute(FIELD_PRINTER_PUBLISHER_DATE, dates.get(0).getValue());
            }
            // name
            List<String> publisher = originInfo.getPublisher();
            if (publisher != null && !publisher.isEmpty()) {
                r.setAttribute(FIELD_PRINTER_PUBLISHER_NAME, publisher.get(0));
            }
            // place
            List<PlaceTypeClient> places = originInfo.getPlace();
            if (places != null && !places.isEmpty()) {
                List<PlaceTermTypeClient> placeTerm = places.get(0).getPlaceTerm();
                if (placeTerm != null && !placeTerm.isEmpty()) {
                    r.setAttribute(FIELD_PRINTER_PUBLISHER_PLACE, placeTerm.get(0).getValue());
                }
            }
        }

        record.setAttribute(FIELD_PUBLISHERS, toRecords(publishers));
        record.setAttribute(FIELD_PRINTERS, toRecords(printers));
    }

    private void fetchTitles(List<TitleInfoTypeClient> titleInfos, Record record) {
        if (titleInfos != null && !titleInfos.isEmpty()) {
            boolean foundTitle = false;
            boolean foundAlternatives = false;
            boolean foundKeyTitles = false;
            for (TitleInfoTypeClient titleInfo : titleInfos) {
                String type = titleInfo.getType();
                if (type == null) {
                    if (foundTitle) {
                        continue;
                    }
                    record.setAttribute(FIELD_TITLES, convertStrings(titleInfo.getTitle(), FIELD_STRING_VALUE));
                    record.setAttribute(FIELD_SUBTITLES, convertStrings(titleInfo.getSubTitle(), FIELD_STRING_VALUE));
                    foundTitle = true;
                } else if ("alternative".equals(type) && "Klíčový název".equals(titleInfo.getDisplayLabel())) {
                    if (foundKeyTitles) {
                        continue;
                    }
                    record.setAttribute(FIELD_KEY_TITLES, convertStrings(titleInfo.getTitle(), FIELD_STRING_VALUE));
                    foundKeyTitles = true;
                } else if ("alternative".equals(type)) {
                    if (foundAlternatives) {
                        continue;
                    }
                    record.setAttribute(FIELD_ALTERNATIVE_TITLES, convertStrings(titleInfo.getTitle(), FIELD_STRING_VALUE));
                    foundAlternatives = true;
                }
            }
        }
    }

    private void fetchNote(List<PartTypeClient> parts, Record record) {
        if (parts != null && !parts.isEmpty()) {
            PartTypeClient part = parts.get(0);
            String type = part.getType();
            List<String> notes = part.getText();
            if (notes != null && !notes.isEmpty()) {
                String note = notes.get(0);
                record.setAttribute(FIELD_NOTE, note);
            }
        }
    }

    private Record[] convertStrings(List<String> items, String attrName) {
        items = nonNullList(items);
        ArrayList<Record> list = new ArrayList<Record>();
        for (String item : items) {
            Record itemRecord = new Record();
            itemRecord.setAttribute(attrName, item);
            list.add(itemRecord);
        }
        return toRecords(list);
    }

    public Record convertPage(ModsCollectionClient modsCollection, Record record) {
        List<ModsTypeClient> modsTypes = modsCollection.getMods();
        if (modsTypes != null && !modsTypes.isEmpty()) {
            ModsTypeClient mods = modsTypes.get(0);
            fetchPagePart(mods.getPart(), record);
            List<IdentifierTypeClient> identifiers = mods.getIdentifier();
            record.setAttribute(FIELD_IDENTIFIERS, IdentifierDataSource.convert(identifiers));
        }
        return record;
    }

    private void fetchPagePart(List<PartTypeClient> parts, Record record) {
        if (parts != null && !parts.isEmpty()) {
            PartTypeClient part = parts.get(0);
            String type = part.getType();
            record.setAttribute(FIELD_PAGE_TYPE, type);
            List<String> notes = part.getText();
            if (notes != null && !notes.isEmpty()) {
                String note = notes.get(0);
                record.setAttribute(FIELD_NOTE, note);
            }
            fetchDetails(part.getDetail(), record);
        }
    }

    private List<PartTypeClient> writePart(List<PartTypeClient> parts,
            String pageTypeAttr, String pageIndexAttr, String pageNumberAttr,
            String noteAttr) {

        if (parts == null) {
            parts = new ArrayList<PartTypeClient>();
        }
        if (parts.isEmpty()) {
            parts.add(new PartTypeClient());
        }
        PartTypeClient part = parts.get(0);
        part.setType(pageTypeAttr);
        part.setDetail(writeDetails(part.getDetail(), pageIndexAttr, pageNumberAttr));
        part.setText(writePartNote(part.getText(), noteAttr));
        return parts;
    }

    private List<String> writePartNote(List<String> notes, String note) {
        return updateList(notes, note);
    }

    private void fetchDetails(List<DetailTypeClient> details, Record record) {
        boolean findIndex = true;
        boolean findNumber = true;
        if (details != null && !details.isEmpty()) {
            for (Iterator<DetailTypeClient> it = details.iterator(); it.hasNext() && (findIndex || findNumber);) {
                DetailTypeClient detail = it.next();
                if (findNumber) {
                    findNumber = fetchNumber(detail, FIELD_PAGE_NUMBER, record, FIELD_PAGE_NUMBER);
                }
                if (findIndex) {
                    findIndex = fetchNumber(detail, FIELD_PAGE_INDEX, record, FIELD_PAGE_INDEX);
                }
            }
        }
    }

    /**
     * XXX remove empty details from List?
     */
    private List<DetailTypeClient> writeDetails(List<DetailTypeClient> details, String pageIndexAttr, String pageNumberAttr) {
        if (details == null) {
            details = new ArrayList<DetailTypeClient>();
        }
        boolean findIndex = true;
        boolean findNumber = true;
        for (Iterator<DetailTypeClient> it = details.iterator(); it.hasNext() && (findIndex || findNumber);) {
            DetailTypeClient detail = it.next();
            if (findNumber) {
                findNumber = writePageNumber(detail, FIELD_PAGE_NUMBER, pageNumberAttr);
            }
            if (findIndex) {
                findIndex = writePageNumber(detail, FIELD_PAGE_INDEX, pageIndexAttr);
            }
        }
        if (findNumber) {
            DetailTypeClient detail = new DetailTypeClient();
            detail.setType(FIELD_PAGE_NUMBER);
            writePageNumber(detail, FIELD_PAGE_NUMBER, pageNumberAttr);
            details.add(detail);
        }
        if (findIndex) {
            DetailTypeClient detail = new DetailTypeClient();
            detail.setType(FIELD_PAGE_INDEX);
            writePageNumber(detail, FIELD_PAGE_INDEX, pageIndexAttr);
            details.add(detail);
        }
        return details;
    }

    private boolean fetchNumber(DetailTypeClient detail, String detailType, Record record, String field) {
        if (detailType.equals(detail.getType())) {
            List<String> numbers = detail.getNumber();
            if (numbers != null && !numbers.isEmpty()) {
                record.setAttribute(field, numbers.get(0));
                return false;
            }
        }
        return true;
    }

    private boolean writePageNumber(DetailTypeClient detail, String detailType, String value) {
        if (detailType.equals(detail.getType())) {
            List<String> numbers = detail.getNumber();
            numbers = updateList(numbers, value);
            detail.setNumber(numbers);
            return false;
        }
        return true;
    }

    private static String normalizeAttr(String attr) {
        if (attr != null) {
            attr = attr.trim();
            if (attr.length() == 0) {
                // treat empty attribute as null
                attr = null;
            }
        }
        return attr;
    }

    /**
     * Updates first item of the list.
     *
     * @param <T> item type
     * @param list list to update, can be {@code null}
     * @param item new value for the first item, can be {@code null}
     * @return updated or created list or {@code null} when the list would remain empty
     */
    private static <T> List<T> updateList(List<T> list, T item) {
        if (list == null) {
            list = new ArrayList<T>();
        }
        if (list.isEmpty()) {
            if (item == null) {
                // no item, empty list
                return null;
            }
            list.add(item);
        } else if (item == null && list.size() == 1) {
            // remove last item => empty list
            return null;
        } else {
            list.set(0, item);
        }
        return list;
    }

    private static <T> List<T> nonNullList(List<T> l) {
        return l != null ? l : Collections.<T>emptyList();
    }

    private static Record[] toRecords(List<Record> l) {
        if (l == null || l.isEmpty()) {
            return new Record[] {new Record()};
        } else {
            return l.toArray(new Record[l.size()]);
        }
    }

}
