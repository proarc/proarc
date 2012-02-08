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
import cz.fi.muni.xkremser.editor.client.mods.DetailTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.IdentifierTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.ModsCollectionClient;
import cz.fi.muni.xkremser.editor.client.mods.ModsTypeClient;
import cz.fi.muni.xkremser.editor.client.mods.PartTypeClient;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.ClientUtils.DataSourceFieldBuilder;
import cz.incad.pas.editor.client.rpc.ModsGwtServiceAsync;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
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
    /** original object */
    public static final String FIELD_MODS_OBJECT = "ModsCollectionClient";
    public static final String FIELD_PAGE_TYPE = "pageType";
    public static final String FIELD_PAGE_INDEX = "pageIndex";
    public static final String FIELD_PAGE_NUMBER = "pageNumber";
    public static final String FIELD_IDENTIFIERS = "identifiers";
    public static final String FIELD_NOTE = "note";
    public static final String FIELD_XML_SRC = "xmlSource";

    public PageDataSource() {
        setID(ID);
        setDataProtocol(DSProtocol.CLIENTCUSTOM);
        setDataFormat(DSDataFormat.CUSTOM);
        setClientOnly(false);
        DataSourceField pid = new DataSourceFieldBuilder<DataSourceField>(new DataSourceField(FIELD_PID, FieldType.TEXT))
                .primaryKey().build();
        DataSourceField modsObject = new DataSourceFieldBuilder<DataSourceField>(new DataSourceField(FIELD_MODS_OBJECT, FieldType.ANY))
                .hidden().required().build();
        modsObject.setIgnore(true);
        DataSourceField pageType = new DataSourceField(FIELD_PAGE_TYPE, FieldType.TEXT, "Page Type");
        DataSourceField pageIndex = new DataSourceField(FIELD_PAGE_INDEX, FieldType.INTEGER, "Page Index");
        DataSourceField pageNumber = new DataSourceField(FIELD_PAGE_NUMBER, FieldType.TEXT, "Page Number");
        DataSourceField identifiers = new DataSourceField(FIELD_IDENTIFIERS, FieldType.ANY);
        // value treated as Record[]
        identifiers.setTypeAsDataSource(new IdentifierDataSource());
        DataSourceField note = new DataSourceField(FIELD_NOTE, FieldType.TEXT, "Note");
        setFields(pid, modsObject, pageType, pageIndex, pageNumber, identifiers, note);
    }

    public static PageDataSource getInstance() {
        PageDataSource ds = (PageDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new PageDataSource();
        return ds;
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
        ModsCollectionClient modsCollection = convert(record);
        service.write(pidAttr, modsCollection, new AsyncCallback<String>() {

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
        if (pid == null || pid.isEmpty()) {
            dsResponse.setStatus(RPCResponse.STATUS_VALIDATION_ERROR);
            dsResponse.getErrors().put(FIELD_PID, "Missing pid");
            processResponse(dsRequest.getRequestId(), dsResponse);
            return ;
        }
        service.read(pid, new AsyncCallback<ModsCollectionClient>() {

            @Override
            public void onFailure(Throwable caught) {
                dsResponse.setStatus(RPCResponse.STATUS_FAILURE);
                dsResponse.getErrors().put(FIELD_PID, caught.getMessage());
                processResponse(dsRequest.getRequestId(), dsResponse);
            }

            @Override
            public void onSuccess(ModsCollectionClient modsCollection) {
                Record[] data = convert(pid, modsCollection);
                dsResponse.setData(data);
                dsResponse.setStatus(RPCResponse.STATUS_SUCCESS);
                processResponse(dsRequest.getRequestId(), dsResponse);
            }
        });
    }

    public ModsCollectionClient convert(Record record) {
        ModsCollectionClient modsCollection = (ModsCollectionClient) record.getAttributeAsObject(FIELD_MODS_OBJECT);
        if (modsCollection == null) {
            modsCollection = new ModsCollectionClient();
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
        return modsCollection;
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
            LOG.severe("unsupported value type: " + o.getClass() + ", dump: \n" + msg);
            throw new IllegalStateException("unsupported value type: " + o.getClass() + ", dump: \n" + msg);
        }
        return IdentifierDataSource.convert(records);
    }

    public Record[] convert(String pid, ModsCollectionClient modsCollection) {
        Record record = new Record();
        record.setAttribute(FIELD_PID, pid);
        record.setAttribute(FIELD_MODS_OBJECT, modsCollection);
        List<ModsTypeClient> modsTypes = modsCollection.getMods();
        if (modsTypes != null && !modsTypes.isEmpty()) {
            ModsTypeClient mods = modsTypes.get(0);
            fetchPart(mods.getPart(), record);
            List<IdentifierTypeClient> identifiers = mods.getIdentifier();
            record.setAttribute(FIELD_IDENTIFIERS, IdentifierDataSource.convert(identifiers));
        }
        Record[] data = new Record[]{record};
        return data;
    }

    private void fetchPart(List<PartTypeClient> parts, Record record) {
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

}
