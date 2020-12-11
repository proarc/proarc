package cz.cas.lib.proarc.webapp.client.presenter;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.TechnicalCodingHistoryCustomDataSource;
import cz.cas.lib.proarc.webapp.client.ds.TextDataSource;
import cz.cas.lib.proarc.webapp.client.widget.CodeMirror;
import cz.cas.lib.proarc.webapp.client.widget.DatastreamEditor;
import java.util.logging.Logger;

public class TechnicalXmlCodingHistoryEditor implements DatastreamEditor, RefreshAction.Refreshable {

    private static final Logger LOG = Logger.getLogger(TechnicalXmlCodingHistoryEditor.class.getName());
    private final CodeMirror sourceForm;
    private DigitalObject digitalObject;
    private String xml;
    private Long timestamp;
    private final ClientMessages i18n;

    public TechnicalXmlCodingHistoryEditor(ClientMessages i18n) {
        sourceForm = new CodeMirror();
        this.i18n = i18n;
    }


    @Override
    public void edit(DigitalObject digitalObject) {
        this.digitalObject = digitalObject;
        this.sourceForm.clearHistory();
        refresh(true);
    }

    @Override
    public void focus() {
        this.sourceForm.getUI().focus();
    }

    @Override
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (RefreshAction.Refreshable.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        return new Canvas[0];
    }

    @Override
    public Canvas getUI() {
        return sourceForm.getUI();
    }

    @Override
    public void refresh() {
        refresh(false);
    }

    private void refresh(boolean cleanHistory) {
        if (digitalObject != null) {
            if (digitalObject.getPid() != null) {
                Criteria pidCriteria = new Criteria(ModsCustomDataSource.FIELD_PID, digitalObject.getPid());
                if (digitalObject.getBatchId() != null) {
                    pidCriteria.addCriteria(ModsCustomDataSource.FIELD_BATCHID, digitalObject.getBatchId());
                }
                TextDataSource.getTechnicalMetadataCodingHistory().fetchData(pidCriteria, new DSCallback() {
                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        handleFetchResponse(response, TextDataSource.FIELD_CONTENT, cleanHistory);
                    }
                });
            }
        }
    }

    private void handleFetchResponse(DSResponse response, String property, boolean cleanHistory) {
        xml = "";
        if (RestConfig.isStatusOk(response)) {
            Record[] data = response.getData();
            if (data != null && data.length == 1) {
                xml = data[0].getAttribute(property);
                timestamp = data[0].getAttributeAsLong(TextDataSource.FIELD_TIMESTAMP);
            }
        }
        sourceForm.setContent(xml);
        if (cleanHistory) {
            sourceForm.clearHistory();
        }
    }

    public void save(BooleanCallback callback, boolean ask, SaveAction.SaveValidation strategy) {
        String sx = sourceForm.getContent();
        final String newXml = sx == null || sx.trim().isEmpty() ? null : sx;
        String oldXml = xml == null || xml.isEmpty() ? null : xml;

        if (oldXml != null && oldXml.equals(newXml)) {
            callback.execute(Boolean.FALSE);
            return ;
        }
        SaveAction.saveTask(new SaveAction.Savable() {

            @Override
            public void save(BooleanCallback result) {
                saveImpl(result, newXml);
            }

            @Override
            public void validate(BooleanCallback result) {
                result.execute(true);
            }
        }, callback, ask, strategy, i18n);
    }

    private void saveImpl(BooleanCallback callback, String newXml) {
        TechnicalCodingHistoryCustomDataSource.getInstance().saveXmlDescription(digitalObject, newXml, timestamp, new TechnicalCodingHistoryCustomDataSource.DescriptionSaveHandler() {

            @Override
            protected void onSave(TechnicalCodingHistoryCustomDataSource.DescriptionMetadata dm) {
                super.onSave(dm);
                refresh(false);
                callback.execute(Boolean.TRUE);
            }

            @Override
            protected void onError() {
                super.onError();
                callback.execute(Boolean.FALSE);
            }

            @Override
            protected void onValidationError() {
                // Do not ignore XML validation!
                String msg = i18n.SaveAction_IgnoreRemoteInvalid_Msg(getValidationMessage());

                SC.ask(i18n.SaveAction_Title(), msg, value -> {
                    // save again
                    if (value != null && value) {
                        TechnicalCodingHistoryCustomDataSource.getInstance().saveXmlDescription(digitalObject, newXml, timestamp, this, true);
                    }
                });
            }

        });
    }
}
