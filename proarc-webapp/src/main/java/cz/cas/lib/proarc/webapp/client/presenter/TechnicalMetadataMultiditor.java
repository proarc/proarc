package cz.cas.lib.proarc.webapp.client.presenter;

import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.events.HasSubmitValuesHandlers;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.IconMenuButton;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.event.EditorLoadEvent;
import cz.cas.lib.proarc.webapp.client.event.HasEditorLoadHandlers;
import cz.cas.lib.proarc.webapp.client.widget.AbstractDatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.BatchDatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.DatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import java.util.HashSet;
import java.util.logging.Logger;


/**
 * Edits Technical metadata in multiple custom editor (all fields, selected fields, plain XML).
 *
 * @author Lukas Sykora
 */
public final class TechnicalMetadataMultiditor extends AbstractDatastreamEditor implements
        BatchDatastreamEditor, Refreshable, Selectable<DigitalObject> {

    private static final Logger LOG = Logger.getLogger(TechnicalMetadataMultiditor.class.getName());
    private static final HashSet<String> ACCEPT_BATCH_MODELS = new HashSet<>();
    static {
        ACCEPT_BATCH_MODELS.add("model:page");
        ACCEPT_BATCH_MODELS.add("model:ndkpage");
        ACCEPT_BATCH_MODELS.add("model:oldprintpage");
        ACCEPT_BATCH_MODELS.add("model:ndkaudiopage");
    }

    private final VLayout uiContainer;
    private final TechnicalXmlEditor technicalXmlEditor;
    private DatastreamEditor activeEditor;
    private final ClientMessages i18n;
    private DigitalObject[] digitalObjects;
    private MenuItem cunstomEditorButton;
    private MenuItem customEditorButton2;
    private final Actions.ActionSource actionSource;
    private HandlerRegistration submitCustomValuesRegistration;
    private final SubmitValuesHandler submitCustomValuesHandler = new SubmitValuesHandler() {
        @Override
        public void onSubmitValues(SubmitValuesEvent event) {
            save();
        }
    };

    private void save() {
        save((Boolean value) -> {
            if (value != null && value) {
                StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
            }
        });
    }

    public void save(BooleanCallback callback) {
        callback = wrapSaveCallback(callback);
        /*if (activeEditor == modsCustomEditor) {
            saveCustomData(callback);
        } else*/
            if (activeEditor == technicalXmlEditor) {
            saveXmlData(callback);
        } else {
            callback.execute(Boolean.TRUE);
        }
    }

    private void saveXmlData(BooleanCallback callback) {
        technicalXmlEditor.save(callback, true, SaveAction.SaveValidation.ASK);
    }

    private BooleanCallback wrapSaveCallback(final BooleanCallback callback) {
        return (Boolean value) -> {
            if (value != null && value) {
                RelationDataSource.getInstance().fireRelationChange(digitalObjects[0].getPid());
            }
            callback.execute(value);
        };
    }


    public TechnicalMetadataMultiditor(ClientMessages i18n) {
        this.i18n = i18n;
        uiContainer = new VLayout();
        technicalXmlEditor = new TechnicalXmlEditor(i18n);
        actionSource = new Actions.ActionSource(this);
        attachDatastreamEditor(technicalXmlEditor);
    }

    private void attachDatastreamEditor(DatastreamEditor deditor) {
        if (deditor instanceof HasEditorLoadHandlers) {
            ((HasEditorLoadHandlers) deditor).addEditorLoadHandler((EditorLoadEvent evt ) -> {fireEvent(evt);});
        }
    }

    @Override
    public void edit(DigitalObjectDataSource.DigitalObject digitalObject) {
        ClientUtils.fine(LOG, "edit %s", digitalObject);
        DigitalObject[] dobjs = digitalObject == null ? null : new DigitalObject[] { digitalObject };
        edit(dobjs);
    }

    @Override
    public void edit(DigitalObject[] items) {
        this.digitalObjects = items;
        if (items == null || items.length == 0) {
            // show nothing or throw exception!
        } else if (items.length == 1) {
            loadTabData(technicalXmlEditor, items[0]);
        } else {
            String firstModelId = null;
            boolean unsupportedBatch = false;
            for (DigitalObject dobj : items) {
                if (firstModelId == null) {
                    firstModelId = dobj.getModelId();
                }
                String currentModelId = dobj.getModelId();
                if (!ACCEPT_BATCH_MODELS.contains(currentModelId) || !firstModelId.equals(currentModelId)) {
                    unsupportedBatch = true;
                    break;
                }
            }
            if (unsupportedBatch) {
                setActiveEditor(null);
            } else {
                loadBatch();
            }
        }
        actionSource.fireEvent();
    }

    private void loadBatch() {
        /*if (activeEditor != modsBatchEditor) {
            modsBatchEditor.refresh();
        }
        setActiveEditor(modsBatchEditor);
        modsBatchEditor.edit(digitalObjects);
        */

    }

    private void loadTabData(DatastreamEditor tab, DigitalObject digitalObject) {
        setEnabledCustom2();
        /*if (tab == modsCustomEditor) {
            loadCustom(digitalObject);
        } else {
        */    setActiveEditor(tab);
            tab.edit(digitalObject);
        //}
    }

    private void setEnabledCustom2() {
        if (customEditorButton2 != null) {
            DigitalObject[] selection = getSelection();
            boolean enable = selection != null && selection.length == 1
                    && "model:bdmarticle".equals(selection[0].getModelId());
            customEditorButton2.setEnabled(enable);
        }
    }

    private void setActiveEditor(DatastreamEditor newEditor) {
        if (newEditor != activeEditor) {
            if (newEditor != null) {
                uiContainer.setMembers(newEditor.getUI());
            } else {
                uiContainer.setMembers(new Canvas[0]);
            }
            if (submitCustomValuesRegistration != null) {
                submitCustomValuesRegistration.removeHandler();
            }
            if (newEditor instanceof HasSubmitValuesHandlers) {
                submitCustomValuesRegistration = ((HasSubmitValuesHandlers) newEditor)
                        .addSubmitValuesHandler(submitCustomValuesHandler);
            }
            activeEditor = newEditor;
        }
    }

    @Override
    public void focus() {
        if (activeEditor != null) {
            activeEditor.focus();
        }
    }

    @Override
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz) || BatchDatastreamEditor.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        SaveAction saveAction = new SaveAction(i18n) {

            @Override
            public void performAction(ActionEvent event) {
            save();
        }

            @Override
            public boolean accept(ActionEvent event) {
                return activeEditor != null;
            }
        };

        return new Canvas[] {
                createTechnicalMenu(),
                Actions.asIconButton(saveAction, actionSource)
        };
    }

    private IconMenuButton createTechnicalMenu() {
        IconMenuButton btnTechnical = Actions.asIconMenuButton(new AbstractAction(
                i18n.ImportBatchItemEditor_TabTechnical_Title(),
                "[SKIN]/actions/edit.png", null) {

            @Override
            public void performAction(ActionEvent event) {
            }

            @Override
            public boolean accept(ActionEvent event) {
                DigitalObject[] selections = getSelection();
                return selections != null && selections.length == 1;
            }
        }, actionSource);

        Menu menuTechnical = Actions.createMenu();
        menuTechnical.addItem(Actions.asMenuItem(
                new SwitchAction(technicalXmlEditor,
                        i18n.ImportBatchItemEditor_TabTechnical_Title(),
                        Page.getAppDir() + "image/silk/16/application_form_edit.png",
                        i18n.ImportBatchItemEditor_TabTechnical_Hint()), actionSource, false));

        btnTechnical.setMenu(menuTechnical);
        return btnTechnical;
    }

    @Override
    public Canvas getUI() {
        return uiContainer;
    }

    @Override
    public void refresh() {
        if (activeEditor != null) {
            Refreshable refreshable = activeEditor.getCapability(Refreshable.class);
            if (refreshable != null) {
                refreshable.refresh();
            } else {
                loadTabData(activeEditor, digitalObjects[0]);
            }
        }

    }

    @Override
    public DigitalObject[] getSelection() {
        return digitalObjects;
    }


    private class SwitchAction extends AbstractAction {

        private final DatastreamEditor tab;
        private final String format;


        public SwitchAction(DatastreamEditor tab, String title, String icon, String tooltip) {
            this(tab, title, icon, tooltip, null);
        }

        public SwitchAction(DatastreamEditor tab, String title, String icon, String tooltip, String format) {
            super(title, icon, tooltip);
            this.tab = tab;
            this.format = format;
        }

        @Override
        public void performAction(ActionEvent event) {
            loadTabData(tab, digitalObjects[0]);
        }

        @Override
        public boolean accept(ActionEvent event) {
            DigitalObject[] selections = getSelection();
            return selections != null && selections.length == 1 && accept(selections[0]);
        }

        boolean accept(DigitalObject obj) {
            return acceptFormat(obj);
        }

        private boolean acceptFormat(DigitalObject obj) {
            return format == null || format != null && format.equals(obj.getModel().getMetadataFormat());
        }
    }
}
