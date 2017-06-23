/*
 * Copyright (C) 2015 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.widget.workflow;

import com.google.gwt.user.client.ui.Widget;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.AutoFitTextAreaItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowJobDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowMaterialDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowTaskDataSource;
import cz.cas.lib.proarc.webapp.client.widget.CanvasSizePersistence;
import cz.cas.lib.proarc.webapp.client.widget.ListGridPersistance;

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowMaterialView {

    private final ClientMessages i18n;
    private ListGrid materialGrid;
    private final Widget widget;
    private final boolean jobMaterial;

    public WorkflowMaterialView(ClientMessages i18n) {
        this(i18n, false);
    }

    public WorkflowMaterialView(ClientMessages i18n, boolean jobMaterial) {
        this.i18n = i18n;
        this.jobMaterial = jobMaterial;
        widget = createMaterialList();
    }

    public Widget getWidget() {
        return widget;
    }

    public void setEmptyMaterials() {
        materialGrid.setData(new Record[0]);
    }

    public void setTaskMaterials(String taskId) {
        setMaterials(new Criteria(WorkflowModelConsts.MATERIALFILTER_TASKID, taskId));
    }

    public void setJobMaterials(String jobId) {
        setMaterials(new Criteria(WorkflowModelConsts.MATERIALFILTER_JOBID, jobId));
    }

    private void setMaterials(Criteria c) {
        materialGrid.invalidateCache();
        materialGrid.fetchData(c);
    }

    private Widget createMaterialList() {
        materialGrid = new ListGrid() {

            @Override
            protected Canvas getExpansionComponent(final ListGridRecord record) {
                String type = record.getAttribute(WorkflowMaterialDataSource.FIELD_TYPE);
                DynamicForm form = null;
                if (MaterialType.FOLDER.name().equals(type)) {
                    form = createFolderForm();
                } else if (MaterialType.PHYSICAL_DOCUMENT.name().equals(type)) {
                    form = createPhysicalDocumentForm();
                } else if (MaterialType.DIGITAL_OBJECT.name().equals(type)) {
                    form = createDigitalDocumentForm();
                }
                if (form != null) {
                    return bindExpansinonForm(form, record);
                } else {
                    return super.getExpansionComponent(record);
                }
            }

        };
        materialGrid.setSelectionType(SelectionStyle.SINGLE);
        materialGrid.setExpansionMode(ExpansionMode.DETAIL_FIELD);
        materialGrid.setCanExpandRecords(true);
        materialGrid.setCanExpandMultipleRecords(false);
        materialGrid.setAutoSaveEdits(false);
        materialGrid.setCanSort(false);
        materialGrid.setCanGroupBy(false);
        materialGrid.setWrapCells(true);

        CanvasSizePersistence persistence = new CanvasSizePersistence("WorkflowMaterialView.materialList", materialGrid);
        materialGrid.setHeight(persistence.getHeight());

        materialGrid.setDataSource(WorkflowMaterialDataSource.getInstance(),
                new ListGridField(WorkflowMaterialDataSource.FIELD_PROFILENAME),
                new ListGridField(WorkflowMaterialDataSource.FIELD_VALUE),
                new ListGridField(WorkflowMaterialDataSource.FIELD_WAY),
                new ListGridField(WorkflowMaterialDataSource.FIELD_NOTE),
                new ListGridField(WorkflowMaterialDataSource.FIELD_ID)
        );
        materialGrid.getField(WorkflowMaterialDataSource.FIELD_WAY).setHidden(jobMaterial);
        String dbPrefix = jobMaterial ? "WorkflowJobFormView.WorkflowMaterialView"
                : "WorkflowTaskFormView.WorkflowMaterialView";
        ListGridPersistance listGridPersistance = new ListGridPersistance(
                dbPrefix, materialGrid);
        materialGrid.setViewState(listGridPersistance.getViewState());
        return materialGrid;
    }

    private DynamicForm bindExpansinonForm(final DynamicForm form, final Record record) {
        form.addDrawHandler(new DrawHandler() {

            @Override
            public void onDraw(DrawEvent event) {
                form.editRecord(record);
            }
        });
        return form;
    }

    private DynamicForm createExpansionForm() {
        DynamicForm form = new DynamicForm();
        form.setDataSource(WorkflowMaterialDataSource.getInstance());
        form.addSubmitValuesHandler(new SubmitValuesHandler() {

            @Override
            public void onSubmitValues(SubmitValuesEvent event) {
                save(event.getForm());
            }
        });
        return form;
    }

    private void save(DynamicForm form) {
        final String type = form.getValueAsString(WorkflowMaterialDataSource.FIELD_TYPE);
        form.saveData(new DSCallback() {

            @Override
            public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                if (RestConfig.isStatusOk(dsResponse)) {
                    if (MaterialType.PHYSICAL_DOCUMENT.name().equals(type)) {
                        // the name of job might be changed, refresh the job and task
                        DSResponse resetCache = new DSResponse();
                        resetCache.setInvalidateCache(true);
                        resetCache.setOperationType(DSOperationType.UPDATE);
                        WorkflowTaskDataSource.getInstance().updateCaches(resetCache);
                        WorkflowJobDataSource.getInstance().updateCaches(resetCache);
                    }
                }
            }
        });
    }

    private DynamicForm createFolderForm() {
        DynamicForm form = createExpansionForm();
        TextItem path = new TextItem(WorkflowMaterialDataSource.FIELD_FOLDER_PATH);
        path.setWidth("*");
        form.setDataSource(WorkflowMaterialDataSource.getInstance(),
                path, createNoteItem(), createSaveItem());
        return form;
    }

    private DynamicForm createPhysicalDocumentForm() {
        DynamicForm form = createExpansionForm();
        TextAreaItem xml = new TextAreaItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_METADATA);
        xml.setWidth("*");
        form.setDataSource(WorkflowMaterialDataSource.getInstance(),
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_CATALOG),
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_BARCODE),
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_FIELD001),
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_SIGLA),
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_SIGNATURE),
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_DETAIL),
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_ISSUE),
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_VOLUME),
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_YEAR),
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_RDCZID),
                createNoteItem(),
                xml,
                createSaveItem());
        return form;
    }

    private DynamicForm createDigitalDocumentForm() {
        DynamicForm form = createExpansionForm();
        TextItem pid = new TextItem(WorkflowMaterialDataSource.FIELD_DIGITAL_PID);
        pid.setWidth("*");
        form.setDataSource(WorkflowMaterialDataSource.getInstance(),
                pid, createNoteItem(), createSaveItem());
        return form;
    }

    private SubmitItem createSaveItem() {
        SubmitItem submit = new SubmitItem();
        submit.setTitle(i18n.SaveAction_Title());
        submit.setStartRow(true);
        submit.setEndRow(false);
        return submit;
    }

    private AutoFitTextAreaItem createNoteItem() {
        AutoFitTextAreaItem note = new AutoFitTextAreaItem(WorkflowMaterialDataSource.FIELD_NOTE);
        note.setStartRow(true);
        note.setColSpan("*");
        note.setWidth("*");
        return note;
    }

}
