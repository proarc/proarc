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
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowMaterialDataSource;

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

    public ListGrid getMaterialGrid() {
        return materialGrid;
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
        materialGrid.setDataSource(WorkflowMaterialDataSource.getInstance(),
                new ListGridField(WorkflowMaterialDataSource.FIELD_PROFILENAME),
                new ListGridField(WorkflowMaterialDataSource.FIELD_VALUE),
                new ListGridField(WorkflowMaterialDataSource.FIELD_WAY),
                new ListGridField(WorkflowMaterialDataSource.FIELD_NOTE),
                new ListGridField(WorkflowMaterialDataSource.FIELD_ID)
        );
        materialGrid.getField(WorkflowMaterialDataSource.FIELD_WAY).setHidden(jobMaterial);
        materialGrid.setExpansionMode(ExpansionMode.DETAIL_FIELD);
        materialGrid.setCanExpandRecords(true);
        materialGrid.setCanExpandMultipleRecords(false);
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
        return form;
    }

    private DynamicForm createFolderForm() {
        DynamicForm form = createExpansionForm();
        TextItem path = new TextItem(WorkflowMaterialDataSource.FIELD_FOLDER_PATH);
        path.setWidth("*");
        form.setDataSource(WorkflowMaterialDataSource.getInstance(),
                path);
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
                new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_RDCZID),
                xml);
        return form;
    }

    private DynamicForm createDigitalDocumentForm() {
        DynamicForm form = createExpansionForm();
        TextItem pid = new TextItem(WorkflowMaterialDataSource.FIELD_DIGITAL_PID);
        pid.setWidth("*");
        form.setDataSource(WorkflowMaterialDataSource.getInstance(),
                pid);
        return form;
    }
}
