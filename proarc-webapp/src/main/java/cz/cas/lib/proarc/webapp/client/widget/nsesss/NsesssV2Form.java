/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.widget.nsesss;

import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.ChangeEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangeHandler;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.widget.form.FormGenerator;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableForm.Row;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.FormWidget;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.FormWidgetListener;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.logging.Logger;

/**
 * The form to edit NSESSS V2 format.
 *
 * @author Jan Pokorsky
 */
public class NsesssV2Form {

    private static final Logger LOG = Logger.getLogger(NsesssV2Form.class.getName());
    private final String activeLocale;
    private final MetaModelRecord model;

    public NsesssV2Form(ClientMessages i18n, MetaModelRecord model) {
        activeLocale = LanguagesDataSource.activeLocale();
        this.model = model;
    }

    public DynamicForm getForm() {
        String modelId = model.getId();
        Form f;
        if ("model:desFolder".equals(modelId)) {
            f = DesForms.spisForm();
        } else if ("model:desInternalRecord".equals(modelId)) {
            f = DesForms.intenalDocumentForm();
        } else if ("model:desExternalRecord".equals(modelId)) {
            f = DesForms.externalDocumentForm();
        } else {
            return null;
        }
        return new NsesssV2FormGenerator(f, activeLocale).generateForm();
    }

    private static class NsesssV2FormGenerator extends FormGenerator {

        public NsesssV2FormGenerator(Form f, String activeLocale) {
            super(f, activeLocale);
        }

        @Override
        protected FormWidget customizeNestedForm(FormWidget fw, Field f) {
            if (!Field.CUSTOM_FORM.equals(f.getType())) {
                return fw;
            }
            final DynamicForm form = (DynamicForm) fw.getWidget();
            if ("Subjekt".equals(f.getName())
                    && f.getParent() != null && "Prijemce".equals(f.getParent().getName())) {

                FormItem formItem = form.getField("subjectType");
                if (formItem != null) {
                    return attachVyrizeniPrijemceSubjektGroup(fw, formItem, f);
                }
            } else if ("Subjekt".equals(f.getName())
                    && f.getParent() != null && "Odesilatel".equals(f.getParent().getName())) {

                FormItem formItem = form.getField("subjectType");
                if (formItem != null) {
                    return attachPuvodDorucenyDokumentOdesilatelSubjectGroup(fw, formItem, f);
                }
            }
            return fw;
        }

        private FormWidget attachVyrizeniPrijemceSubjektGroup(FormWidget fw, final FormItem itemSubject, final Field group) {
            class Handler implements FormWidgetListener, ChangeHandler {

                @Override
                public void onDataLoad() {
                    if (itemSubject.getValue() == null) {
                        // init of empty form
                        itemSubject.setValue("PravnickaOsoba");
                    }
                    resetDokumentPrijemceForm(itemSubject.getForm(), group, null, false);
                }

                @Override
                public void onChange(ChangeEvent event) {
                    String name = event.getItem().getName();
                    Object oldValue = event.getOldValue();
                    Object value = event.getValue();
                    if (oldValue == null || !oldValue.equals(value)) {
                        resetDokumentPrijemceForm(itemSubject.getForm(), group, String.valueOf(value), true);
                    }
                }

            }
            Handler handler = new Handler();
            fw.addFormWidgetListener(handler);
            itemSubject.addChangeHandler(handler);
            return fw;
        }

        /** XXX ugly; requires refactoring! */
        private FormWidget attachPuvodDorucenyDokumentOdesilatelSubjectGroup(FormWidget fw, final FormItem itemSubject, final Field group) {
            class Handler implements FormWidgetListener, ChangeHandler {

                @Override
                public void onDataLoad() {
                    if (itemSubject.getValue() == null) {
                        // init of empty form
                        itemSubject.setValue("PravnickaOsoba");
                    }
                    resetDorucenyDokumentOdesilatelForm(itemSubject.getForm(), group, null, false);
                }

                @Override
                public void onChange(ChangeEvent event) {
                    String name = event.getItem().getName();
                    Object oldValue = event.getOldValue();
                    Object value = event.getValue();
                    if (oldValue == null || !oldValue.equals(value)) {
                        resetDorucenyDokumentOdesilatelForm(itemSubject.getForm(), group, String.valueOf(value), true);
                    }
                }

            }
            Handler handler = new Handler();
            fw.addFormWidgetListener(handler);
            itemSubject.addChangeHandler(handler);
            return fw;
        }

        /** Switches form members according to selected subject type. */
        private void resetDorucenyDokumentOdesilatelForm(DynamicForm form, Field group, String subjectTypeValue, boolean clear) {
            String value = subjectTypeValue != null ? subjectTypeValue : form.getValueAsString("subjectType");
            boolean isPravnickaOsoba = "PravnickaOsoba".equals(value);
            resetField(form.getField("PostovniAdresa"), clear && isPravnickaOsoba, !isPravnickaOsoba, !isPravnickaOsoba);
            resetIdentifierField(form.getField("IdentifikatorOrganizace"), clear && !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetIdentifierField(form.getField("IdentifikatorFyzickeOsoby"), false, true, false);
            resetField(form.getField("NazevFyzickeOsoby"),  false, true, !isPravnickaOsoba);
            resetField(form.getField("NazevOrganizace"), clear && !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetField(form.getField("OrganizacniUtvar"), clear && !isPravnickaOsoba, isPravnickaOsoba, false);
            resetField(form.getField("PracovniPozice"), clear && !isPravnickaOsoba, isPravnickaOsoba, false);
            resetField(form.getField("SidloOrganizace"), clear && !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetField(form.getField("ElektronickyKontakt"), false, true, !isPravnickaOsoba);
        }

        /** Switches form members according to selected subject type. */
        private void resetDokumentPrijemceForm(DynamicForm form, Field group, String subjectTypeValue, boolean clear) {
            String value = subjectTypeValue != null ? subjectTypeValue : form.getValueAsString("subjectType");
            boolean isPravnickaOsoba = "PravnickaOsoba".equals(value);
            resetField(form.getField("PostovniAdresa"), clear && isPravnickaOsoba, !isPravnickaOsoba, !isPravnickaOsoba);
            resetIdentifierField(form.getField("IdentifikatorOrganizace"), clear && !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetIdentifierField(form.getField("IdentifikatorFyzickeOsoby"), false, true, !isPravnickaOsoba);
            resetField(form.getField("NazevOrganizace"), clear && !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetField(form.getField("OrganizacniUtvar"), clear && !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetField(form.getField("PracovniPozice"), clear && !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetField(form.getField("SidloOrganizace"), clear && !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
        }

        /** Resets nested Identifier members. */
        private void resetIdentifierField(FormItem fi, boolean clear, boolean visible, boolean required) {
            Canvas canvas = ((RepeatableFormItem) fi).getCanvas();
            RepeatableForm rf = (RepeatableForm) canvas;
            for (Row row : rf.getRows()) {
                ValuesManager vm = row.getForm();
                DynamicForm[] nestedForms = vm.getMembers();
                for (DynamicForm nf : nestedForms) {
                    for (FormItem formItem : nf.getFields()) {
                        formItem.setRequired(required);
                    }
                }
            }
            resetField(fi, clear, visible, required);
        }
        private void resetField(FormItem fi, boolean clear, boolean visible, boolean required) {
            fi.setVisible(visible);
            fi.setRequired(required);
            clearIfTrue(clear, fi);
        }

        private void clearIfTrue(boolean state, FormItem fi) {
            if (state) {
                fi.clearValue();
//                fi.setValue((Object) null);
            }
        }

    }

}
