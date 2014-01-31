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

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.widget.form.FormGenerator;
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
//        } else if ("model:desInternalRecord".equals(modelId)) {
//            f = DesForms.intenalDocumentForm();
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
            }
            return fw;
        }

        private FormWidget attachVyrizeniPrijemceSubjektGroup(FormWidget fw, final FormItem formItem, final Field group) {
            class Handler implements FormWidgetListener, ChangedHandler {

                @Override
                public void onDataLoad() {
                    resetDokumentPrijemceForm(formItem.getForm(), group);
                }

                @Override
                public void onChanged(ChangedEvent event) {
                    resetDokumentPrijemceForm(formItem.getForm(), group);
                }

            }
            Handler handler = new Handler();
            fw.addFormWidgetListener(handler);
            formItem.addChangedHandler(handler);
            return fw;
        }

        private void resetDokumentPrijemceForm(DynamicForm form, Field group) {
            String value = form.getValueAsString("subjectType");
            boolean isPravnickaOsoba = "PravnickaOsoba".equals(value);
            resetField(form.getField("PostovniAdresa"), isPravnickaOsoba, !isPravnickaOsoba, !isPravnickaOsoba);
            resetField(form.getField("IdentifikatorOrganizace"), !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetField(form.getField("IdentifikatorFyzickeOsoby"), false, true, !isPravnickaOsoba);
            resetField(form.getField("NazevOrganizace"), !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetField(form.getField("OrganizacniUtvar"), !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetField(form.getField("PracovniPozice"), !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
            resetField(form.getField("SidloOrganizace"), !isPravnickaOsoba, isPravnickaOsoba, isPravnickaOsoba);
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
