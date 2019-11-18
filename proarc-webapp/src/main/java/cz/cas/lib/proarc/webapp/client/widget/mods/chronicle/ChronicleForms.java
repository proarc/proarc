package cz.cas.lib.proarc.webapp.client.widget.mods.chronicle;

import com.smartgwt.client.widgets.form.DynamicForm;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.object.chronicle.ChroniclePlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkFormGenerator;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkForms;
import cz.cas.lib.proarc.webapp.client.widget.mods.PageForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.oldprint.OldPrintChapterForms;
import cz.cas.lib.proarc.webapp.client.widget.mods.oldprint.OldPrintMonographTitleForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.oldprint.OldPrintSupplementForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.oldprint.OldPrintVolumeForm;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;

/**
 * The helper for old print forms.
 *
 * @author Jan Pokorsky
 */
public class ChronicleForms {

    private final ClientMessages i18n;
    private final String activeLocale;
    private final String prefix = "complex:";

    public ChronicleForms(ClientMessages i18n) {
        this.i18n = i18n;
        activeLocale = LanguagesDataSource.activeLocale();
    }
    public DynamicForm getForm(MetaModelDataSource.MetaModelRecord model, String prefix) {
        String modelId = prefix + model.getId();
        Form f;
        if (ChroniclePlugin.MODEL_CHRONICLETITLE.equals(modelId)) {
            f = new SimpleChronicleTitleForm().build();
        } else if (ChroniclePlugin.MODEL_CHRONICLEVOLUME.equals(modelId)) {
            f = new SimpleChronicleVolumeForm().build();
        } else if (ChroniclePlugin.MODEL_CHRONICLESUPPLEMENT.equals(modelId)) {
            f = new SimpleChronicleSupplementForm().build();
        } else if ((this.prefix + ChroniclePlugin.MODEL_CHRONICLETITLE).equals(modelId)) {
            f = new ChronicleTitleForm().build();
        } else if ((this.prefix + ChroniclePlugin.MODEL_CHRONICLEVOLUME).equals(modelId)) {
            f = new ChronicleVolumeForm().build();
        } else if ((this.prefix + ChroniclePlugin.MODEL_CHRONICLESUPPLEMENT).equals(modelId)) {
            f = new ChronicleSupplementForm().build();
        } else if (ChroniclePlugin.MODEL_PAGE.equals(modelId)) {
            return new PageForm(i18n);
        } else {
            return null;
        }
        return new NdkFormGenerator(f, activeLocale).generateForm();
    }

    public static FieldBuilder createLangTermValue() {
        return new FieldBuilder("value").setTitle("Language - M").setMaxOccurrences(1)
                .setType(Field.COMBO).setRequired(true)
                .setHint("Přesné určení jazyka kódem.<p>Nutno použít kontrolovaný slovník ISO 639-2.")
                .setOptionDataSource(new FieldBuilder("ndk.mods.languageTerms").setWidth("300")
                                .addField(new FieldBuilder("title").setTitle("Name").createField())
                                .addField(new FieldBuilder("value").setTitle("Language").createField())
                                .addField(new FieldBuilder("type").setTitle("Type").createField())
                                .addField(new FieldBuilder("authority").setTitle("Authority").createField())
                                .createField(),
                        "value", "type", "authority");
    }

    public static Field roleTerm(String valueTitle, Boolean isValueRequired,
                                 String authorityTitle, Boolean isAuthorityRequired,
                                 String typeTitle, Boolean isTypeRequired) {

        // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
        return new FieldBuilder("roleTerm").setMaxOccurrences(1)
                .addField(new FieldBuilder("value").setTitle(valueTitle).setMaxOccurrences(1)
                        .setType(Field.COMBO)
                        .setWidth("200")
                        .setRequired(isValueRequired)
                        .setHint("Kód role z kontrolovaného slovníku.")
                        .setOptionDataSource(new FieldBuilder(BundleName.MODS_ROLES.getValueMapId()).setWidth("300")
                                .addField(new FieldBuilder("value").setTitle("Kód").createField())
                                .addField(new FieldBuilder("label").setTitle("Popis").createField())
                                .createField(), "value", "type", "authority")
                        .createField()) // value
                // @type, codeOrText(code, text)
                .addField(new FieldBuilder("type").setTitle(typeTitle).setMaxOccurrences(1)
                        .setType(Field.SELECT)
                        .setRequired(isTypeRequired)
                        .addMapValue("code", "code")
                        .addMapValue("text", "text")
                        .createField()) // @type
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("authority").setTitle(authorityTitle).setMaxOccurrences(1)
                        .setType(Field.COMBO)
                        .setWidth("200")
                        .setRequired(isAuthorityRequired)
                        .addMapValue("marcrelator", "marcrelator")
                        .setHint("Údaje o kontrolovaném slovníku využitém k popisu role."
                                + "<p>K popisu výše uvedeného MARC seznamu nutno uvést authority=“marcrelator“.")
                        .createField()) // authority
                .createField(); // roleTerm
    }

    public static Field part() {
        return new FieldBuilder("part").setTitle("Vypůjčení").setMaxOccurrences(10)
                .setHint("Popis půčování.")
                .addField(new FieldBuilder("extent").setMaxOccurrences(1)
                        .addField(new FieldBuilder("start").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Vypůjčení").setMaxOccurrences(1).setType(Field.TEXT)
                                        .createField()) // value
                                .createField()) // start
                        .addField(new FieldBuilder("end").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Vrácení").setMaxOccurrences(1).setType(Field.TEXT)
                                        .createField()) // value
                                .createField()) // end
                        .createField()) // extent
                .createField(); // part
    }

}
