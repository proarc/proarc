package cz.cas.lib.proarc.webapp.client.widget.technicalMetadata;

import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

public class CodingHistoryForm {

    public Form build() {
        Form f = new Form();
        Field codingHistory = new FieldBuilder("codingHistory").setMaxOccurrences(1).createField();
        f.getFields().add(codingHistory);

        List<Field> codingHistoryFields = codingHistory.getFields();
        codingHistoryFields.add(name());
        codingHistoryFields.add(values());

        return f;
    }

    private Field name() {
        return new FieldBuilder("name").setTitle("Name").setMaxOccurrences(1)
                .setType(Field.TEXT).createField();
    }

    private Field values() {
        return new FieldBuilder("values").setTitle("Values").setMaxOccurrences(100)
                .addField(new FieldBuilder("arity").setTitle("Arity").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .addField(new FieldBuilder("type").setTitle("Type").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .addField(property())
                //.addField(new FieldBuilder("value").setTitle("Value").setMaxOccurrences(100).setType(Field.TEXT).createField())
                .createField();
    }

    private Field property() {
        return new FieldBuilder("property").setTitle("Property").setMaxOccurrences(100)
                .addField(name())
                .addField(valuesInProperty())
                .createField();
    }

    private Field valuesInProperty() {
        return new FieldBuilder("values").setTitle("Values").setMaxOccurrences(100)
                .addField(new FieldBuilder("arity").setTitle("Arity").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .addField(new FieldBuilder("type").setTitle("Type").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .addField(new FieldBuilder("value").setTitle("Value").setMaxOccurrences(1).setType(Field.TEXTAREA).createField())
                .createField();
    }
}
