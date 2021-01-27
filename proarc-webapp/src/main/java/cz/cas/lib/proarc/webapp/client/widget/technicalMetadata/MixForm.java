/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.client.widget.technicalMetadata;

import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

public class MixForm {

    public Form build() {
        Form f = new Form();
        Field audioObject = new FieldBuilder("test").setMaxOccurrences(1).createField();
        f.getFields().add(audioObject);
        List<Field> audioObjectFields = audioObject.getFields();
        audioObjectFields.add(format());
        return f;
    }

    private Field format() {
        return new FieldBuilder("format").setTitle("Test").setMaxOccurrences(1)
                .addField(new FieldBuilder("specificationVersion").setTitle("Test Version").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .addField(new FieldBuilder("value").setTitle("Test value").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField();
    }
}
