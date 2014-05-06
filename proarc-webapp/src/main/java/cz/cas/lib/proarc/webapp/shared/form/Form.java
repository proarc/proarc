/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.shared.form;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;

/**
 * The form definition that can be shared between client and server.
 *
 * It supports repeatable fields and nested forms.
 *
 * <p>XXX add support for XML or JSON declarations
 *
<pre>
form: {
    members: [
        {
            name: "title", title: "Titul", hint: "Title hint", maxOccurences: 4,
//            template: {value: { type:text, required: true, canRemove:false }, lang: {visible}},
            members: [
                { field:{ name:"value", type:text, required: true }} ,
                { field:{ name:"value", type:text, required: false }, field:{name:"lang", type:"combo", valueMap:{}} }
            ]
        },
        {
            name: "identifier", title: "Identifikator", hint: "Popis I", maxOccurences: 1
            members: [
                { value: { type:text, required: true, canRemove:false } },
                { value: { type:text, required: false } }
            ]
        }
    ]
}</pre>
 *
 * @author Jan Pokorsky
 */
public class Form {

    private String itemWidth = "400";
    private List<Field> fields;

    public String getItemWidth() {
        return itemWidth;
    }

    public void setItemWidth(String itemWidth) {
        this.itemWidth = itemWidth;
    }

    public List<Field> getFields() {
        if (fields == null) {
            fields = new ArrayList<Field>();
        }
        return fields;
    }

}
