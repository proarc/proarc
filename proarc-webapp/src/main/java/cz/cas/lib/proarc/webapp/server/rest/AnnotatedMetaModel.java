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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import java.util.Locale;
import java.util.Set;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Helper class to annotate {@link MetaModel} properties.
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AnnotatedMetaModel {

    private final MetaModel model;
    private final Locale l;

    public AnnotatedMetaModel(MetaModel model, Locale l) {
        this.model = model;
        this.l = l;
    }

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_DISPLAYNAME_PARAM)
    public String getDisplayName() {
        return model.getDisplayName(l.getLanguage());
    }

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_LEAF_PARAM)
    public Boolean isLeaf() {
        return model.isLeaf();
    }

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_PID_PARAM, required = true)
    public String getPid() {
        return model.getPid();
    }

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_ROOT_PARAM)
    public Boolean isRoot() {
        return model.isRoot();
    }

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_METADATAFORMAT_PARAM)
    public String getMetadataFormat() {
        return model.getMetadataFormat();
    }

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_MODSCUSTOMEDITORID_PARAM)
    public String getModsCustomEditor() {
        return model.getModsCustomEditor();
    }

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_DATASTREAMEDITOR_PARAM)
    public Set<DatastreamEditorType> getDataStreamEditors() {
        return model.getDataStreamEditors();
    }

}
