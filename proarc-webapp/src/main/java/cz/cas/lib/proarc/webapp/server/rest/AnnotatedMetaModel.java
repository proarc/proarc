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
public abstract class AnnotatedMetaModel extends MetaModel {

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_DISPLAYNAME_PARAM)
    @Override
    public abstract String getDisplayName();

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_LEAF_PARAM)
    @Override
    public abstract Boolean isLeaf();

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_PID_PARAM, required = true)
    @Override
    public abstract String getPid();

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_ROOT_PARAM)
    @Override
    public abstract Boolean isRoot();

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_METADATAFORMAT_PARAM)
    @Override
    public abstract String getMetadataFormat();

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_MODSCUSTOMEDITORID_PARAM)
    @Override
    public abstract String getModsCustomEditor();

    @XmlElement(name = DigitalObjectResourceApi.METAMODEL_DATASTREAMEDITOR_PARAM)
    @Override
    public abstract Set<DatastreamEditorType> getDataStreamEditors();

}
