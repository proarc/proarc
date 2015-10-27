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
package cz.cas.lib.proarc.common.workflow.profile;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlID;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class ParamDefinition extends DisplayableType<ParamDefinition> {

    @XmlAttribute(name = WorkflowProfileConsts.PARAM_NAME_ATT, required = true)
    @XmlID
    private String name;

    @XmlAttribute(name = WorkflowProfileConsts.PARAM_REQUIRED_ATT)
    private Boolean required;

    @XmlAttribute(name = WorkflowProfileConsts.PARAM_DATASOURCE_ATT)
    private String datasource;

    public String getName() {
        return name;
    }

    public ParamDefinition setName(String name) {
        this.name = name;
        return this;
    }

    public boolean isRequired() {
        return required != null && required;
    }

    public ParamDefinition setRequired(Boolean required) {
        this.required = required;
        return this;
    }

    public String getDatasource() {
        return datasource;
    }

    public ParamDefinition setDatasource(String datasource) {
        this.datasource = datasource;
        return this;
    }

}
