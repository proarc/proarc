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

import cz.cas.lib.proarc.common.i18n.BundleValue;
import cz.cas.lib.proarc.common.workflow.model.ValueType;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlIDREF;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.NONE)
public class ParamDefinition extends DisplayableType<ParamDefinition> {

    @XmlAttribute(name = WorkflowProfileConsts.PARAM_REQUIRED_ATT)
    private Boolean required;

    @XmlAttribute(name = WorkflowProfileConsts.PARAM_DATASOURCE_ATT)
    @XmlIDREF
    private ValueMapDefinition datasource;

    @XmlAttribute(name = WorkflowProfileConsts.PARAM_VALUETYPE)
    private String valueType;

    @XmlAttribute(name = WorkflowProfileConsts.PARAM_DISPLAYTYPE)
    private String displayType;

    @XmlAttribute(name = WorkflowProfileConsts.PARAM_OPTIONVALUEFIELD)
    private String optionValueField;

    @XmlAttribute(name = WorkflowProfileConsts.PARAM_OPTIONDISPLAYFIELD)
    private String optionDisplayField;

    public boolean isRequired() {
        return required != null && required;
    }

    public ParamDefinition setRequired(Boolean required) {
        this.required = required;
        return this;
    }

    public ValueMapDefinition getDatasource() {
        return datasource;
    }

    /**
     * See {@link cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles#getValueMap}.
     */
    public String getOptionValueField() {
        if (optionValueField == null && datasource != null
                && datasource.getSource() == ValueMapSource.INTERNAL) {
            return BundleValue.KEY;
        }
        return optionValueField;
    }

    public void setOptionValueField(String optionValueField) {
        this.optionValueField = optionValueField;
    }

    /**
     * See {@link cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles#getValueMap}.
     */
    public String getOptionDisplayField() {
        if (optionDisplayField == null && datasource != null
                && datasource.getSource() == ValueMapSource.INTERNAL) {
            return BundleValue.VALUE;
        }
        return optionDisplayField;
    }

    public void setOptionDisplayField(String optionDisplayField) {
        this.optionDisplayField = optionDisplayField;
    }


    public ValueType getValueType() {
        return ValueType.fromString(valueType);
    }

    public ParamDefinition setValueType(ValueType valueType) {
        setValueType(valueType == null ? null : valueType.name());
        return this;
    }

    public void setValueType(String valueType) {
        this.valueType = valueType;
    }

    public DisplayType getDisplayType() {
        return DisplayType.fromString(displayType);
    }

    public ParamDefinition setDisplayType(DisplayType displayType) {
        setDisplayType(displayType == null ? null : displayType.name());
        return this;
    }

    public ParamDefinition setDisplayType(String displayType) {
        this.displayType = displayType;
        return this;
    }

}
