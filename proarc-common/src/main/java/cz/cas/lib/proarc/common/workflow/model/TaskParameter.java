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
package cz.cas.lib.proarc.common.workflow.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import cz.cas.lib.proarc.common.workflow.WorkflowException;
import java.math.BigDecimal;
import java.sql.Timestamp;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;
import org.joda.time.DateTime;
import org.joda.time.format.ISODateTimeFormat;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public class TaskParameter {

    @XmlAttribute(name = WorkflowModelConsts.PARAMETER_TASKID)
    private BigDecimal taskId;
    /**
     * The name of a parameter in workflow profile.
     */
    @XmlAttribute(name = WorkflowModelConsts.PARAMETER_PROFILENAME)
    private String paramRef;
    @XmlAttribute(name = WorkflowModelConsts.PARAMETER_VALUETYPE)
    private ValueType valueType;

    // Typed values are XML transient for now. See getValue.
    private String valueString;
    private BigDecimal valueNumber;
    private Timestamp valueDateTime;

    public BigDecimal getTaskId() {
        return taskId;
    }

    public TaskParameter addTaskId(BigDecimal taskId) {
        setTaskId(taskId);
        return this;
    }

    public void setTaskId(BigDecimal taskId) {
        this.taskId = taskId;
    }

    public String getParamRef() {
        return paramRef;
    }

    public TaskParameter addParamRef(String paramRef) {
        setParamRef(paramRef);
        return this;
    }

    public void setParamRef(String paramRef) {
        this.paramRef = paramRef;
    }

    public Object getValueAsObject() {
        switch (valueType) {
            case DATETIME:
                return getValueDateTime();
            case NUMBER:
                return getValueNumber();
            default:
                return getValueString();
        }
    }

    @XmlValue
    /* Jackson 2.0 ignores @XmlValue for methods and thus @JsonProperty. */
    @JsonProperty
    public String getValue() {
        String value = null;
        if (valueType == ValueType.NUMBER) {
            BigDecimal n = getValueNumber();
            value = n == null ? null : n.toPlainString();
        } else if (valueType == ValueType.DATETIME) {
            Timestamp t = getValueDateTime();
            if (t != null) {
                value = ISODateTimeFormat.dateTime().withZoneUTC().print(t.getTime());
            }
        } else if (valueType == ValueType.STRING) {
            value = getValueString();
        } else {
            throw new IllegalStateException("Unsupported type: " + valueType);
        }
        return value;
    }

    public void setValue(String value) throws WorkflowException {
        if (valueType == ValueType.NUMBER) {
            BigDecimal number = null;
            if (value != null && !value.isEmpty()) {
                if ("true".equals(value)) {
                    number = BigDecimal.ONE;
                } else if ("false".equals(value)) {
                    number = BigDecimal.ZERO;
                } else {
                    try {
                        number = new BigDecimal(value);
                    } catch (NumberFormatException ex) {
                        throw new WorkflowException(value, ex)
                                .addParamNumberFormat(paramRef, value);
                    }
                }
            }
            setValueNumber(number);
        } else if (valueType == ValueType.DATETIME) {
            Timestamp t = null;
            if (value != null) {
                try {
                    DateTime dateTime = ISODateTimeFormat.dateOptionalTimeParser().withZoneUTC().parseDateTime(value);
                    t = new Timestamp(dateTime.getMillis());
                } catch (IllegalArgumentException ex) {
                    throw new WorkflowException(value, ex)
                            .addParamDateTimeFormat(paramRef, value);
                }
            }
            setValueDateTime(t);
        } else { // valueType == Type.STRING and others
            setValueString(value);
        }
    }

    public TaskParameter addValue(ValueType type, String value) throws WorkflowException {
        setValueType(type);
        setValue(value);
        return this;
    }

    public String getValueTypeAsString() {
        return valueType == null ? null : valueType.name();
    }

    public void setValueTypeAsString(String type) {
        setValueType(ValueType.fromString(type));
    }

    public ValueType getValueType() {
        return valueType;
    }

    public void setValueType(ValueType valueType) {
        this.valueType = valueType;
    }

    public TaskParameter addValueType(ValueType type) {
        setValueType(type);
        return this;
    }

    public String getValueString() {
        return valueString;
    }

    public void setValueString(String val) {
        this.valueString = val;
    }

    public TaskParameter addValueString(String value) {
        setValueType(ValueType.STRING);
        this.valueNumber = null;
        this.valueDateTime = null;
        setValueString(value);
        return this;
    }

    public BigDecimal getValueNumber() {
        return valueNumber;
    }

    public void setValueNumber(BigDecimal val) {
        // postgresql adds trailing zeros on jdbc read -> strip them
        this.valueNumber = val == null ? null
                // stripTrailingZeros does not work for zero, e.g. 0E-9; JDK bug 6480539
                : BigDecimal.ZERO.compareTo(val) == 0 ? BigDecimal.ZERO : val.stripTrailingZeros();
    }

    public TaskParameter addValueNumber(BigDecimal value) {
        setValueType(ValueType.NUMBER);
        this.valueString = null;
        this.valueDateTime = null;
        setValueNumber(value);
        return this;
    }

    public Timestamp getValueDateTime() {
        return valueDateTime;
    }

    public void setValueDateTime(Timestamp val) {
        this.valueDateTime = val;
    }

    public TaskParameter addValueDateTime(Timestamp value) {
        setValueType(ValueType.DATETIME);
        this.valueNumber = null;
        this.valueString = null;
        setValueDateTime(value);
        return this;
    }

}
