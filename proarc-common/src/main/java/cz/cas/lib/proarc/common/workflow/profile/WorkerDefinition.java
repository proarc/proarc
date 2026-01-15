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

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlValue;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class WorkerDefinition {

    @XmlAttribute(name = WorkflowProfileConsts.WORKER_ACTUAL_ATT)
    private Boolean actual;

    @XmlValue
    private String username;

    /**
     * Gets whether to use the user that creates the workflow.
     */
    public boolean getActual() {
        return actual != null && actual;
    }

    public WorkerDefinition setActual(boolean actual) {
        this.actual = actual ? actual : null;
        return this;
    }

    public String getUsername() {
        return username;
    }

    public WorkerDefinition setUsername(String username) {
        this.username = username;
        return this;
    }

}
