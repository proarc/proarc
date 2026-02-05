/*
 * Copyright (C) 2022 Lukas Sykora
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

/**
 * Defines a dependency on all steps before have to be completed.
 *
 * @author Lukas Sykora
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class MainBlockerDefinition {

    @XmlAttribute(name = WorkflowProfileConsts.MAIN_BLOCKER_EL_ALLBEFORE)
    private Boolean blockAllBefore;

    public Boolean getBlockAllBefore() {
        return blockAllBefore;
    }

    public void setBlockAllBefore(Boolean blockAllBefore) {
        this.blockAllBefore = blockAllBefore;
    }

}
