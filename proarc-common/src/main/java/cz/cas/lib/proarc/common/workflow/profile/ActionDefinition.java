/*
 * Copyright (C) 2017 Martin Rumanek
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
import javax.xml.bind.annotation.XmlElement;
import java.util.ArrayList;
import java.util.List;

/**
 * Definition command expression and arguments
 *
 * @author Martin Rumanek
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class ActionDefinition {

    @XmlAttribute(name = WorkflowProfileConsts.ACTION_COMMAND, required = true)
    private String command;

    @XmlElement(name = WorkflowProfileConsts.ACTION_COMMAND_ARG)
    private List<String> args = new ArrayList<>();

    public String getCommand() {
        return command;
    }

    public List<String> getArgs() {
        return args;
    }
}
