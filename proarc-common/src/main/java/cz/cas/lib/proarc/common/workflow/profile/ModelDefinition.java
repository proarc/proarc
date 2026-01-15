/*
 * Copyright (C) 2017 Martin Rumanek
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

import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import java.util.Optional;

/**
 *
 * @author Martin Rumanek
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class ModelDefinition implements IDisplayableType<ModelDefinition> {

    @XmlAttribute(name = WorkflowProfileConsts.MODEL_PID)
    private String pid;

    public String getPid() {
        return pid;
    }

    public void setPid(String pid) {
        this.pid = pid;
    }


    @Override
    public String getName() {
        return pid;
    }

    @Override
    public boolean isDisabled() {
        return false;
    }

    @Override
    public String getTitle(String lang, String defaultValue) {
        Optional<String> title = MetaModelRepository.getInstance().find().stream()
                .filter(metaModel -> metaModel.getPid().equals(pid))
                .map(metaModel -> metaModel.getDisplayName(lang)).findFirst();
        if (title.isPresent()) {
            return title.get();
        } else {
            return defaultValue;
        }
    }

    @Override
    public String getHint(String lang, String defaultValue) {
        return null;
    }
}
