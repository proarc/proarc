/*
 * Copyright (C) 2025 Lukas Sykora
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
package cz.cas.lib.proarc.common.software;

import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.mets.Mets;
import java.util.ArrayList;
import java.util.List;

/**
 * A software referenced by a digital object.
 *
 * @author Lukas Sykora
 */
public class Software {

    private String id;
    private String label;
    private String model;
    private Mets description;
    private String descriptionXml;
    private Long timestamp;
    private List<String> setOfLinkedIds;

    public Software() {
    }

    public void create(String id, String label, String model, Mets description, List<String> setOfLinkedIds, Long timestamp) throws SoftwareException {
        this.id = id;
        this.label = label;
        this.model = model;
        this.setOfLinkedIds = new ArrayList<>();
        this.description = description;
        this.timestamp = timestamp;
        this.setOfLinkedIds = setOfLinkedIds;

        if (description != null) {
            descriptionXml = MetsUtils.toXml(description, true);
        } else {
            descriptionXml = null;
        }
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getModel() {
        return model;
    }

    public void setModel(String model) {
        this.model = model;
    }

    public Mets getDescription() {
        return description;
    }

    public String getDescriptionAsXml() {
         return descriptionXml;
    }

    public void setDescription(Mets description) {
        this.description = description;
        if (description != null) {
            this.descriptionXml = MetsUtils.toXml(description, false);
        } else {
            this.descriptionXml = null;
        }
    }

    public List<String> getSetOfLinkedIds() {
        return setOfLinkedIds;
    }

    public void setSetOfLinkedIds(List<String> setOfLinkedIds) {
        this.setOfLinkedIds = setOfLinkedIds;
    }

    public Long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Long timestamp) {
        this.timestamp = timestamp;
    }

    @Override
    public String toString() {
        return "Software{" + "id=" + id + ", label=" + label + ", model=" + model +
                ", setOfLinkedIds="+ setOfLinkedIds != null ? setOfLinkedIds.toString() : "" +
                ", description=" + description + ", timestamp=" + timestamp +
                '}';
    }
}
