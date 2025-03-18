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

import cz.cas.lib.proarc.premis.PremisComplexType;
import java.util.ArrayList;

/**
 * A software referenced by a digital object.
 *
 * @author Lukas Sykora
 */
public class Software {

    private String id;
    private String label;
    private String model;

    private PremisComplexType agentDescription;
    private Long agentTimestamp;

    private PremisComplexType eventDescription;
    private Long eventTimestamp;

    private PremisComplexType objectDescription;
    private Long objectTimestamp;

    private String linkedId;
    private ArrayList<String> setOfLinkedIds;
    private Long setOfLinkedIdsTimestamp;

    public Software() {
    }

    public void create(String id, String label, String model, PremisComplexType description, String linkedId, Long timestamp) throws SoftwareException {
        this.id = id;
        this.label = label;
        this.model = model;

        if (SoftwareRepository.METAMODEL_AGENT_ID.equals(model)) {
            this.agentDescription = description;
            this.agentTimestamp = timestamp;
        } else if (SoftwareRepository.METAMODEL_EVENT_ID.equals(model)) {
            this.eventDescription = description;
            this.eventTimestamp = timestamp;
            this.linkedId = linkedId;
        } else if (SoftwareRepository.METAMODEL_OBJECT_ID.equals(model)) {
            this.objectDescription = description;
            this.objectTimestamp = timestamp;
            this.linkedId = linkedId;
        } else {
            throw new SoftwareException("Nepodporovaný model pro vytvoření single softwaru (\"" + model + "\").");
        }
    }

    public void create(String id, String label, String model, ArrayList<String> setOfLinkedIds, Long timestamp) throws SoftwareException {
        this.id = id;
        this.label = label;
        this.model = model;

        if (SoftwareRepository.METAMODEL_SET_ID.equals(model)) {
            this.setOfLinkedIds = setOfLinkedIds;
            this.setOfLinkedIdsTimestamp = timestamp;
        } else {
            throw new SoftwareException("Nepodporovaný model pro vytvoření setu objektu (\"" + model + "\").");
        }
    }

    public PremisComplexType getAgentDescription() {
        return agentDescription;
    }

    public void setAgentDescription(PremisComplexType agentDescription) {
        this.agentDescription = agentDescription;
    }

    public Long getAgentTimestamp() {
        return agentTimestamp;
    }

    public void setAgentTimestamp(Long agentTimestamp) {
        this.agentTimestamp = agentTimestamp;
    }

    public PremisComplexType getEventDescription() {
        return eventDescription;
    }

    public void setEventDescription(PremisComplexType eventDescription) {
        this.eventDescription = eventDescription;
    }

    public Long getEventTimestamp() {
        return eventTimestamp;
    }

    public void setEventTimestamp(Long eventTimestamp) {
        this.eventTimestamp = eventTimestamp;
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

    public PremisComplexType getObjectDescription() {
        return objectDescription;
    }

    public void setObjectDescription(PremisComplexType objectDescription) {
        this.objectDescription = objectDescription;
    }

    public Long getObjectTimestamp() {
        return objectTimestamp;
    }

    public void setObjectTimestamp(Long objectTimestamp) {
        this.objectTimestamp = objectTimestamp;
    }

    public ArrayList<String> getSetOfLinkedIds() {
        return setOfLinkedIds;
    }

    public void setSetOfLinkedIds(ArrayList<String> setOfLinkedIds) {
        this.setOfLinkedIds = setOfLinkedIds;
    }

    public Long getSetOfLinkedIdsTimestamp() {
        return setOfLinkedIdsTimestamp;
    }

    public void setSetOfLinkedIdsTimestamp(Long setOfLinkedIdsTimestamp) {
        this.setOfLinkedIdsTimestamp = setOfLinkedIdsTimestamp;
    }

    public String getLinkedId() {
        return linkedId;
    }

    public void setLinkedId(String linkedId) {
        this.linkedId = linkedId;
    }

    @Override
    public String toString() {
        return "Software{" + "id=" + id + ", label=" + label + ", model=" + model + ", linkedId=" + linkedId +
                ", setOfLinkedIds="+ setOfLinkedIds.toString() + ", setOfLinkedIdsTimestamp=" + setOfLinkedIdsTimestamp +
                ", agentDesctiption=" + agentDescription + ", agentTimestamp=" + agentTimestamp +
                ", eventDescription=" + eventDescription + ", eventTimestamp=" + eventTimestamp +
                ", objectDescription=" + objectDescription + ", objectTimestamp=" + objectTimestamp +
                '}';
    }

    public long getTimestamp() throws SoftwareException {
        if (model == null) {
            throw new SoftwareException("Objekt nemá přiřazený model.");
        } else if (SoftwareRepository.METAMODEL_AGENT_ID.equals(model)) {
            return agentTimestamp;
        } else if (SoftwareRepository.METAMODEL_EVENT_ID.equals(model)) {
            return eventTimestamp;
        } else if (SoftwareRepository.METAMODEL_OBJECT_ID.equals(model)) {
            return objectTimestamp;
        } else if (SoftwareRepository.METAMODEL_SET_ID.equals(model)) {
            return setOfLinkedIdsTimestamp;
        } else {
            throw new SoftwareException("Nepodporovaný model pro zapsani timestampu softwaru (\"" + model + "\").");
        }
    }

    public void setTimestamp(long timestamp) throws SoftwareException {
        if (model == null) {
            throw new SoftwareException("Objekt nemá přiřazený model.");
        } else if (SoftwareRepository.METAMODEL_AGENT_ID.equals(model)) {
            this.agentTimestamp = timestamp;
        } else if (SoftwareRepository.METAMODEL_EVENT_ID.equals(model)) {
            this.eventTimestamp = timestamp;
        } else if (SoftwareRepository.METAMODEL_OBJECT_ID.equals(model)) {
            this.objectTimestamp = timestamp;
        } else if (SoftwareRepository.METAMODEL_SET_ID.equals(model)) {
            this.setOfLinkedIdsTimestamp = timestamp;
        } else {
            throw new SoftwareException("Nepodporovaný model pro zapsani timestampu softwaru (\"" + model + "\").");
        }
    }

    public PremisComplexType getDescription() throws SoftwareException {
        if (model == null) {
            throw new SoftwareException("Objekt nemá přiřazený model.");
        } else if (SoftwareRepository.METAMODEL_AGENT_ID.equals(model)) {
            return agentDescription;
        } else if (SoftwareRepository.METAMODEL_EVENT_ID.equals(model)) {
            return eventDescription;
        } else if (SoftwareRepository.METAMODEL_OBJECT_ID.equals(model)) {
            return objectDescription;
        } else if (SoftwareRepository.METAMODEL_SET_ID.equals(model)) {
            return null;
        } else {
            throw new SoftwareException("Nepodporovaný model pro zapsani timestampu softwaru (\"" + model + "\").");
        }
    }

    public void setDescription(PremisComplexType description) throws SoftwareException {
        if (model == null) {
            throw new SoftwareException("Objekt nemá přiřazený model.");
        } else if (SoftwareRepository.METAMODEL_AGENT_ID.equals(model)) {
            this.agentDescription = description;
        } else if (SoftwareRepository.METAMODEL_EVENT_ID.equals(model)) {
            this.eventDescription = description;
        } else if (SoftwareRepository.METAMODEL_OBJECT_ID.equals(model)) {
            this.objectDescription = description;
        } else if (SoftwareRepository.METAMODEL_SET_ID.equals(model)) {
            this.setOfLinkedIds = new ArrayList<>();
        } else {
            throw new SoftwareException("Nepodporovaný model pro zapsani timestampu softwaru (\"" + model + "\").");
        }
    }
}
