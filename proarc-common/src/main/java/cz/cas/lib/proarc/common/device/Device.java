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
package cz.cas.lib.proarc.common.device;

import cz.cas.lib.proarc.audiopremis.AudioPremisUtils;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mix.Mix;
import javax.xml.bind.annotation.XmlElementWrapper;

/**
 * A device referenced by a digital object.
 *
 * @author Jan Pokorsky
 */
public class Device {

    private String id;
    private String label;
    private String model;
    private Mix description;
    private Mets audioDescription;
    private Long audioTimestamp;
    private Long timestamp;

    public Mets getAudioDescription() {
        return audioDescription;
    }

    public void setAudioDescription(Mets audioDescription) {
        this.audioDescription = audioDescription;
    }


    public void create(String audioDescription) throws DeviceException{
        try {
            AudioPremisUtils audioPremisUtils = new AudioPremisUtils(id, label, model, audioDescription);
            audioPremisUtils.createAudioDescription(audioDescription);
            this.audioDescription = audioPremisUtils.getMets();
        } catch (Exception e) {
            throw new DeviceException("Error while generating agent node in premis data", e);
        }

    }


    public Long getAudioTimestamp() {
        return audioTimestamp;
    }

    public void setAudioTimestamp(Long audioTimestamp) {
        this.audioTimestamp = audioTimestamp;
    }


    public Device() {
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

    public Mix getDescription() {
        return description;
    }

    public void setDescription(Mix description) {
        this.description = description;
    }

    public Long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Long timestamp) {
        this.timestamp = timestamp;
    }

    public String getModel() {
        return model;
    }

    public void setModel(String model) {
        this.model = model;
    }

    @Override
    public String toString() {
        return "Device{" + "id=" + id + ", label=" + label
                 + ", timestamp=" + timestamp+ ", description=" + description + ", audiodescription=" + audioDescription + '}';
    }


}
