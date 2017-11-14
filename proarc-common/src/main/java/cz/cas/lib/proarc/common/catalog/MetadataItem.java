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
package cz.cas.lib.proarc.common.catalog;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * Describes metadata item fetched from catalog.
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class MetadataItem {

    private int id;
    /** MODS XML */
    private String mods;
    /** displayable mods; HTML is permitted */
    private String preview;
    /** short descriptor of the item; used in ListGrid */
    private String title;
    /** optional id from rd */
    private Long rdczId;

    public MetadataItem(int id,  Long rdczId, String mods, String preview, String title) {
        this.id = id;
        this.mods = mods;
        this.preview = preview;
        this.title = title;
        this.rdczId = rdczId;
    }

    public MetadataItem(int id, String mods, String preview, String title) {
        this.id = id;
        this.mods = mods;
        this.preview = preview;
        this.title = title;
    }

    public int getId() {
        return id;
    }

    public String getMods() {
        return mods;
    }

    public String getPreview() {
        return preview;
    }

    public String getTitle() {
        return title;
    }

    public Long getRdczId() {
        return rdczId;
    }
}
