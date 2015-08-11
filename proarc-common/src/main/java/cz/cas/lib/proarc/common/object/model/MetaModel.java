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
package cz.cas.lib.proarc.common.object.model;

import cz.cas.lib.proarc.common.object.DigitalObjectPlugin;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import java.util.Collection;
import java.util.EnumSet;
import java.util.Set;

/**
 * The model of a digital object.
 *
 * @author Jan Pokorsky
 */
public class MetaModel {

    private String pid;
    private Boolean root;
    private Boolean leaf;
    private Collection<ElementType> displayNames;
    private String metadataFormat;
    private String editorId;
    private DigitalObjectPlugin plugin;
    private EnumSet<DatastreamEditorType> dataStreamEditors;
    private double priority = 1.0;

    public MetaModel() {
    }

    public MetaModel(String pid, Boolean root, Boolean leaf,
            Collection<ElementType> displayNames,
            String metadataFormat,
            String editorId,
            DigitalObjectPlugin plugin,
            EnumSet<DatastreamEditorType> dataStreamEditors) {

        this.pid = pid;
        this.root = root;
        this.leaf = leaf;
        this.displayNames = displayNames;
        this.metadataFormat = metadataFormat;
        this.editorId = editorId;
        this.plugin = plugin;
        this.dataStreamEditors = dataStreamEditors;
    }

    public String getDisplayName(String lang) {
        for (ElementType displayName : displayNames) {
            if (lang == null ? displayName.getLang() == null : lang.equals(displayName.getLang())) {
                return displayName.getValue();
            }
        }
        return displayNames.isEmpty() ? "?" : displayNames.iterator().next().getValue();
    }

    public Boolean isLeaf() {
        return leaf;
    }

    public String getPid() {
        return pid;
    }

    public Boolean isRoot() {
        return root;
    }

    public String getMetadataFormat() {
        return metadataFormat;
    }

    // XXX rename to getMetadataEditor
    public String getModsCustomEditor() {
        return editorId;
    }

    public Set<DatastreamEditorType> getDataStreamEditors() {
        return dataStreamEditors;
    }

    public DigitalObjectPlugin getPlugin() {
        return plugin;
    }

    public double getPriority() {
        return priority;
    }

    /**
     * The priority of the model to solve model ID collisions. The higher priority
     * wins. Default priority is 1.
     */
    public MetaModel setPriority(double priority) {
        this.priority = priority;
        return this;
    }

}
