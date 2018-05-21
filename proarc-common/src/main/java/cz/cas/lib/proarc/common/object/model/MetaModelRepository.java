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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.ServiceLoader;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The repository of models of digital objects.
 *
 * <p>For now model metadata are hardcoded.
 *
 * @author Jan Pokorsky
 */
public final class MetaModelRepository {

    private static final Logger LOG = Logger.getLogger(MetaModelRepository.class.getName());
    private static MetaModelRepository INSTANCE;

    private final Collection<MetaModel> repository;

    /**
     * Creates the repository for digital object plugins.
     * @param pluginIds
     */
    public static void setInstance(String[] pluginIds) {
        MetaModelRepository mmr = new MetaModelRepository();
        ServiceLoader<DigitalObjectPlugin> pluginLoader = ServiceLoader.load(DigitalObjectPlugin.class);
        HashMap<String, DigitalObjectPlugin> pluginMap = new HashMap<String, DigitalObjectPlugin>();
        for (DigitalObjectPlugin plugin : pluginLoader) {
            LOG.log(Level.FINE, "ID: {0}, class: {1}", new Object[]{plugin.getId(), plugin.getClass()});
            DigitalObjectPlugin duplicate = pluginMap.put(plugin.getId(), plugin);
            if (duplicate != null) {
                LOG.warning(String.format("Duplicate plugin ID: %s, %s, %s", plugin.getId(), duplicate.getClass(), plugin.getClass()));
            }
        }
        for (String pluginId : pluginIds) {
            DigitalObjectPlugin plugin = pluginMap.get(pluginId);
            if (plugin == null) {
                LOG.warning("Nonexxisting plugin {} " + pluginId);
            } else {
                mmr.registerModels(plugin.getModel());
            }
        }
        INSTANCE = mmr;
    }

    public static MetaModelRepository getInstance() {
        if (INSTANCE == null) {
            throw new IllegalStateException("set instance first!");
        }
        return INSTANCE;
    }


    private MetaModelRepository() {
        repository = new ArrayList<MetaModel>();
    }

    public Collection<MetaModel> find() {
        return Collections.unmodifiableCollection(repository);
    }

    public MetaModel find(String model) {
        for (MetaModel metaModel : find()) {
            if (metaModel.getPid().equals(model)) {
                return metaModel;
            }
        }
        return null;
    }

    void registerModels(Collection<MetaModel> models) {
        for (MetaModel model : models) {
            MetaModel exists = find(model.getPid());
            if (exists != null) {
                if (exists.getPriority() < model.getPriority()) {
                    LOG.log(Level.WARNING, "Model ID duplicates! {0}, {2} ovverides {1} plugin.",
                            new Object[]{model.getPid(), exists.getPlugin().getId(), model.getPlugin().getId()});
                    this.repository.remove(exists);
                } else {
                    LOG.log(Level.WARNING, "Model ID duplicates! {0}, {1} overrides {2} plugin.",
                            new Object[]{model.getPid(), exists.getPlugin().getId(), model.getPlugin().getId()});
                    continue;
                }
            }
            this.repository.add(model);
        }
    }

}
