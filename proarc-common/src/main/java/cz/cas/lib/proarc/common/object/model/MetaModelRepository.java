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
import cz.cas.lib.proarc.common.object.NdkPlugin;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
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

    private static MetaModelRepository INSTANCE;

    private final Collection<MetaModel> repository;

    public static void setInstance(String[] plugins) {
        MetaModelRepository mmr = new MetaModelRepository();
        for (String pluginClass : plugins) {
            try {
                Class<?> clazz = mmr.getClass().getClassLoader().loadClass(pluginClass);
                DigitalObjectPlugin plugin = (DigitalObjectPlugin) clazz.newInstance();
                mmr.registerModels(plugin.getModel());
            } catch (Exception ex) {
                Logger.getLogger(MetaModelRepository.class.getName()).log(Level.SEVERE, pluginClass, ex);
            }
        }
        INSTANCE = mmr;
    }

    public static MetaModelRepository getInstance() {
        if (INSTANCE == null) {
            setInstance(new String[] {NdkPlugin.class.getName()});
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
        this.repository.addAll(models);
    }

}
