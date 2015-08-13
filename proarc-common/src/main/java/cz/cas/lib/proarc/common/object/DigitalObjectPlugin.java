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
package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.object.model.MetaModel;
import java.util.Collection;
import java.util.List;

/**
 * Allows to add specific behavior of digital objects.
 *
 * @author Jan Pokorsky
 */
public interface DigitalObjectPlugin {

    /**
     * Gets unique ID of the plugin.
     * @return ID
     */
    String getId();

    /**
     * Gets supported models.
     * @return models
     */
    Collection<MetaModel> getModel();

    /**
     * Gets a provider to handle contents of supported digital objects.
     * @param <T> handler type
     * @param type handler type class
     * @return the provider or {@code null}
     */
    <T extends HasDataHandler> T getHandlerProvider(Class<T> type);

    /**
     * Gets value maps specific to provided models.
     * @return the list of value maps
     */
    List<ValueMap> getValueMaps(ValueMap.Context context);

}
