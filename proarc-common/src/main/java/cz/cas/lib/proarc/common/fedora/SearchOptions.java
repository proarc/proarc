/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.common.fedora;

import org.apache.commons.configuration.Configuration;

;

/**
 * Settings for Search options
 *
 * @author lsykora
 */
public class SearchOptions {
    static final String PROP_SEARCH_FILTER_PROCESSOR = "search.filter.processor";
    private Boolean searchFilterProcessor;

    public static SearchOptions getOptions(Configuration config) {
        SearchOptions options = new SearchOptions();

        String searchFilterProcessor = config.getString(PROP_SEARCH_FILTER_PROCESSOR);
        if (searchFilterProcessor != null
                && !searchFilterProcessor.isEmpty()) {
            options.setSearchFilterProcessor("true".equals(searchFilterProcessor));
        } else {
            options.setSearchFilterProcessor(false);
        }
        return options;
    }

    public Boolean getSearchFilterProcessor() {
        return searchFilterProcessor;
    }

    public void setSearchFilterProcessor(boolean value) {
        this.searchFilterProcessor = value;
    }
}
