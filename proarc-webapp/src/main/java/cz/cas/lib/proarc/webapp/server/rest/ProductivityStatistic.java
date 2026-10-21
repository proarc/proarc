/*
 * Copyright (C) 2026 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.server.rest;

import java.util.List;

/** Created object count and the filters used to calculate it. */
public final class ProductivityStatistic {
    private final List<String> usernames;
    private final String dateFrom;
    private final String dateTo;
    private final List<String> models;
    private final List<String> siglas;
    private final long count;
    private final List<ModelStatistic> modelCounts;

    public ProductivityStatistic(List<String> usernames, String dateFrom, String dateTo, List<String> models, long count) {
        this(usernames, dateFrom, dateTo, models, count, List.of());
    }

    public ProductivityStatistic(List<String> usernames, String dateFrom, String dateTo, List<String> models,
            long count, List<ModelStatistic> modelCounts) {
        this(usernames, dateFrom, dateTo, models, List.of(), count, modelCounts);
    }

    public ProductivityStatistic(List<String> usernames, String dateFrom, String dateTo, List<String> models,
            List<String> siglas, long count, List<ModelStatistic> modelCounts) {
        this.usernames = usernames == null ? List.of() : List.copyOf(usernames);
        this.dateFrom = dateFrom;
        this.dateTo = dateTo;
        this.models = models == null ? List.of() : List.copyOf(models);
        this.siglas = siglas == null ? List.of() : List.copyOf(siglas);
        this.count = count;
        this.modelCounts = List.copyOf(modelCounts);
    }

    public List<String> getUsernames() { return usernames; }
    public String getDateFrom() { return dateFrom; }
    public String getDateTo() { return dateTo; }
    public List<String> getModels() { return models; }
    public List<String> getSiglas() { return siglas; }
    public long getCount() { return count; }
    public List<ModelStatistic> getModelCounts() { return modelCounts; }
}
