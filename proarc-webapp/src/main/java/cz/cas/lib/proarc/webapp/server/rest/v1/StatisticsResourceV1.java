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
package cz.cas.lib.proarc.webapp.server.rest.v1;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.SolrExportStatistics;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.server.rest.ModelStatistic;
import cz.cas.lib.proarc.webapp.server.rest.ProductivityStatistic;
import cz.cas.lib.proarc.webapp.server.rest.ProArcResponse;
import cz.cas.lib.proarc.webapp.server.rest.RestException;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.shared.rest.StatisticsResourceApi;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.DefaultValue;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_1;

/** Statistics of repository objects and historical audit events. */
@Deprecated
@Path(URL_API_VERSION_1 + "/" + StatisticsResourceApi.PATH)
public class StatisticsResourceV1 {
    private final AppConfiguration appConfig;
    private final AkubraConfiguration akubraConfiguration;
    private final HttpHeaders httpHeaders;
    private final UserProfile user;
    private final SessionContext session;

    public StatisticsResourceV1(@Context HttpHeaders httpHeaders,
            @Context HttpServletRequest httpServletRequest) throws AppConfigurationException {
        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.akubraConfiguration = Storage.AKUBRA.equals(appConfig.getTypeOfStorage())
                ? AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome()) : null;
        this.session = SessionContext.from(httpServletRequest);
        this.user = session.getUser();
    }

    /** Aggregated counts with optional filtering to the current user's organization. */
    @GET
    @Path(StatisticsResourceApi.MODELS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public ProArcResponse<ModelStatistic> getModelStatistics(
            @DefaultValue("true") @QueryParam(StatisticsResourceApi.ACTIVE_PARAM) boolean active,
            @DefaultValue("false") @QueryParam(StatisticsResourceApi.RESPECT_ORGANIZATION_PARAM) boolean respectOrganization
    ) throws IOException {
        if (!Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
        SearchView search = AkubraStorage.getInstance(akubraConfiguration).getSearch(session.getLocale(httpHeaders));
        String organization = respectOrganization ? user.getOrganization() : null;
        List<ModelStatistic> statistics = search.countObjectsByModel(organization, active).entrySet().stream()
                .map(entry -> new ModelStatistic(entry.getKey(), entry.getValue()))
                .collect(Collectors.toList());
        return new ProArcResponse<>(statistics);
    }

    @GET
    @Path(StatisticsResourceApi.PRODUCTIVITY_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public ProArcResponse<ProductivityStatistic> getProductivityStatistics(
            @QueryParam(StatisticsResourceApi.USERNAME_PARAM) List<String> usernames,
            @QueryParam(StatisticsResourceApi.DATE_FROM_PARAM) String dateFrom,
            @QueryParam(StatisticsResourceApi.DATE_TO_PARAM) String dateTo,
            @QueryParam(StatisticsResourceApi.MODEL_PARAM) List<String> models,
            @QueryParam(StatisticsResourceApi.SIGLA_PARAM) List<String> siglas,
            @DefaultValue("false") @QueryParam(StatisticsResourceApi.RESPECT_ORGANIZATION_PARAM) boolean respectOrganization
    ) throws IOException {
        LocalDate from;
        LocalDate to;
        try {
            from = LocalDate.parse(dateFrom == null ? "" : dateFrom);
            to = LocalDate.parse(dateTo == null ? "" : dateTo);
        } catch (DateTimeParseException ex) {
            throw RestException.plainBadRequest("dateFrom/dateTo", dateFrom + "/" + dateTo);
        }
        if (from.isAfter(to)) {
            throw RestException.plainBadRequest("dateFrom/dateTo", dateFrom + "/" + dateTo);
        }
        if (!Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
        String organization = respectOrganization ? user.getOrganization() : null;
        List<String> selectedUsers = usernames == null ? List.of() : usernames.stream()
                .filter(value -> value != null && !value.isBlank()).map(String::trim).distinct().collect(Collectors.toList());
        List<String> selectedModels = models == null ? List.of() : models.stream()
                .filter(value -> value != null && !value.isBlank()).map(String::trim).distinct().collect(Collectors.toList());
        List<String> selectedSiglas = siglas == null ? List.of() : siglas.stream()
                .filter(value -> value != null && !value.isBlank()).map(String::trim).distinct().collect(Collectors.toList());
        var counts = AkubraStorage.getInstance(akubraConfiguration).getLogSearch()
                .getCreatedObjectsStatistics(selectedUsers, from, to, selectedModels, selectedSiglas, organization);
        List<ModelStatistic> modelCounts = counts.getModelCounts().entrySet().stream()
                .map(entry -> new ModelStatistic(entry.getKey(), entry.getValue()))
                .collect(Collectors.toList());
        return new ProArcResponse<>(new ProductivityStatistic(selectedUsers, from.toString(), to.toString(),
                selectedModels, selectedSiglas, counts.getCount(), modelCounts));
    }

    @GET
    @Path(StatisticsResourceApi.PRODUCTIVITY_OPTIONS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public ProArcResponse<Map<String, List<String>>> getProductivityStatisticsOptions(
            @DefaultValue("false") @QueryParam(StatisticsResourceApi.RESPECT_ORGANIZATION_PARAM) boolean respectOrganization) throws IOException {
        if (!Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
        return new ProArcResponse<>(Map.of("sigla", AkubraStorage.getInstance(akubraConfiguration)
                .getLogSearch().getSiglas(respectOrganization ? user.getOrganization() : null)));
    }

    @GET
    @Path(StatisticsResourceApi.EXPORT_OPTIONS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public ProArcResponse<Map<String, List<String>>> getExportStatisticsOptions(
            @DefaultValue("false") @QueryParam(StatisticsResourceApi.ARTICLES_PARAM) boolean articles,
            @DefaultValue("false") @QueryParam(StatisticsResourceApi.RESPECT_ORGANIZATION_PARAM) boolean respectOrganization) throws IOException {
        if (!Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
        return new ProArcResponse<>(AkubraStorage.getInstance(akubraConfiguration).getExportStatistics()
                .options(respectOrganization ? user.getOrganization() : null, articles));
    }

    @GET
    @Path(StatisticsResourceApi.EXPORTS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public ProArcResponse<SolrExportStatistics.Result> getExportStatistics(
            @QueryParam(StatisticsResourceApi.USERNAME_PARAM) List<String> usernames,
            @QueryParam(StatisticsResourceApi.DATE_FROM_PARAM) String dateFrom,
            @QueryParam(StatisticsResourceApi.DATE_TO_PARAM) String dateTo,
            @QueryParam(StatisticsResourceApi.MODEL_PARAM) List<String> models,
            @QueryParam(StatisticsResourceApi.EXPORT_TYPE_PARAM) List<String> exportTypes,
            @QueryParam(StatisticsResourceApi.DEVICE_PARAM) List<String> devices,
            @QueryParam(StatisticsResourceApi.SOFTWARE_PARAM) List<String> software,
            @QueryParam(StatisticsResourceApi.SIGLA_PARAM) List<String> siglas,
            @DefaultValue("model") @QueryParam(StatisticsResourceApi.GROUP_BY_PARAM) String groupBy,
            @DefaultValue("true") @QueryParam(StatisticsResourceApi.UNIQUE_PARAM) boolean unique,
            @DefaultValue("false") @QueryParam(StatisticsResourceApi.RESPECT_ORGANIZATION_PARAM) boolean respectOrganization) throws IOException {
        if (!Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
        SolrExportStatistics.Filters filters;
        try {
            filters = new SolrExportStatistics.Filters(
                    LocalDate.parse(dateFrom == null ? "" : dateFrom),
                    LocalDate.parse(dateTo == null ? "" : dateTo), usernames, models, exportTypes, devices, software, siglas,
                    respectOrganization ? user.getOrganization() : null, groupBy, unique);
        } catch (IllegalArgumentException | DateTimeParseException ex) {
            throw RestException.plainBadRequest("exportStatistics", dateFrom + "/" + dateTo + "/" + groupBy);
        }
        return new ProArcResponse<>(AkubraStorage.getInstance(akubraConfiguration).getExportStatistics().calculate(filters));
    }

    @GET
    @Path(StatisticsResourceApi.ARTICLE_EXPORTS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public ProArcResponse<SolrExportStatistics.ArticleResult> getArticleExportStatistics(
            @QueryParam(StatisticsResourceApi.USERNAME_PARAM) List<String> usernames,
            @QueryParam(StatisticsResourceApi.DATE_FROM_PARAM) String dateFrom,
            @QueryParam(StatisticsResourceApi.DATE_TO_PARAM) String dateTo,
            @QueryParam(StatisticsResourceApi.MODEL_PARAM) List<String> models,
            @QueryParam(StatisticsResourceApi.EXPORT_TYPE_PARAM) List<String> exportTypes,
            @QueryParam(StatisticsResourceApi.SIGLA_PARAM) List<String> siglas,
            @DefaultValue("0") @QueryParam(StatisticsResourceApi.OFFSET_PARAM) int offset,
            @DefaultValue("50") @QueryParam(StatisticsResourceApi.LIMIT_PARAM) int limit,
            @DefaultValue("true") @QueryParam(StatisticsResourceApi.UNIQUE_PARAM) boolean unique,
            @DefaultValue("false") @QueryParam(StatisticsResourceApi.RESPECT_ORGANIZATION_PARAM) boolean respectOrganization) throws IOException {
        if (!Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
        SolrExportStatistics.Filters filters;
        try {
            if (offset < 0 || limit < 1 || limit > 500) throw new IllegalArgumentException("Invalid pagination");
            filters = new SolrExportStatistics.Filters(
                    LocalDate.parse(dateFrom == null ? "" : dateFrom),
                    LocalDate.parse(dateTo == null ? "" : dateTo), usernames, models, exportTypes,
                    List.of(), List.of(), siglas, respectOrganization ? user.getOrganization() : null, "model", unique);
        } catch (IllegalArgumentException | DateTimeParseException ex) {
            throw RestException.plainBadRequest("articleExportStatistics", dateFrom + "/" + dateTo);
        }
        return new ProArcResponse<>(AkubraStorage.getInstance(akubraConfiguration).getExportStatistics().articles(filters, offset, limit));
    }

}
