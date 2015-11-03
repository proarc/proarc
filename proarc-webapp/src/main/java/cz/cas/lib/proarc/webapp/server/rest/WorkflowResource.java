/*
 * Copyright (C) 2015 Jan Pokorsky
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

import cz.cas.lib.proarc.common.workflow.profile.JobDefinition;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinitionView;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfileConsts;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import cz.cas.lib.proarc.webapp.shared.rest.WorkflowResourceApi;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;

/**
 * I allows to manage workflow remotely.
 *
 * @author Jan Pokorsky
 */
@Path(WorkflowResourceApi.PATH)
public class WorkflowResource {

    private static final Logger LOG = Logger.getLogger(WorkflowResource.class.getName());

    private final SessionContext session;
    private final HttpHeaders httpHeaders;

    public WorkflowResource(
            @Context HttpHeaders httpHeaders,
            @Context HttpServletRequest httpRequest
    ) {
        this.session = SessionContext.from(httpRequest);
        this.httpHeaders = httpHeaders;
    }

    /**
     * Gets workflow profiles defined with {@link WorkflowProfiles}
     * @param name a profile name filter
     * @param disabled an availability filter
     * @return the list of profiles
     */
    @Path(WorkflowResourceApi.PROFILE_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    @GET
    public SmartGwtResponse<JobDefinitionView> getProfiles(
            @QueryParam(WorkflowProfileConsts.JOB_NAME_ATT) String name,
            @QueryParam(WorkflowProfileConsts.DISABLED) Boolean disabled
    ) {
        WorkflowProfiles wp = WorkflowProfiles.getInstance();
        WorkflowDefinition workflowDefinition = wp.getProfiles();
        if (workflowDefinition == null) {
            return SmartGwtResponse.asError("Invalid workflow.xml! Check server configuration.");
        }
        String lang = session.getLocale(httpHeaders).getLanguage();
        ArrayList<JobDefinitionView> profiles = new ArrayList<JobDefinitionView>();
        for (JobDefinition job : workflowDefinition.getJobs()) {
            if ((name == null || name.equals(job.getName()))
                    && (disabled == null || disabled == job.isDisabled())) {
                JobDefinitionView p = new JobDefinitionView();
                p.setName(job.getName());
                p.setTitle(getI18n(job.getTitles(), lang, job.getName()));
                p.setHint(getI18n(job.getHints(), lang, null));
                p.setDisabled(job.isDisabled());
                profiles.add(p);
            }
        }
        return new SmartGwtResponse<JobDefinitionView>(profiles);
    }

    private static String getI18n(Map<String, String> vals, String lang, String defaultValue) {
        String i18n = vals.get(lang);
        if (i18n == null) {
            Iterator<String> i18ns = vals.values().iterator();
            if (i18ns.hasNext()) {
                i18n = i18ns.next();
            }
        }
        return i18n != null ? i18n : defaultValue;
    }
}
