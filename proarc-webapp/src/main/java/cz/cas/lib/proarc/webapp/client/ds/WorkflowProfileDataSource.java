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
package cz.cas.lib.proarc.webapp.client.ds;

import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.events.DataArrivedEvent;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FetchMode;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfileConsts;
import java.util.function.Consumer;

/**
 * The data source of workflow profiles.
 *
 * @author Jan Pokorsky
 */
public class WorkflowProfileDataSource extends RestDataSource {

    public static final String ID = "WorkflowProfileDataSource";

    public static final String FIELD_ID = WorkflowProfileConsts.NAME;
    public static final String FIELD_LABEL = WorkflowProfileConsts.TITLE_EL;
    public static final String FIELD_HINT = WorkflowProfileConsts.HINT_EL;
    public static final String FIELD_DISABLED = WorkflowProfileConsts.DISABLED;

    private static WorkflowProfileDataSource INSTANCE = null;

    public static WorkflowProfileDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new WorkflowProfileDataSource();
        }
        return INSTANCE;
    }

    private ResultSet profiles;

    public WorkflowProfileDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_WORKFLOW_PROFILE);

        DataSourceTextField fieldId = new DataSourceTextField(FIELD_ID);
        fieldId.setPrimaryKey(Boolean.TRUE);

        DataSourceTextField label = new DataSourceTextField(FIELD_LABEL);
        DataSourceTextField hint = new DataSourceTextField(FIELD_HINT);
        DataSourceBooleanField disabled = new DataSourceBooleanField(FIELD_DISABLED);

        setFields(fieldId, label, hint, disabled);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public void getSubjobs(boolean reload, String jobName, Consumer<Record[]> consumer) {
        getJob(reload, jobName, (profile) -> {
            consumer.accept(profile == null ? null : profile.getAttributeAsRecordArray(WorkflowProfileConsts.JOBVIEW_SUBJOB));
        });
    }

    public void getTasks(boolean reload, String jobName, Consumer<Record[]> consumer) {
        getJob(reload, jobName, (profile) -> {
            consumer.accept(profile == null ? null : profile.getAttributeAsRecordArray(WorkflowProfileConsts.JOBVIEW_TASK));
        });
    }

    public void getJob(boolean reload, String jobName, Consumer<Record> consumer) {
        getJobs(reload, (rs) -> {
            Record jobProfile = rs.find(WorkflowProfileConsts.NAME, jobName);
            consumer.accept(jobProfile);
        });
    }

    public void getJobs(boolean reload, Consumer<ResultSet> consumer) {
        ResultSet rs = getProfileResultSet(reload);
        if (rs.lengthIsKnown()) {
            consumer.accept(rs);
        } else {
            final HandlerRegistration[] handler = new HandlerRegistration[1];
            handler[0] = rs.addDataArrivedHandler((DataArrivedEvent event) -> {
                handler[0].removeHandler();
                consumer.accept(rs);
            });
        }
    }

    public ResultSet getProfileResultSet(boolean reload) {
        if (profiles == null) {
            profiles = new ResultSet(this);
            profiles.setFetchMode(FetchMode.LOCAL);
            profiles.get(0);
        } else if (reload) {
            profiles.invalidateCache();
            profiles.get(0);
        }
        return profiles;
    }
}
