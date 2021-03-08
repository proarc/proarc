/*
 * Copyright (C) 2021 Lukas Sykora
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
package cz.cas.lib.proarc.common.export.workflow;


import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;

/**
 * Get information from Workflow
 *
 * @author Lukáš Sýkora
 */

@XmlRootElement(name = "workflow")
@XmlAccessorType(value = XmlAccessType.FIELD)
public class WorkflowExportFile {

    @XmlElement(name = "job")
    private List<WorkflowJobExport> workflowJobExports;

    public List<WorkflowJobExport> getWorkflowJobExports() {
        if (workflowJobExports == null) {
            workflowJobExports = new ArrayList<>();
        }
        return workflowJobExports;
    }

    @XmlAccessorType(value = XmlAccessType.FIELD)
    public static class WorkflowJobExport {


        @XmlElement(name = "profile")
        private String profile;

        @XmlElement(name = "label")
        private String label;

        @XmlElement(name = "task")
        private List<WorkflowTaskExport> workflowTasks;

        public List<WorkflowTaskExport> getWorkflowTasks() {
            if (workflowTasks == null) {
                workflowTasks = new ArrayList<>();
            }
            return workflowTasks;
        }

        public String getProfile() {
            return profile;
        }

        public void setProfile(String profile) {
            this.profile = profile;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(String label) {
            this.label = label;
        }
    }

    @XmlAccessorType(value = XmlAccessType.FIELD)
    public static class WorkflowTaskExport {

        @XmlSchemaType(name = "taskId")
        private String taskId;

        @XmlSchemaType(name = "taskName")
        private String taskName;

        @XmlSchemaType(name = "user")
        private String user;

        @XmlSchemaType(name = "time")
        private String time;

        @XmlSchemaType(name = "state")
        private String state;

        public String getTaskId() {
            return taskId;
        }

        public void setTaskId(String taskId) {
            this.taskId = taskId;
        }

        public String getTaskName() {
            return taskName;
        }

        public void setTaskName(String taskName) {
            this.taskName = taskName;
        }

        public String getUser() {
            return user;
        }

        public void setUser(String user) {
            this.user = user;
        }

        public String getTime() {
            return time;
        }

        public void setTime(Timestamp time) {
            this.time = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss").format(time);
        }

        public String getState() {
            return state;
        }

        public void setState(String state) {
            this.state = state;
        }
    }
}
