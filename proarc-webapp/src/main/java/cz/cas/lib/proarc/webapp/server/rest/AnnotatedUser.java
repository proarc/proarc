/*
 * Copyright (C) 2014 Jan Pokorsky
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

import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.shared.rest.UserResourceApi;
import java.net.URI;
import java.sql.Timestamp;
import java.util.Date;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;

/**
 * Helper class to annotate {@link UserProfile} properties.
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AnnotatedUser extends UserProfile {

    @XmlElement(name = UserResourceApi.USER_ID)
    @Override
    public abstract Integer getId();

    @XmlElement(name = UserResourceApi.USER_HOME)
    @Override
    public abstract String getUserHome();

    @XmlElement(name = UserResourceApi.USER_NAME)
    @Override
    public abstract String getUserName();

    @XmlElement(name = UserResourceApi.USER_REMOTENAME)
    @Override
    public abstract String getRemoteName();

    @XmlElement(name = UserResourceApi.USER_REMOTETYPE)
    @Override
    public abstract String getRemoteType();

    @XmlElement(name = UserResourceApi.USER_CREATED)
    @Override
    public abstract Date getCreated();

    @XmlElement(name = UserResourceApi.USER_EMAIL)
    @Override
    public abstract String getEmail();

    @XmlElement(name = UserResourceApi.USER_FORENAME)
    @Override
    public abstract String getForename();

    @XmlElement(name = UserResourceApi.USER_ORGANIZATION)
    @Override
    public abstract String getOrganization();

    @XmlElement(name = UserResourceApi.USER_ROLE)
    @Override
    public abstract String getRole();

    @XmlElement(name = UserResourceApi.USER_SURNAME)
    @Override
    public abstract String getSurname();

    @XmlElement(name = UserResourceApi.USER_LASTLOGIN)
    @Override
    public abstract Date getLastLogin();

    @XmlElement(name = UserResourceApi.USER_STATUS)
    @Override
    public abstract String getStatus();

    @XmlElement(name = UserResourceApi.USER_DEFAULTGROUP)
    @Override
    public abstract Integer getDefaultGroup();

    @XmlElement(name = UserResourceApi.USER_USERGROUP)
    @Override
    public abstract Integer getUserGroup();

    @XmlElement(name = UserResourceApi.USER_TIMESTAMP)
    @Override
    public abstract Timestamp getTimestamp();

    @XmlElement(name = UserResourceApi.USER_RUN_CHANGE_MODEL_FUNCTION)
    @Override
    public abstract Boolean getChangeModelFunction();

    @XmlElement(name = UserResourceApi.USER_RUN_UPDATE_MODEL_FUNCTION)
    @Override
    public abstract Boolean getUpdateModelFunction();

    @XmlElement(name = UserResourceApi.USER_RUN_LOCK_OBJECT_FUNCTION)
    @Override
    public abstract Boolean getLockObjectFunction();

    @XmlElement(name = UserResourceApi.USER_RUN_UNLOCK_OBJECT_FUNCTION)
    @Override
    public abstract Boolean getUnlockObjectFunction();

    @XmlElement(name = UserResourceApi.USER_IMPORT_TO_PROD_FUNCTION)
    @Override
    public abstract Boolean getImportToProdFunction();

    @XmlElement(name = UserResourceApi.USER_CZIDLO_FUNCTION)
    @Override
    public abstract Boolean getCzidloFunction();

    @XmlElement(name = UserResourceApi.USER_WF_DELETE_JOB_FUNCTION)
    @Override
    public abstract Boolean getWfDeleteJobFunction();

    @XmlElement(name = UserResourceApi.USER_IMPORT_TO_CATALOG_FUNCTION)
    @Override
    public abstract Boolean getImportToCatalogFunction();

    @XmlElement(name = UserResourceApi.CHANGE_OBJECTS_OWNER_FUNCTION)
    @Override
    public abstract Boolean getChangeObjectsOwnerFunction();

    @XmlElement(name = UserResourceApi.CHANGE_PAGES_FUNCTION)
    @Override
    public abstract Boolean getChangePagesFunction();

    @XmlElement(name = UserResourceApi.DEVICE_FUNCTION)
    @Override
    public abstract Boolean getDeviceFunction();

    @XmlElement(name = UserResourceApi.WF_CREATE_JOB_FUNCTION)
    @Override
    public abstract Boolean getWfCreateJobFunction();

    @XmlElement(name = UserResourceApi.CREATE_USER_FUNCTION)
    @Override
    public abstract Boolean getCreateUserFunction();

    @XmlElement(name = UserResourceApi.UPDATE_USER_FUNCTION)
    @Override
    public abstract Boolean getUpdateUserFunction();

    @XmlElement(name = UserResourceApi.UPDATE_USER_PERMISSION_FUNCTION)
    @Override
    public abstract Boolean getUpdateUserPermissionFunction();

    @XmlElement(name = UserResourceApi.DELETE_USER_FUNCTION)
    @Override
    public abstract Boolean getDeleteUserFunction();

    @XmlElement(name = UserResourceApi.SOLR_FUNCTION)
    @Override
    public abstract Boolean getSolrFunction();

    @XmlElement(name = UserResourceApi.DELETE_ACTION_FUNCTION)
    @Override
    public abstract Boolean getDeleteActionFunction();

    @XmlElement(name = UserResourceApi.ALL_OBJECTS_FUNCTION)
    @Override
    public abstract Boolean getAllObjectsFunction();

    @XmlElement(name = UserResourceApi.PREPARE_BATCH_FUNCTION)
    @Override
    public abstract Boolean getPrepareBatchFunction();

    @XmlElement(name = UserResourceApi.SYS_ADMIN_FUNCTION)
    @Override
    public abstract Boolean getSysAdminFunction();

    @XmlTransient
    @Override
    public abstract URI getImportFolder();

    @XmlTransient
    @Override
    public abstract URI getExportFolder();

    @XmlTransient
    @Override
    public abstract String getUserNameAsPid();

    @XmlTransient
    @Override
    public abstract String getUserPassword();

    @XmlTransient
    @Override
    public abstract String getUserPasswordDigest();



}
