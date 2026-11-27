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
package cz.cas.lib.proarc.common.externalApp.ltp;

import com.fasterxml.jackson.annotation.JsonIgnore;
import cz.cas.lib.proarc.common.storage.relation.Relations;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/** Information about the latest archive submission. No credentials belong here. */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(namespace = Relations.PROARC_RELS_NS)
public class ArchivInfo {
    @JsonIgnore
    @XmlAttribute(name = "parseType", namespace = Relations.RDF_NS)
    private String parseType = "Resource";
    public enum State { REGISTERING, UPLOADING, UPLOADED, STORED, FAILED }
    public enum CatalogState { PENDING, DONE }

    private String provider;
    private String endpoint;
    private String group;
    private String operationId;
    private String packageId;
    private String previousPackageId;
    private Integer version;
    private State state;
    private String integrityStatus;
    private String uploadStatus;
    private String submittedAt;
    private String storedAt;
    private String catalogId;
    private Integer credentialOwnerId;
    private CatalogState catalogState = CatalogState.PENDING;

    public String getProvider() { return provider; }
    public void setProvider(String value) { provider = value; }
    public String getEndpoint() { return endpoint; }
    public void setEndpoint(String value) { endpoint = value; }
    public String getGroup() { return group; }
    public void setGroup(String value) { group = value; }
    public String getOperationId() { return operationId; }
    public void setOperationId(String value) { operationId = value; }
    public String getPackageId() { return packageId; }
    public void setPackageId(String value) { packageId = value; }
    public String getPreviousPackageId() { return previousPackageId; }
    public void setPreviousPackageId(String value) { previousPackageId = value; }
    public Integer getVersion() { return version; }
    public void setVersion(Integer value) { version = value; }
    public State getState() { return state; }
    public void setState(State value) { state = value; }
    public String getIntegrityStatus() { return integrityStatus; }
    public void setIntegrityStatus(String value) { integrityStatus = value; }
    public String getUploadStatus() { return uploadStatus; }
    public void setUploadStatus(String value) { uploadStatus = value; }
    public String getSubmittedAt() { return submittedAt; }
    public void setSubmittedAt(String value) { submittedAt = value; }
    public String getStoredAt() { return storedAt; }
    public void setStoredAt(String value) { storedAt = value; }
    public String getCatalogId() { return catalogId; }
    public void setCatalogId(String value) { catalogId = value; }
    public Integer getCredentialOwnerId() { return credentialOwnerId; }
    public void setCredentialOwnerId(Integer value) { credentialOwnerId = value; }
    public CatalogState getCatalogState() { return catalogState; }
    public void setCatalogState(CatalogState value) { catalogState = value; }
}
