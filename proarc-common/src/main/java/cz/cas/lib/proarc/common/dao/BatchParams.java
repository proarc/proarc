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
package cz.cas.lib.proarc.common.dao;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import java.util.List;

@XmlRootElement(name = "params")
@XmlAccessorType(XmlAccessType.PROPERTY)
public class BatchParams {

    private List<String> pids;
    private List<String> dsIds;
    private String policy;
    private Boolean hierarchy;
    private String krameriusInstanceId;
    private String krameriusImportInstanceId;
    private Boolean forDownload;
    private Boolean dryRun;
    private String typeOfPackage;
    private Boolean ignoreMissingUrnNbn;
    private Boolean bagit;
    private Boolean ltpCesnet;
    private String ltpCesnetToken;
    private String noTifAvailableMessage;
    private String additionalInfoMessage;
    private String license;
    private List<String> collections;
    private Boolean updateMods;
    private Boolean updateCatalog;
    private String catalogId;
    private Boolean extendedArchivePackage;
    private Boolean deleteRawScans;
    private Boolean purge;
    private Boolean restore;
    private String type;
    private Integer peroOcrEngine;
    private String metakatEngine;
    private String notBefore;
    private String notAfter;
    private Boolean generatePageType;
    private Integer sourceBatchId;
    private Integer nextBatchId;
    private List<String> bagitArchives;
    private List<String> completedPackageSources;
    private Boolean bagitComplete;
    private String bagitSourceFolder;
    private List<String> completedUploads;
    private Boolean confirmCesnetUpload;
    private String cesnetOperationId;
    private Integer cesnetCredentialOwnerId;
    private List<String> rawScanFolders;
    private String importFolder;

    /** Canonical source directory for BagIt/CESNET jobs without a repository object. */
    @XmlElement(name = "importFolder")
    public String getImportFolder() {
        return importFolder;
    }

    public void setImportFolder(String importFolder) {
        this.importFolder = importFolder;
    }

    public BatchParams() {
    }

    public BatchParams(List<String> pids) {
        this.pids = pids;
    }

    public BatchParams(List<String> pids, String policy, Boolean hierarchy, String krameriusInstanceId, Boolean bagit, String license) {
        this.pids = pids;
        this.policy = policy;
        this.hierarchy = hierarchy;
        this.krameriusInstanceId = krameriusInstanceId;
        this.bagit = bagit;
        this.license = license;
    }

    public BatchParams(List<String> pids, String krameriusInstanceId, String krameriusImportInstanceId) {
        this.pids = pids;
        this.krameriusInstanceId = krameriusInstanceId;
        this.krameriusImportInstanceId = krameriusImportInstanceId;
    }

    public BatchParams(List<String> pids, String typeOfPackage, Boolean ignoreMissingUrnNbn, Boolean bagit, String noTifAvailableMessage, String additionalInfoMessage, Boolean extendedArchivePackage, Boolean ltpCesnet) {
        this.pids = pids;
        this.typeOfPackage = typeOfPackage;
        this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
        this.bagit = bagit;
        this.noTifAvailableMessage = noTifAvailableMessage;
        this.additionalInfoMessage = additionalInfoMessage;
        this.extendedArchivePackage = extendedArchivePackage;
        this.ltpCesnet = ltpCesnet;
    }

    public BatchParams(List<String> pids, String typeOfPackage, Boolean ignoreMissingUrnNbn, Boolean bagit, Boolean ltpCesnet, String krameriusInstanceId, String policy, String license) {
        this.pids = pids;
        this.typeOfPackage = typeOfPackage;
        this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
        this.bagit = bagit;
        this.ltpCesnet = ltpCesnet;
        this.krameriusInstanceId = krameriusInstanceId;
        this.policy = policy;
        this.license = license;
    }

    public BatchParams(List<String> pids, String krameriusInstanceId) {
        this.pids = pids;
        this.krameriusInstanceId = krameriusInstanceId;
    }

    public BatchParams(List<String> pids, Boolean hierarchy, Boolean forDownload, Boolean dryRun) {
        this.pids = pids;
        this.hierarchy = hierarchy;
        this.forDownload = forDownload;
        this.dryRun = dryRun;
    }

    public BatchParams(List<String> pids, Boolean hierarchy, List<String> dsIds) {
        this.pids = pids;
        this.hierarchy = hierarchy;
        this.dsIds = dsIds;
    }

    @XmlElement(name = "pids")
    public List<String> getPids() {
        return pids;
    }

    public void setPids(List<String> pids) {
        this.pids = pids;
    }

    @XmlElement(name = "dsIds")
    public List<String> getDsIds() {
        return dsIds;
    }

    public void setDsIds(List<String> dsIds) {
        this.dsIds = dsIds;
    }

    @XmlElement(name = "policy")
    public String getPolicy() {
        return policy;
    }

    public void setPolicy(String policy) {
        this.policy = policy;
    }

    @XmlElement(name = "hierarchy")
    public Boolean getHierarchy() {
        return hierarchy;
    }

    public void setHierarchy(Boolean hierarchy) {
        this.hierarchy = hierarchy;
    }

    @XmlElement(name = "krameriusInstanceId")
    public String getKrameriusInstanceId() {
        return krameriusInstanceId;
    }

    public void setKrameriusInstanceId(String krameriusInstanceId) {
        this.krameriusInstanceId = krameriusInstanceId;
    }

    @XmlElement(name = "krameriusImportInstanceId")
    public String getKrameriusImportInstanceId() {
        return krameriusImportInstanceId;
    }

    public void setKrameriusImportInstanceId(String krameriusImportInstanceId) {
        this.krameriusImportInstanceId = krameriusImportInstanceId;
    }

    @XmlElement(name = "forDownload")
    public Boolean isForDownload() {
        return forDownload;
    }

    public void setForDownload(Boolean forDownload) {
        this.forDownload = forDownload;
    }

    @XmlElement(name = "dryRun")
    public Boolean isDryRun() {
        return dryRun;
    }

    public void setDryRun(Boolean dryRun) {
        this.dryRun = dryRun;
    }

    @XmlElement(name = "typeOfPackage")
    public String getTypeOfPackage() {
        return typeOfPackage;
    }

    public void setTypeOfPackage(String typeOfPackage) {
        this.typeOfPackage = typeOfPackage;
    }

    @XmlElement(name = "ignoreMissingUrnNbn")
    public Boolean isIgnoreMissingUrnNbn() {
        return ignoreMissingUrnNbn;
    }

    public void setIgnoreMissingUrnNbn(Boolean ignoreMissingUrnNbn) {
        this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
    }

    @XmlElement(name = "bagit")
    public Boolean isBagit() {
        return bagit;
    }

    public void setBagit(Boolean bagit) {
        this.bagit = bagit;
    }

    // pokud slouzi Kramerius Export jako archivace (STT konvoluty a STT Grafiky) je potreba, aby se exportovaly vsechny datastreamy
    @XmlTransient
    public Boolean isArchive() {
        return bagit;
    }

    @XmlElement(name = "ltpCesnet")
    public Boolean isLtpCesnet() {
        return ltpCesnet;
    }

    public void setLtpCesnet(Boolean ltpCesnet) {
        this.ltpCesnet = ltpCesnet;
    }

    @XmlElement(name = "ltpCesnetToken")
    public String getLtpCesnetToken() {
        return ltpCesnetToken;
    }

    public void setLtpCesnetToken(String ltpCesnetToken) {
        this.ltpCesnetToken = ltpCesnetToken;
    }

    @XmlElement(name = "noTifAvailableMessage")
    public String getNoTifAvailableMessage() {
        return noTifAvailableMessage;
    }

    public void setNoTifAvailableMessage(String noTifAvailableMessage) {
        this.noTifAvailableMessage = noTifAvailableMessage;
    }

    @XmlElement(name = "additionalInfoMessage")
    public String getAdditionalInfoMessage() {
        return additionalInfoMessage;
    }

    public void setAdditionalInfoMessage(String additionalInfoMessage) {
        this.additionalInfoMessage = additionalInfoMessage;
    }

    @XmlElement(name = "license")
    public String getLicense() {
        return license;
    }

    public void setLicense(String license) {
        this.license = license;
    }

    @XmlElement(name = "collections")
    public List<String> getCollections() {
        return collections;
    }

    public void setCollections(List<String> collections) {
        this.collections = collections;
    }

    @XmlElement(name = "updateMods")
    public Boolean isUpdateMods() {
        return updateMods;
    }

    public void setUpdateMods(Boolean updateMods) {
        this.updateMods = updateMods;
    }

    @XmlElement(name = "extendedArchivePackage")
    public Boolean getExtendedArchivePackage() {
        return extendedArchivePackage;
    }

    public void setExtendedArchivePackage(Boolean extendedArchivePackage) {
        this.extendedArchivePackage = extendedArchivePackage;
    }

    @XmlElement(name = "deleteRawScans")
    public Boolean getDeleteRawScans() {
        return deleteRawScans;
    }

    public void setDeleteRawScans(Boolean deleteRawScans) {
        this.deleteRawScans = deleteRawScans;
    }

    @XmlElement(name = "purge")
    public Boolean isPurge() {
        return purge;
    }

    public void setPurge(Boolean purge) {
        this.purge = purge;
    }

    @XmlElement(name = "restore")
    public Boolean isRestore() {
        return restore;
    }

    public void setRestore(Boolean restore) {
        this.restore = restore;
    }

    @XmlElement(name = "type")
    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    @XmlElement(name = "peroOcrEngine")
    public Integer getPeroOcrEngine() {
        return this.peroOcrEngine;
    }

    public void setPeroOcrEngine(Integer peroOcrEngine) {
        this.peroOcrEngine = peroOcrEngine;
    }

    @XmlElement(name = "metakatEngine")
    public String getMetakatEngine() {
        return this.metakatEngine;
    }

    public void setMetakatEngine(String metakatEngine) {
        this.metakatEngine = metakatEngine;
    }

    @XmlElement(name = "notBefore")
    public String getNotBefore() {
        return notBefore;
    }

    public void setNotBefore(String notBefore) {
        this.notBefore = notBefore;
    }

    @XmlElement(name = "notAfter")
    public String getNotAfter() {
        return notAfter;
    }

    public void setNotAfter(String notAfter) {
        this.notAfter = notAfter;
    }

    @XmlElement(name = "generatePageType")
    public Boolean isGeneratePageType() {
        return generatePageType;
    }

    public void setGeneratePageType(Boolean generatePageType) {
        this.generatePageType = generatePageType;
    }

    @XmlElement(name = "updateCatalog")
    public Boolean isUpdateCatalog() {
        return updateCatalog;
    }

    public void setUpdateCatalog(Boolean updateCatalog) {
        this.updateCatalog = updateCatalog;
    }

    @XmlElement(name = "catalogId")
    public String getCatalogId() {
        return catalogId;
    }

    public void setCatalogId(String catalogId) {
        this.catalogId = catalogId;
    }

    @XmlElement(name = "sourceBatchId")
    public Integer getSourceBatchId() {
        return sourceBatchId;
    }

    public void setSourceBatchId(Integer sourceBatchId) {
        this.sourceBatchId = sourceBatchId;
    }

    @XmlElement(name = "nextBatchId")
    public Integer getNextBatchId() {
        return nextBatchId;
    }

    public void setNextBatchId(Integer nextBatchId) {
        this.nextBatchId = nextBatchId;
    }

    @XmlElement(name = "bagitArchives")
    public List<String> getBagitArchives() {
        return bagitArchives;
    }

    public void setBagitArchives(List<String> bagitArchives) {
        this.bagitArchives = bagitArchives;
    }

    @XmlElement(name = "completedPackageSources")
    public List<String> getCompletedPackageSources() {
        return completedPackageSources;
    }

    public void setCompletedPackageSources(List<String> completedPackageSources) {
        this.completedPackageSources = completedPackageSources;
    }

    @XmlElement(name = "bagitComplete")
    public Boolean getBagitComplete() {
        return bagitComplete;
    }

    public void setBagitComplete(Boolean bagitComplete) {
        this.bagitComplete = bagitComplete;
    }

    /** Export output retained while packaging runs in the same batch. */
    @XmlElement(name = "bagitSourceFolder")
    public String getBagitSourceFolder() {
        return bagitSourceFolder;
    }

    public void setBagitSourceFolder(String bagitSourceFolder) {
        this.bagitSourceFolder = bagitSourceFolder;
    }

    @XmlElement(name = "completedUploads")
    public List<String> getCompletedUploads() {
        return completedUploads;
    }

    public void setCompletedUploads(List<String> completedUploads) {
        this.completedUploads = completedUploads;
    }

    @XmlElement(name = "confirmCesnetUpload")
    public Boolean getConfirmCesnetUpload() { return confirmCesnetUpload; }
    public void setConfirmCesnetUpload(Boolean value) { confirmCesnetUpload = value; }

    @XmlElement(name = "cesnetOperationId")
    public String getCesnetOperationId() { return cesnetOperationId; }
    public void setCesnetOperationId(String value) { cesnetOperationId = value; }

    /** ProArc user supplying temporary credentials, never the token itself. */
    @XmlElement(name = "cesnetCredentialOwnerId")
    public Integer getCesnetCredentialOwnerId() { return cesnetCredentialOwnerId; }
    public void setCesnetCredentialOwnerId(Integer value) { cesnetCredentialOwnerId = value; }

    @XmlElement(name = "rawScanFolders")
    public List<String> getRawScanFolders() {
        return rawScanFolders;
    }

    public void setRawScanFolders(List<String> rawScanFolders) {
        this.rawScanFolders = rawScanFolders;
    }
}
