package cz.cas.lib.proarc.common.dao;

import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

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
    private Boolean extendedArchivePackage;
    private Boolean purge;
    private Boolean restore;
    private String type;

    public BatchParams() {}

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

    public BatchParams(List<String> pids, String typeOfPackage, Boolean ignoreMissingUrnNbn, Boolean bagit, String noTifAvailableMessage, String additionalInfoMessage, Boolean extendedArchivePackage) {
        this.pids = pids;
        this.typeOfPackage = typeOfPackage;
        this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
        this.bagit = bagit;
        this.noTifAvailableMessage = noTifAvailableMessage;
        this.additionalInfoMessage = additionalInfoMessage;
        this.extendedArchivePackage = extendedArchivePackage;
    }

    public BatchParams(List<String> pids, String typeOfPackage, Boolean ignoreMissingUrnNbn, Boolean bagit, Boolean ltpCesnet, String ltpCesnetToken, String krameriusInstanceId, String policy, String license) {
        this.pids = pids;
        this.typeOfPackage = typeOfPackage;
        this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
        this.bagit = bagit;
        this.ltpCesnet = ltpCesnet;
        this.ltpCesnetToken = ltpCesnetToken;
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

    @XmlElement(name = "extendedArchivePackage")
    public Boolean getExtendedArchivePackage() {
        return extendedArchivePackage;
    }

    public void setExtendedArchivePackage(Boolean extendedArchivePackage) {
        this.extendedArchivePackage = extendedArchivePackage;
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
}
