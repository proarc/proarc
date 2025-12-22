package cz.cas.lib.proarc.common.dao;

import javax.xml.bind.annotation.*;
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
    private boolean forDownload;
    private boolean dryRun;
    private String typeOfPackage;
    private boolean ignoreMissingUrnNbn;
    private boolean bagit;
    private boolean ltpCesnet;
    private String ltpCesnetToken;
    private String noTifAvailableMessage;
    private String additionalInfoMessage;
    private String license;
    private Boolean extendedArchivePackage;
    private Boolean purge;
    private Boolean restore;
    private String type;
    private Integer peroOcrEngine;

    public BatchParams() {}

    public BatchParams(List<String> pids) {
        this.pids = pids;
    }

    public BatchParams(List<String> pids, Boolean hierarchy, Boolean purge, Boolean restore) {
        this.pids = pids;
        this.hierarchy = hierarchy;
        this.purge = purge;
        this.restore = restore;
    }

    public BatchParams(List<String> pids, String policy, boolean hierarchy, String krameriusInstanceId, boolean bagit, String license) {
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

    public BatchParams(List<String> pids, String typeOfPackage, boolean ignoreMissingUrnNbn, boolean bagit, String noTifAvailableMessage, String additionalInfoMessage, Boolean extendedArchivePackage) {
        this.pids = pids;
        this.typeOfPackage = typeOfPackage;
        this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
        this.bagit = bagit;
        this.noTifAvailableMessage = noTifAvailableMessage;
        this.additionalInfoMessage = additionalInfoMessage;
        this.extendedArchivePackage = extendedArchivePackage;
    }

    public BatchParams(List<String> pids, String typeOfPackage, boolean ignoreMissingUrnNbn, boolean bagit, boolean ltpCesnet, String ltpCesnetToken, String krameriusInstanceId, String policy, String license) {
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

    public BatchParams(List<String> pids, boolean hierarchy, boolean forDownload, boolean dryRun) {
        this.pids = pids;
        this.hierarchy = hierarchy;
        this.forDownload = forDownload;
        this.dryRun = dryRun;
    }

    public BatchParams(List<String> pids, boolean hierarchy, List<String> dsIds) {
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
    public boolean isForDownload() {
        return forDownload;
    }

    public void setForDownload(boolean forDownload) {
        this.forDownload = forDownload;
    }

    @XmlElement(name = "dryRun")
    public boolean isDryRun() {
        return dryRun;
    }

    public void setDryRun(boolean dryRun) {
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
    public boolean isIgnoreMissingUrnNbn() {
        return ignoreMissingUrnNbn;
    }

    public void setIgnoreMissingUrnNbn(boolean ignoreMissingUrnNbn) {
        this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
    }

    @XmlElement(name = "bagit")
    public boolean isBagit() {
        return bagit;
    }

    public void setBagit(boolean bagit) {
        this.bagit = bagit;
    }

    // pokud slouzi Kramerius Export jako archivace (STT konvoluty a STT Grafiky) je potreba, aby se exportovaly vsechny datastreamy
    @XmlTransient
    public boolean isArchive() {
        return bagit;
    }

    @XmlElement(name = "ltpCesnet")
    public boolean isLtpCesnet() {
        return ltpCesnet;
    }

    public void setLtpCesnet(boolean ltpCesnet) {
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

    @XmlElement(name = "peroOcrEngine")
    public Integer getPeroOcrEngine() {
        return (peroOcrEngine == null || peroOcrEngine < 1) ? 1 : peroOcrEngine;
    }

    public void setPeroOcrEngine(Integer peroOcrEngine) {
        this.peroOcrEngine = peroOcrEngine;
    }
}
