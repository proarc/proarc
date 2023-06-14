package cz.cas.lib.proarc.common.dao;

import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "params")
public class BatchParams {

    @XmlElement (name = "pids")
    private List<String> pids;

    @XmlElement (name = "dsIds")
    private List<String> dsIds;

    @XmlElement(name = "policy")
    private String policy;

    @XmlElement (name =  "hierarchy")
    private boolean hierarchy;

    @XmlElement (name = "krameriusInstanceId")
    private String krameriusInstanceId;

    @XmlElement (name = "krameriusImportInstanceId")
    private String krameriusImportInstanceId;

    @XmlElement (name = "forDownload")
    private boolean forDownload;

    @XmlElement (name = "dryRun")
    private boolean dryRun;

    @XmlElement (name = "typeOfPackage")
    private String typeOfPackage;

    @XmlElement (name = "ignoreMissingUrnNbn")
    private boolean ignoreMissingUrnNbn;

    @XmlElement (name = "bagit")
    private boolean bagit;

    @XmlElement (name = "ltpCesnet")
    private boolean ltpCesnet;

    @XmlElement (name = "ltpCesnetToken")
    private String ltpCesnetToken;

    public BatchParams() {}

    public BatchParams(List<String> pids) {
        this.pids = pids;
    }

    public BatchParams(List<String> pids, String policy, boolean hierarchy, String krameriusInstanceId, boolean bagit) {
        this.pids = pids;
        this.policy = policy;
        this.hierarchy = hierarchy;
        this.krameriusInstanceId = krameriusInstanceId;
        this.bagit = bagit;
    }

    public BatchParams(List<String> pids, String krameriusInstanceId, String krameriusImportInstanceId) {
        this.pids = pids;
        this.krameriusInstanceId = krameriusInstanceId;
        this.krameriusImportInstanceId = krameriusImportInstanceId;
    }

    public BatchParams(List<String> pids, String typeOfPackage, boolean ignoreMissingUrnNbn, boolean bagit) {
        this.pids = pids;
        this.typeOfPackage = typeOfPackage;
        this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
        this.bagit = bagit;
    }

    public BatchParams(List<String> pids, String typeOfPackage, boolean ignoreMissingUrnNbn, boolean bagit, boolean ltpCesnet, String ltpCesnetToken) {
        this.pids = pids;
        this.typeOfPackage = typeOfPackage;
        this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
        this.bagit = bagit;
        this.ltpCesnet = ltpCesnet;
        this.ltpCesnetToken = ltpCesnetToken;
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

    public List<String> getPids() {
        return pids;
    }

    public String getPolicy() {
        return policy;
    }

    public boolean isHierarchy() {
        return hierarchy;
    }

    public String getKrameriusInstanceId() {
        return krameriusInstanceId;
    }

    public String getKrameriusImportInstanceId() {
        return krameriusImportInstanceId;
    }

    public boolean isForDownload() {
        return forDownload;
    }

    public boolean isDryRun() {
        return dryRun;
    }

    public String getTypeOfPackage() {
        return typeOfPackage;
    }

    public boolean isIgnoreMissingUrnNbn() {
        return ignoreMissingUrnNbn;
    }

    public boolean isBagit() {
        return bagit;
    }

    // pokud slouzi Kramerius Export jako archivace (STT konvoluty a STT Grafiky) je potreba, aby se exportovaly vsechny datastreamy
    public boolean isArchive() {
        return bagit;
    }

    public List<String> getDsIds() {
        return dsIds;
    }

    public boolean isLtpCesnet() {
        return ltpCesnet;
    }

    public String getLtpCesnetToken() {
        return ltpCesnetToken;
    }
}
