package cz.cas.lib.proarc.common.fedora;

public class SearchViewItem {

    private String pid;
    private String model;
    private String owner;
    private String label;
    private String state;
    private String created;
    private String modified;
    /**
     * Parent PID. Optional for some queries
     */
    private String parent;
    /**
     * batch import ID. Optional for some queries
     */
    private Integer batchId;
    private String organization;
    private String user;
    private String status;
    private String validation;
    /**
     * Synthetic name of count query. count(hasExport)
     *
     * @see <a href='http://docs.mulgara.org/itqlcommands/select.html#o194'>
     * Count Function</a>
     */
    private String k0;
    private String k1;
    private String k2;
    private String k3;
    private String k4;
    private String k5;

    private String ndkExportPath;
    private String krameriusExportPath;
    private String archiveExportPath;
    private String crossrefExportPath;
    private Boolean isLocked;
    private String content;

    public SearchViewItem() {
    }

    public SearchViewItem(String pid) {
        this.pid = pid;
    }

    public String getOrganization() {
        return organization;
    }

    public void setOrganization(String organization) {
        this.organization = organization;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getCreated() {
        return created;
    }

    public void setCreated(String created) {
        this.created = created;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getModel() {
        return model;
    }

    public void setModel(String model) {
        this.model = model;
    }

    public String getModified() {
        return modified;
    }

    public void setModified(String modified) {
        this.modified = modified;
    }

    public String getOwner() {
        return owner;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public String getPid() {
        return pid;
    }

    public void setPid(String pid) {
        this.pid = pid;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getParentPid() {
        return parent;
    }

    public void setParentPid(String parentPid) {
        this.parent = parentPid;
    }

    public Integer getBatchId() {
        return batchId;
    }

    public void setBatchId(Integer batchId) {
        this.batchId = batchId;
    }

    public Integer isLocked() {
        if (k5 != null && !k5.isEmpty()) {
            try {
                return Integer.parseInt(k5);
            } catch (NumberFormatException ex) {
                // ignore
            }
        }
        return null;
    }

    public void setK5(String k5) {
        this.k5 = k5;
    }

    public String getK0() {
        return k0;
    }

    public void setK0(String k0) {
        this.k0 = k0;
    }

    public Integer getHasExport() {
        if (k0 != null && !k0.isEmpty()) {
            try {
                return Integer.parseInt(k0);
            } catch (NumberFormatException ex) {
                // ignore
            }
        }
        return null;
    }

    public String getK1() {
        return k1;
    }

    public void setK1(String k1) {
        this.k1 = k1;
    }

    public Integer getHasNdkExport() {
        if (k1 != null && !k1.isEmpty()) {
            try {
                return Integer.parseInt(k1);
            } catch (NumberFormatException ex) {
                // ignore
            }
        }
        return null;
    }

    public String getK2() {
        return k2;
    }

    public void setK2(String k2) {
        this.k2 = k2;
    }

    public Integer getHasKrameriusExport() {
        if (k2 != null && !k2.isEmpty()) {
            try {
                return Integer.parseInt(k2);
            } catch (NumberFormatException ex) {
                // ignore
            }
        }
        return null;
    }

    public String getK3() {
        return k3;
    }

    public void setK3(String k3) {
        this.k3 = k3;
    }

    public Integer getHasArchiveExport() {
        if (k3 != null && !k3.isEmpty()) {
            try {
                return Integer.parseInt(k3);
            } catch (NumberFormatException ex) {
                // ignore
            }
        }
        return null;
    }

    public String getK4() {
        return k4;
    }

    public void setK4(String k4) {
        this.k4 = k4;
    }

    public Integer getHasCrossrefExport() {
        if (k4 != null && !k4.isEmpty()) {
            try {
                return Integer.parseInt(k4);
            } catch (NumberFormatException ex) {
                // ignore
            }
        }
        return null;
    }

    public String getValidation() {
        return validation;
    }

    public void setValidation(String validation) {
        this.validation = validation;
    }

    public String getJsonData() {
        return content;
    }

    public void setJsonData(String content) {
        this.content = content;
    }

    public String getNdkExportPath() {
        return ndkExportPath;
    }

    public void setNdkExportPath(String ndkExportPath) {
        this.ndkExportPath = ndkExportPath;
    }

    public String getKrameriusExportPath() {
        return krameriusExportPath;
    }

    public void setKrameriusExportPath(String krameriusExportPath) {
        this.krameriusExportPath = krameriusExportPath;
    }

    public String getArchiveExportPath() {
        return archiveExportPath;
    }

    public void setArchiveExportPath(String archiveExportPath) {
        this.archiveExportPath = archiveExportPath;
    }

    public String getCrossrefExportPath() {
        return crossrefExportPath;
    }

    public void setCrossrefExportPath(String crossrefExportPath) {
        this.crossrefExportPath = crossrefExportPath;
    }

    public Boolean getIsLocked() {
        return isLocked;
    }

    public void setIsLocked(Boolean isLocked) {
        this.isLocked = isLocked;
    }
}
