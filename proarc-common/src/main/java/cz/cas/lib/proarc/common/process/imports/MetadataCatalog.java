package cz.cas.lib.proarc.common.process.imports;

public class MetadataCatalog {

    private String folderPath;
    private String title;
    private String subtitle;
    private String partName;
    private String partNumber;
    private String dateIssued;
    private String localId;

    private String pages;
    private String note;

    private String model;
    private String parentUUID;

    public MetadataCatalog() {
    }

    private String setValue(String value) {
        return value == null || value.isEmpty() ? null : value;
    }

    public void setFolderPath(String folderPath) {
        this.folderPath = setValue(folderPath);
    }

    public void setTitle(String title) {
        this.title = setValue(title);
    }

    public void setSubtitle(String subtitle) {
        this.subtitle = setValue(subtitle);
    }

    public void setPartName(String partName) {
        this.partName = setValue(partName);
    }

    public void setPartNumber(String partNumber) {
        this.partNumber = setValue(partNumber);
    }

    public void setDateIssued(String dateIssued) {
        this.dateIssued = setValue(dateIssued);
    }

    public void setPages(String pages) {
        this.pages = setValue(pages);
    }

    public void setNote(String note) {
        this.note = setValue(note);
    }

    public void setLocalId(String localId) {
        this.localId = setValue(localId);
    }

    public void setModel(String model) {
        this.model = setValue(model);
    }

    public void setParent(String parent) {
        this.parentUUID = setValue(parent);
    }

    public String getFolderPath() {
        return folderPath;
    }

    public String getTitle() {
        return title;
    }

    public String getSubtitle() {
        return subtitle;
    }

    public String getPartName() {
        return partName;
    }

    public String getPartNumber() {
        return partNumber;
    }

    public String getDateIssued() {
        return dateIssued;
    }

    public String getPages() {
        return pages;
    }

    public String getNote() {
        return note;
    }

    public String getLocalId() {
        return localId;
    }

    public String getModel() {
        return model;
    }

    public String getParent() {
        if (parentUUID != null && !parentUUID.startsWith("uuid:")) {
            return "uuid:" + parentUUID;
        }
        return parentUUID;
    }
}
