package cz.cas.lib.proarc.common.workflow;

import org.apache.commons.configuration2.Configuration;

public class WorkflowOptions {
    static final String PROP_MATERIAL_FOLDER_RAW_SCAN = "workflow.material.folder.rawScan";
    static final String PROP_MATERIAL_FOLDER_MASTER_COPY = "workflow.material.folder.masterCopy";
    static final String PROP_MATERIAL_FOLDER_OCR = "workflow.material.folder.ocr";
    static final String PROP_MATERIAL_FOLDER_OCR_IMAGE = "workflow.material.folder.OcrAndProcessedImages";

    private String rawScan;
    private String masterCopy;
    private String ocr;
    private String ocrImage;

    public static WorkflowOptions getOptions(Configuration config) {
        WorkflowOptions options = new WorkflowOptions();

        String materialFolderRawScan = config.getString(PROP_MATERIAL_FOLDER_RAW_SCAN);
        if (materialFolderRawScan == null) {
            materialFolderRawScan = "";
        }
        options.setRawScan(materialFolderRawScan);

        String materialFolderMasterCopy = config.getString(PROP_MATERIAL_FOLDER_MASTER_COPY);
        if (materialFolderMasterCopy == null) {
            materialFolderMasterCopy = "";
        }
        options.setMasterCopy(materialFolderMasterCopy);

        String materialFolderOcr = config.getString(PROP_MATERIAL_FOLDER_OCR);
        if (materialFolderOcr == null) {
            materialFolderOcr = "";
        }
        options.setOcr(materialFolderOcr);

        String materialFolderOcrProcess = config.getString(PROP_MATERIAL_FOLDER_OCR_IMAGE);
        if (materialFolderOcrProcess == null) {
            materialFolderOcrProcess = "";
        }
        options.setOcrImage(materialFolderOcrProcess);

        return options;
    }

    public String getRawScan() {
        return rawScan;
    }

    public void setRawScan(String rawScan) {
        this.rawScan = rawScan;
    }

    public String getMasterCopy() {
        return masterCopy;
    }

    public void setMasterCopy(String masterCopy) {
        this.masterCopy = masterCopy;
    }

    public String getOcr() {
        return ocr;
    }

    public void setOcr(String ocr) {
        this.ocr = ocr;
    }

    public String getOcrImage() {
        return ocrImage;
    }

    public void setOcrImage(String ocrImage) {
        this.ocrImage = ocrImage;
    }
}
