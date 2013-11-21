package cz.cas.lib.proarc.common.export.mets;

import java.util.List;

public class MetsExportTestElement {
    private final List<String> fileList;
    private final String directory;
    private final int totalItems;
    private final int size;
    private final int numberOfFiles;
    private final String type;
    private final String initialDocument;

    public String getInitialDocument() {
        return initialDocument;
    }

    public List<String> getFileList() {
        return fileList;
    }

    public String getDirectory() {
        return directory;
    }

    public int getTotalItems() {
        return totalItems;
    }

    public int getSize() {
        return size;
    }

    public int getNumberOfFiles() {
        return numberOfFiles;
    }

    public String getType() {
        return type;
    }

    public MetsExportTestElement(List<String> fileList, String directory, int totalItems, int size, int numberOfFiles, String type, String initialDocument) {
        super();
        this.fileList = fileList;
        this.directory = directory;
        this.totalItems = totalItems;
        this.size = size;
        this.numberOfFiles = numberOfFiles;
        this.type = type;
        this.initialDocument = initialDocument;
    }
}
