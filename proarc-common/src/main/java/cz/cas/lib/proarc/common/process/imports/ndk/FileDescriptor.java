package cz.cas.lib.proarc.common.process.imports.ndk;

/**
 * Reprezentace digitalniho objektu
 *
 * @author Lukas Sykora
 */
public class FileDescriptor {

    private String filename;

    private StreamFileType fileType;


    public FileDescriptor(String filename, StreamFileType fileType) {
        super();
        this.filename = filename;
        this.fileType = fileType;
    }

    public StreamFileType getFileType() {
        return fileType;
    }

    public void setFileType(StreamFileType fileType) {
        this.fileType = fileType;
    }


    public String getFilename() {
        return filename;
    }

    public void setFilename(String filename) {
        this.filename = filename;
    }


}
