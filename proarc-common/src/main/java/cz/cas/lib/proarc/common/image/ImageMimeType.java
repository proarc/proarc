package cz.cas.lib.proarc.common.image;

public enum ImageMimeType {
    JPEG("image/jpeg", "jpg", true, false, false),
    PNG("image/png", "png", true, false, false),
    JPEG2000("image/jp2", "jp2", true, false, false),
    XDJVU("image/x.djvu", "djvu", false, false, true),
    VNDDJVU("image/vnd.djvu", "djvu", false, false, true),
    DJVU("image/djvu", "djvu", false, false, true),
    PDF("application/pdf", "pdf", false, false, true),
    TIFF("image/tiff", "tiff", false, true, false);

    private String mime;
    private boolean supportedbyJava;
    private boolean supportedbyJAI;
    private boolean multipageFormat;
    private String defaultFileExtension;

    private ImageMimeType(String mime, String defaultExtension, boolean javasupport, boolean jaiSupport, boolean multipageformat) {
        this.mime = mime;
        this.supportedbyJava = javasupport;
        this.supportedbyJAI = jaiSupport;
        this.multipageFormat = multipageformat;
        this.defaultFileExtension = defaultExtension;
    }

    public String getMimeType() {
        return this.mime;
    }

    public boolean javaNativeSupport() {
        return this.supportedbyJava;
    }

    public boolean isMultipageFormat() {
        return this.multipageFormat;
    }

    public boolean isSupportedbyJAI() {
        return this.supportedbyJAI;
    }

    public void setSupportedbyJAI(boolean supportedbyJAI) {
        this.supportedbyJAI = supportedbyJAI;
    }

    public String getDefaultFileExtension() {
        return this.defaultFileExtension;
    }

    public static ImageMimeType loadFromMimeType(String mime) {
        ImageMimeType[] values = values();

        for(ImageMimeType iType : values) {
            if (iType.getMimeType().equals(mime)) {
                return iType;
            }
        }

        return null;
    }
}