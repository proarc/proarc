package cz.cas.lib.proarc.common.xml.docmd;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "document", namespace = "http://www.fcla.edu/docmd")
public class DocumentMd {

    @XmlElement(name = "PageCount", namespace = "http://www.fcla.edu/docmd")
    protected BigInteger pageCount;

    @XmlElement(name = "WordCount", namespace = "http://www.fcla.edu/docmd")
    protected BigInteger wordCount;

    @XmlElement(name = "CharacterCount", namespace = "http://www.fcla.edu/docmd")
    protected BigInteger characterCount;

    @XmlElement(name = "ParagraphCount", namespace = "http://www.fcla.edu/docmd")
    protected BigInteger paragraphCount;

    @XmlElement(name = "LineCount", namespace = "http://www.fcla.edu/docmd")
    protected BigInteger lineCount;

    @XmlElement(name = "TableCount", namespace = "http://www.fcla.edu/docmd")
    protected BigInteger tableCount;

    @XmlElement(name = "GraphicsCount", namespace = "http://www.fcla.edu/docmd")
    protected BigInteger graphicsCount;

    @XmlElement(name = "Language", namespace = "http://www.fcla.edu/docmd")
    protected List<String> language;

    @XmlElement(name = "Font", namespace = "http://www.fcla.edu/docmd")
    protected List<FontType> font;

    @XmlElement(name = "Features", namespace = "http://www.fcla.edu/docmd")
    protected List<FeaturesType> features;

    @XmlElement(name = "documentMetadataExtension", namespace = "http://www.fcla.edu/docmd")
    protected ExtensionType documentMetadataExtension;

    public BigInteger getPageCount() {
        return pageCount;
    }

    public void setPageCount(BigInteger pageCount) {
        this.pageCount = pageCount;
    }

    public BigInteger getWordCount() {
        return wordCount;
    }

    public void setWordCount(BigInteger wordCount) {
        this.wordCount = wordCount;
    }

    public BigInteger getCharacterCount() {
        return characterCount;
    }

    public void setCharacterCount(BigInteger characterCount) {
        this.characterCount = characterCount;
    }

    public BigInteger getParagraphCount() {
        return paragraphCount;
    }

    public void setParagraphCount(BigInteger paragraphCount) {
        this.paragraphCount = paragraphCount;
    }

    public BigInteger getLineCount() {
        return lineCount;
    }

    public void setLineCount(BigInteger lineCount) {
        this.lineCount = lineCount;
    }

    public BigInteger getTableCount() {
        return tableCount;
    }

    public void setTableCount(BigInteger tableCount) {
        this.tableCount = tableCount;
    }

    public BigInteger getGraphicsCount() {
        return graphicsCount;
    }

    public void setGraphicsCount(BigInteger graphicsCount) {
        this.graphicsCount = graphicsCount;
    }

    public List<String> getLanguage() {
        if (this.language == null) {
            this.language = new ArrayList();
        }
        return language;
    }

    public void setLanguage(List<String> language) {
        this.language = language;
    }

    public List<FontType> getFont() {
        if (this.font == null) {
            this.font = new ArrayList();
        }
        return font;
    }

    public void setFont(List<FontType> font) {
        this.font = font;
    }

    public List<FeaturesType> getFeatures() {
        if (this.features == null) {
            this.features = new ArrayList();
        }
        return features;
    }

    public void setFeatures(List<FeaturesType> features) {
        this.features = features;
    }

    public ExtensionType getDocumentMetadataExtension() {
        return documentMetadataExtension;
    }

    public void setDocumentMetadataExtension(ExtensionType documentMetadataExtension) {
        this.documentMetadataExtension = documentMetadataExtension;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "fontType", propOrder = {"value"})
    public static class FontType {

        @XmlAttribute(name = "FontName")
        protected String fontName;

        @XmlAttribute(name = "isEmbedded")
        protected Boolean isEmbedded;

        @XmlValue
        protected String value;

        public String getFontName() {
            return fontName;
        }

        public void setFontName(String fontName) {
            this.fontName = fontName;
        }

        public Boolean getEmbedded() {
            return isEmbedded;
        }

        public void setEmbedded(Boolean embedded) {
            isEmbedded = embedded;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "extensionType", propOrder = {"any"})
    public static class ExtensionType {

        @XmlAnyElement(lax = true)
        protected List<Object> any;

        public List<Object> getAny() {
            if (this.any == null) {
                this.any = new ArrayList();
            }
            return any;
        }

        public void setAny(List<Object> any) {
            this.any = any;
        }
    }

    @XmlEnum
    public static enum FeaturesType {

        IS_TAGGED("isTagged"),
        HAS_OUTLINE("hasOutline"),
        HAS_THUMBNAILS("hasThumbnails"),
        HAS_LAYERS("hasLayers"),
        HAS_FORMS("hasForms"),
        HAS_ANNOTATIONS("hasAnnotations"),
        HAS_ATTACHMENTS("hasAttachments"),
        USE_TRANSPARENCY("useTransparency");

        private final String value;

        private FeaturesType(String v) {
            this.value = v;
        }

        public static FeaturesType fromValue(String v) {
            FeaturesType[] var1 = values();
            int var2 = var1.length;

            for(int var3 = 0; var3 < var2; ++var3) {
                FeaturesType c = var1[var3];
                if (c.value.equals(v)) {
                    return c;
                }
            }

            throw new IllegalArgumentException(v);
        }

        public String getValue() {
            return value;
        }
    }
}
