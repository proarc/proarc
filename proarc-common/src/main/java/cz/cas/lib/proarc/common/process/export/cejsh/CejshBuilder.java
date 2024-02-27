/*
 * Copyright (C) 2015 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.process.export.cejsh;

import cz.cas.lib.proarc.common.process.export.ExportParams;
import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.process.export.mets.ValidationErrorHandler;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import cz.cas.lib.proarc.common.xml.TransformErrorListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Properties;
import java.util.TimeZone;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import javax.xml.XMLConstants;
import javax.xml.bind.DatatypeConverter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.apache.commons.io.Charsets;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import net.lingala.zip4j.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.ZipParameters;
import net.lingala.zip4j.model.enums.CompressionLevel;
import net.lingala.zip4j.model.enums.CompressionMethod;
import net.lingala.zip4j.model.enums.EncryptionMethod;

/**
 * Builds the cejsh package of articles.
 *
 * @author Jan Pokorsky
 * @see <a href='https://trac.icm.edu.pl/yadda/Public:BWmeta_format'>BWmeta format</a>
 */
class CejshBuilder {

    static final String IMPORT_PROPERTIES_FILENAME = "import.properties";
    static final String IMPORTS_NEW_FILENAME = "imports_new";
    /**
     * The Cejsh namespace.
     */
    public static final String NS_BWMETA105 = "http://yadda.icm.edu.pl/bwmeta-1.0.5.xsd";
    static final String XSD_FILENAME = "bwmeta-1.0.5.xsd";
    static final String P0XML_FILENAME = "p0.xml";
    static final String PROP_IMPORT_BWMETA_FILES = "import.import_bwmeta_files";
    static final String PROP_IMPORT_CONTENT_FILES = "import.content_files";
    static final String PROP_IMPORT_OBJECTS = "import.import_objects";
    static final String PROP_IMPORT_INFODATE = "import.info.date";

    static final TimeZone UTC = TimeZone.getTimeZone("UTC");
    private static Schema SCHEMA_BWMETA;
    private static Pattern SAFE_FILENAME_RE;
    private static final Logger LOG = Logger.getLogger(CejshBuilder.class.getName());
    private final ExportParams options;

    private final Transformer bwmetaXsl;
    private final TransformErrorListener tranformationErrorHandler;
    private Validator bwValidator;
    private final DocumentBuilder db;
    private final XPathExpression issnPath;
    private final XPathExpression partNumberPath;
    private final XPathExpression dateIssuedPath;
    private final XPathExpression reviewedArticlePath;
    private final XPathExpression englishArticlePath;
    private final GregorianCalendar gcalendar;
    private Title title;
    private Volume volume;
    private Issue issue;
    private Level logLevel;

    public CejshBuilder(CejshConfig config, ExportParams options)
            throws Exception {
        this.gcalendar = new GregorianCalendar(UTC);
        this.logLevel = config.getLogLevel();
        this.options = options;
        TransformerFactory xslFactory = TransformerFactory.newInstance();
        tranformationErrorHandler = new TransformErrorListener();
        bwmetaXsl = xslFactory.newTransformer(new StreamSource(config.getCejshXslUrl()));
        if (bwmetaXsl == null) {
            throw new TransformerConfigurationException("Cannot load XSL: " + config.getCejshXslUrl());
        }
        bwmetaXsl.setOutputProperty(OutputKeys.INDENT, "yes");
        bwmetaXsl.setErrorListener(tranformationErrorHandler);
        if (options.getJournalsInfoPath() == null || options.getJournalsInfoPath().length() == 0) {
            throw new Exception("Not configurated path : \"export.cejsh_crossref.journals.path=\".");
        }
        bwmetaXsl.setParameter("journalsInfo", options.getJournalsInfoPath());
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        db = dbf.newDocumentBuilder();
        XPathFactory xPathFactory = ProarcXmlUtils.defaultXPathFactory();
        XPath xpath = xPathFactory.newXPath();
        xpath.setNamespaceContext(new SimpleNamespaceContext().add("m", ModsConstants.NS));
        issnPath = xpath.compile("m:mods/m:identifier[@type='issn' and not(@invalid)]");
        partNumberPath = xpath.compile("m:mods/m:titleInfo/m:partNumber");
        dateIssuedPath = xpath.compile("m:mods/m:originInfo/m:dateIssued");
        reviewedArticlePath = xpath.compile("m:mods/m:genre[text()='article' and @type='peer-reviewed']");
        englishArticlePath = xpath.compile("m:mods/m:abstract[@lang='eng']");
    }

    public XPathExpression getDateIssuedPath() {
        return dateIssuedPath;
    }

    public XPathExpression getIssnPath() {
        return issnPath;
    }

    public XPathExpression getPartNumberPath() {
        return partNumberPath;
    }

    public XPathExpression getReviewedArticlePath() {
        return reviewedArticlePath;
    }

    public XPathExpression getEnglishArticlePath() {
        return englishArticlePath;
    }

    public Article addArticle(DigitalObjectElement article, CejshContext p) {
        Document articleDom = getModsDom(article, p);
        return addArticle(articleDom, article, p);
    }

    Article addArticle(Document articleDom, DigitalObjectElement article, CejshContext p) {
        if (articleDom == null) {
            p.getStatus().error(article, "Missing article MODS!", null, null);
            return null;
        }
        try {
            Object isReviewed = getReviewedArticlePath().evaluate(articleDom, XPathConstants.BOOLEAN);
            if (!(isReviewed instanceof Boolean) || !((Boolean) isReviewed)) {
                LOG.log(logLevel, "Skipped not reviewed article: {0}", article.getPid());
                return new Article().setReviewed(false);
            }
            Object hasEnglishAbstract = getEnglishArticlePath().evaluate(articleDom, XPathConstants.BOOLEAN);
            if (!(hasEnglishAbstract instanceof Boolean) || !((Boolean) hasEnglishAbstract)) {
                LOG.log(logLevel, "Skipped article has not english abstract", article.getPid());
                return new Article().setEnglishAbstract(false);
            }
        } catch (XPathExpressionException ex) {
            p.getStatus().error(article, "Unexpected error!", null, ex);
        }
        if (getTitle() == null) {
            p.getStatus().error(article, "Missing title!", null, null);
            return null;
        }
        if (getVolume() == null) {
            p.getStatus().error(article, "Missing volume!", null, null);
            return null;
        }
        try {
            String articleIssn = getIssnPath().evaluate(articleDom);
            // XXX check mods vs modsCollection?
            Element modsElement = articleDom.getDocumentElement();
            return new Article(article, modsElement, articleIssn)
                    .setReviewed(true).setEnglishAbstract(true);
        } catch (Exception ex) {
            p.getStatus().error(article, "Unexpected error!", null, ex);
        }
        return null;
    }

    String getPackageIssn() {
        String issn = getIssue() == null ? null : getIssue().getIssn();
        if (issn == null || issn.isEmpty()) {
            issn = getTitle() == null ? null : getTitle().getIssn();
            if (issn == null || issn.isEmpty()) {
                issn = "NA";
            }
        }
        return issn;
    }

    /**
     * Transforms a collection of articles to the bwmeta document.
     * @param src modsCollection in MODS format
     * @param dst bwmeta document
     * @return the error handler
     */
    TransformErrorListener createCejshXml(Source src, Result dst) {
        bwmetaXsl.reset();
        String packageIssn = getPackageIssn();
        bwmetaXsl.setParameter("issn", packageIssn);
        if (getVolume() != null) {
            bwmetaXsl.setParameter("volume", getVolume().getVolumeNumber());
            bwmetaXsl.setParameter("volumeId", getVolume().getVolumeId());
            bwmetaXsl.setParameter("year", getVolume().getYear());
        }
        if (getIssue() != null) {
            bwmetaXsl.setParameter("issue", getIssue().getIssueNumber());
            bwmetaXsl.setParameter("issueId", getIssue().getIssueId());
            bwmetaXsl.setParameter("date", getIssue().getDateIssued());
        }
        try {
            tranformationErrorHandler.reset();
            bwmetaXsl.transform(src, dst);
        } catch (TransformerException ex) {
            if (tranformationErrorHandler.getErrors().isEmpty()) {
                tranformationErrorHandler.getErrors().add(ex.getMessageAndLocation());
            }
        }
        return tranformationErrorHandler;
    }

    public File writePackage(DigitalObjectElement packageElm, List<Article> articles, CejshContext p) {
        if (articles == null || articles.isEmpty()) {
            return null;
        }
        File packageFolder = null;
        try {
            Document doc = mergeElements(articles);

            String pkgName = createPackageName();
            packageFolder = ExportUtils.createFolder(p.getOutput(), pkgName, options.isOverwritePackage());
            File importFolder = new File(packageFolder, IMPORTS_NEW_FILENAME);
            importFolder.mkdirs();
            writeProperties(packageFolder, articles.size());

            File p0xml = new File(importFolder, P0XML_FILENAME);
            DOMSource domSource = new DOMSource(doc);
            TransformErrorListener cejshXslErrors = createCejshXml(domSource, new StreamResult(p0xml));
            if (!cejshXslErrors.getErrors().isEmpty()) {
                p.getStatus().error(packageElm, "Validation error!", cejshXslErrors.getErrors().toString(), null);
                return packageFolder;
            }
            // validate XML after writing to disk to permit admin to check the output
            List<String> validationErrors = validateCejshXml(new StreamSource(p0xml));

            if (validationErrors.isEmpty()) {
                writeZip(packageFolder, p.getStatus(), packageElm);
            } else {
                p.getStatus().error(packageElm, "Validation error!", ExportUtils.toString(validationErrors), null);
            }
        } catch (Exception ex) {
            p.getStatus().error(packageElm, "Unexpected error!", null, ex);
        }
        return packageFolder;
    }

    void writeZip(File pkgFile, File packageFolder) throws ZipException {
        ZipFile zipFile = new ZipFile(pkgFile);
        ZipParameters zipParameters = new ZipParameters();
        zipParameters.setEncryptionMethod(EncryptionMethod.ZIP_STANDARD);
        zipParameters.setCompressionMethod(CompressionMethod.DEFLATE);
        zipParameters.setCompressionLevel(CompressionLevel.NORMAL);
        zipParameters.setIncludeRootFolder(false);
        zipParameters.setDefaultFolderPath(packageFolder.getAbsolutePath());
        zipFile.addFiles(listZipFiles(packageFolder), zipParameters);
    }

    /**
     * Lists all files that should be part of a zip. Folder entries are not part of the list.
     * See issue #413.
     * @param folder a folder to scan
     * @return the list of files
     */
    private static ArrayList<File> listZipFiles(File folder) {
        ArrayList<File> files = new ArrayList<File>();
        ArrayList<File> subfiles = new ArrayList<File>();
        for (File file : folder.listFiles()) {
            if (file.isFile()) {
                files.add(file);
            } else if (file.isDirectory()) {
                subfiles.addAll(listZipFiles(file));
            }
        }
        files.addAll(subfiles);
        return files;
    }

    File writeZip(File packageFolder, CejshStatusHandler status, DigitalObjectElement packageElm) {
        String pkgName = packageFolder.getName();
        File pkgFile = new File(packageFolder.getParentFile(), pkgName + ".zip");
        try {
            writeZip(pkgFile, packageFolder);
            return pkgFile;
        } catch (ZipException ex) {
            status.error(packageElm, "Zipping error!", pkgFile.getPath(), ex);
            return null;
        }
    }

    static Schema getBwSchema() throws SAXException {
        if (SCHEMA_BWMETA == null) {
            SCHEMA_BWMETA = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
                    .newSchema(CejshBuilder.class.getResource(XSD_FILENAME));
        }
        return SCHEMA_BWMETA;
    }

    List<String> validateCejshXml(Source bwmeta) throws SAXException, IOException {
        if (bwValidator == null) {
            bwValidator = getBwSchema().newValidator();
            bwValidator.setErrorHandler(new ValidationErrorHandler());
        }
        List<String> errors = ((ValidationErrorHandler) bwValidator.getErrorHandler()).getValidationErrors();
        try {
            errors.clear();
            bwValidator.validate(bwmeta);
        } catch (SAXException ex) {
            errors.add(0, ex.getMessage());
        }
        return errors;
    }

    File writeProperties(File packageFolder, int articleCount) throws IOException, FileNotFoundException {
        File propertiesFile = new File(packageFolder, IMPORT_PROPERTIES_FILENAME);
        Properties properties = new Properties();
        gcalendar.setTimeInMillis(System.currentTimeMillis());
        String importDate = DatatypeConverter.printDateTime(gcalendar);
        properties.setProperty(PROP_IMPORT_INFODATE, importDate);
        properties.setProperty(PROP_IMPORT_OBJECTS, String.valueOf(articleCount));
        properties.setProperty(PROP_IMPORT_CONTENT_FILES, "0");
        properties.setProperty(PROP_IMPORT_BWMETA_FILES, "1");
        Writer propsWriter = new NoCommentsWriter(new OutputStreamWriter(new FileOutputStream(propertiesFile), Charsets.UTF_8));
        try {
            properties.store(propsWriter, null);
            return propertiesFile;
        } finally {
            propsWriter.close();
        }
    }

    String createPackageName() {
        //ISSN_publicationYear_volumePartNumber_issueParNumber
        String issn = getPackageIssn();
        String issueNumber = safeFilename(getIssue() == null ? null : getIssue().getIssueNumber());
        String volumeYear = safeFilename(getVolume().getYear());
        String volumeNumber = safeFilename(getVolume().getVolumeNumber());
        String pkgName = String.format("%s_%s_%s_%s", issn, volumeYear, volumeNumber, issueNumber);
        return pkgName;
    }

    static String safeFilename(String name) {
        if (name != null) {
            if (SAFE_FILENAME_RE == null) {
                // replace even '_' as it separates values in the result filename
                SAFE_FILENAME_RE = Pattern.compile("[^0-9a-zA-Z-.,;]");
            }
            SAFE_FILENAME_RE.matcher(name).replaceAll("");
        }
        return name == null || name.isEmpty() ? "NA" : name;
    }

    /**
     * Builds modsCollection from mods of articles.
     */
    Document mergeElements(List<Article> articles) throws DOMException {
        Document doc = db.newDocument();
        Element root = doc.createElementNS(ModsConstants.NS, "modsCollection");
        for (Article article : articles) {
            Element modsElm = article.getModsElement();
            Node n = doc.adoptNode(modsElm);
            root.appendChild(n);
        }
        doc.appendChild(root);
        return doc;
    }

    boolean addTitle(DigitalObjectElement current, DigitalObjectElement title, CejshContext p) {
        if (title == null) {
            p.getStatus().error(current, "Missing title object!", null);
            return false;
        }
        if (getTitle() != null) {
            p.getStatus().error(current, "Title inside title? " + title.toLog() + ". Child title: " + getTitle().getIssn(), null);
            return false;
        }
        Document modsDom = getModsDom(title, p);
        if (modsDom != null) {
            try {
                Title result = new Title();
                result.setIssn(getIssnPath().evaluate(modsDom));
                setTitle(result);
                return true;
            } catch (XPathExpressionException ex) {
                p.getStatus().error(current, "Invalid XPath!", ex);
                return false;
            }
        } else {
            p.getStatus().error(current, "Missing title MODS!" + title.toLog(), null);
            return false;
        }
    }

    boolean addVolume(DigitalObjectElement current, DigitalObjectElement volume, CejshContext p) {
        if (volume == null) {
            p.getStatus().error(current, "Missing volume object!", null);
            return false;
        }
        if (getVolume() != null) {
            p.getStatus().error(current, "Volume inside volume? " + volume.toLog() + ". Child volume: " + getVolume().getVolumeId(), null);
            return false;
        }
        Document modsDom = getModsDom(volume, p);
        if (modsDom != null) {
            try {
                Volume result = new Volume();
                result.setVolumeId(FoxmlUtils.pidAsUuid(volume.getPid()));
                result.setVolumeNumber(getPartNumberPath().evaluate(modsDom));
                if (result.getVolumeNumber() == null || result.getVolumeNumber().isEmpty()) {
                    result.setVolumeNumber("NA");
                }
                result.setYear(getDateIssuedPath().evaluate(modsDom));
                if (result.getYear() == null || result.getYear().isEmpty()) {
                    result.setYear("NA");
                }
                setVolume(result);
                return true;
            } catch (XPathExpressionException ex) {
                p.getStatus().error(current, "Invalid XPath!", ex);
                return false;
            }
        } else {
            p.getStatus().error(current, "Missing volume MODS!" + volume.toLog(), null);
            return false;
        }
    }

    boolean addIssue(DigitalObjectElement current, DigitalObjectElement issue, CejshContext p) {
        if (issue == null) {
            p.getStatus().error(current, "Missing issue object!", null);
            return false;
        }
        if (getIssue() != null) {
            p.getStatus().error(current, "Issue inside issue? " + issue.toLog() + ". Child issue: " + getIssue().getIssueId(), null);
            return false;
        }
        Document modsDom = getModsDom(issue, p);
        if (modsDom != null) {
            try {
                Issue result = new Issue();
                result.setIssueId(FoxmlUtils.pidAsUuid(issue.getPid()));
                result.setIssueNumber(getPartNumberPath().evaluate(modsDom));
                result.setDateIssued(getDateIssuedPath().evaluate(modsDom));
                if (result.getIssueNumber() == null || result.getIssueNumber().isEmpty()) {
                    result.setIssueNumber("NA");
                }
                result.setIssn(getIssnPath().evaluate(modsDom));
                setIssue(result);
                return true;
            } catch (XPathExpressionException ex) {
                p.getStatus().error(current, "Invalid XPath!", ex);
                return false;
            }
        } else {
            p.getStatus().error(current, "Missing issue MODS!" + issue.toLog(), null);
            return false;
        }
    }

    public Title getTitle() {
        return title;
    }

    public void setTitle(Title title) {
        this.title = title;
    }

    public Volume getVolume() {
        return volume;
    }

    public void setVolume(Volume volume) {
        LOG.log(logLevel, String.valueOf(volume));
        this.volume = volume;
    }

    public Issue getIssue() {
        return issue;
    }

    public void setIssue(Issue issue) {
        LOG.log(logLevel, String.valueOf(issue));
        this.issue = issue;
    }

    private Document getModsDom(DigitalObjectElement elm, CejshContext p) {
        try {
            MetadataHandler<?> metadataHandler = elm.getHandler().metadata();
            DescriptionMetadata<String> dm = metadataHandler.getMetadataAsXml();
            String mods = dm.getData();
            Document modsDom = db.parse(new InputSource(new StringReader(mods)));
            return modsDom;
        } catch (DigitalObjectException ex) {
            p.getStatus().error(elm, "Missing MODS!", ex);
            return null;
        } catch (SAXException ex) {
            p.getStatus().error(elm, "Invalid MODS!", ex);
            return null;
        } catch (IOException ex) {
            p.getStatus().error(elm, null, ex);
            return null;
        }
    }

    DocumentBuilder getDocumentBuilder() {
        return db;
    }

    List<String> getTranformationErrors() {
        return tranformationErrorHandler.getErrors();
    }

    static class Title {
        private String issn;
//        private String year;

        public String getIssn() {
            return issn;
        }

        public void setIssn(String issn) {
            this.issn = issn;
        }

        @Override
        public String toString() {
            return "Title{" + "issn=" + issn + '}';
        }
    }

    static class Volume {

        private String year;
        private String volumeNumber;
        private String volumeId;

        public String getYear() {
            return year;
        }

        public void setYear(String year) {
            this.year = year;
        }

        public String getVolumeNumber() {
            return volumeNumber;
        }

        public void setVolumeNumber(String volumeNumber) {
            this.volumeNumber = volumeNumber;
        }

        public String getVolumeId() {
            return volumeId;
        }

        public void setVolumeId(String volumeId) {
            this.volumeId = volumeId;
        }

        @Override
        public String toString() {
            return "Volume{" + "year=" + year + ", volumeNumber=" + volumeNumber + ", volumeId=" + volumeId + '}';
        }
    }

    static class Issue {

        private String issueNumber;
        private String issueId;
        private String issn;
        private String dateIssued;

        public String getDateIssued() {
            return dateIssued;
        }

        public void setDateIssued(String dateIssued) {
            this.dateIssued = dateIssued;
        }

        public String getIssueNumber() {
            return issueNumber;
        }

        public void setIssueNumber(String issueNumber) {
            this.issueNumber = issueNumber;
        }

        public String getIssueId() {
            return issueId;
        }

        public void setIssueId(String issueId) {
            this.issueId = issueId;
        }

        public String getIssn() {
            return issn;
        }

        public void setIssn(String issn) {
            this.issn = issn;
        }

        @Override
        public String toString() {
            return "Issue{" + "issueNumber=" + issueNumber + ", issueId=" + issueId + ", issn=" + issn + '}';
        }

    }

    static class Article {

        private Element mods;
        private DigitalObjectElement article;
        private String issn;
        private boolean reviewed;
        private boolean hasEnglishAbstract;

        public Article() {
        }

        public Article(DigitalObjectElement article, Element mods, String issn) {
            this.mods = mods;
            this.article = article;
            this.issn = issn;
        }

        public DigitalObjectElement getDigitalObject() {
            return article;
        }

        public Element getModsElement() {
            return mods;
        }

        public void setModsElement(Element mods) {
            this.mods = mods;
        }

        public String getIssn() {
            return issn;
        }

        public void setIssn(String issn) {
            this.issn = issn;
        }

        public boolean isReviewed() {
            return reviewed;
        }

        public Article setReviewed(boolean reviewed) {
            this.reviewed = reviewed;
            return this;
        }

        public boolean hasEnglishAbstract() {
            return hasEnglishAbstract;
        }

        public Article setEnglishAbstract(boolean hasEnglishAbstract) {
            this.hasEnglishAbstract = hasEnglishAbstract;
            return this;
        }

        public static List<DigitalObjectElement> toDigitalObjects(List<Article> articles) {
            ArrayList<DigitalObjectElement> dobjs = new ArrayList<DigitalObjectElement>(articles.size());
            for (Article article : articles) {
                dobjs.add(article.getDigitalObject());
            }
            return dobjs;
        }
    }

    /**
     * Filters out default line comment with localized date. The implementation
     * heavily depends on {@link Properties#store} and {@link BufferedWriter}
     * implementations.
     */
    private static class NoCommentsWriter extends BufferedWriter {

        private static final String LINE_SEPARATOR = System.getProperty("line.separator");
        private boolean ignoreNextNewLine;
        private final String expextedDateFragment;

        public NoCommentsWriter(Writer out) {
            super(out);
            expextedDateFragment = '#' + new Date().toString().substring(0, 10);
        }

        @Override
        public void write(String str) throws IOException {
            if (ignoreNextNewLine && LINE_SEPARATOR.equals(str)) {
                ignoreNextNewLine = false;
                return;
            }
            if (str.length() > 0 && str.startsWith(expextedDateFragment)) {
                ignoreNextNewLine = true;
                return;
            }
            super.write(str);
        }
    }

}
