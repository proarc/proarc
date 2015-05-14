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
package cz.cas.lib.proarc.common.export.cejsh;

import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.export.mets.ValidationErrorHandler;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import cz.cas.lib.proarc.mods.ModsDefinition;
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
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import net.lingala.zip4j.core.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.ZipParameters;
import net.lingala.zip4j.util.Zip4jConstants;
import org.apache.commons.io.Charsets;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

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

    private final Transformer bwmetaXsl;
    private final Transformer mods2cejsh;
    private final Transformer mods2cejshHead;
    private final TranformationErrorHandler tranformationErrorHandler;
    private Validator bwValidator;
    private final DocumentBuilder db;
    private final XPathExpression issnPath;
    private final XPathExpression partNumberPath;
    private final XPathExpression dateIssuedPath;
    private final GregorianCalendar gcalendar;
    private Title title;
    private Volume volume;
    private Issue issue;
    private Level logLevel;

    public CejshBuilder(CejshConfig config) throws TransformerConfigurationException, ParserConfigurationException, XPathExpressionException {
        this.gcalendar = new GregorianCalendar(UTC);
        this.logLevel = config.getLogLevel();
        TransformerFactory xslFactory = TransformerFactory.newInstance();
        mods2cejsh = xslFactory.newTransformer(new StreamSource(config.getXslCejshUrl()));
        if (mods2cejsh == null) {
            throw new TransformerConfigurationException("Cannot load XSL: " + config.getXslCejshUrl());
        }
        mods2cejshHead = xslFactory.newTransformer(new StreamSource(config.getXslCejshHeadUrl()));
        if (mods2cejshHead == null) {
            throw new TransformerConfigurationException("Cannot load cejsh head XSL: " + config.getXslCejshHeadUrl());
        }
        tranformationErrorHandler = new TranformationErrorHandler();
        mods2cejsh.setErrorListener(tranformationErrorHandler);
        mods2cejshHead.setErrorListener(tranformationErrorHandler);
        bwmetaXsl = xslFactory.newTransformer();
        bwmetaXsl.setOutputProperty(OutputKeys.INDENT, "yes");
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        db = dbf.newDocumentBuilder();
        XPathFactory xPathFactory = ProarcXmlUtils.defaultXPathFactory();
        XPath xpath = xPathFactory.newXPath();
        xpath.setNamespaceContext(new SimpleNamespaceContext().add("m", ModsConstants.NS));
        issnPath = xpath.compile("m:mods/m:identifier[@type='issn' and not(@invalid)]");
        partNumberPath = xpath.compile("m:mods/m:titleInfo/m:partNumber");
        dateIssuedPath = xpath.compile("m:mods/m:originInfo/m:dateIssued");
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

    public Article addArticle(DigitalObjectElement article, CejshContext p) {
        Document articleDom = getModsDom(article, p);
        return addArticle(articleDom, article, p);
    }

    Article addArticle(Document articleDom, DigitalObjectElement article, CejshContext p) {
        if (articleDom == null) {
            p.getStatus().error(article, "Missing article MODS!", null, null);
            return null;
        }
        if (getTitle() == null) {
            p.getStatus().error(article, "Missing title!", null, null);
            return null;
        }
        if (getVolume() == null) {
            p.getStatus().error(article, "Missing volume!", null, null);
            return null;
        }
        if (getIssue() == null) {
            p.getStatus().error(article, "Missing issue!", null, null);
            return null;
        }
        mods2cejsh.reset();
        mods2cejshHead.reset();
        try {
            String articleIssn = getIssnPath().evaluate(articleDom);
            String packageIssn = getPackageIssn();
            Element cejshDom = createCejshElement(packageIssn, getIssue().getIssueId(), articleDom);
            if (cejshDom == null) {
                return null;
            }
            Element cejshHeadDom = createCejshHeadElement(packageIssn,
                    getVolume().getVolumeNumber(), getVolume().getVolumeId(), getVolume().getYear(),
                    getIssue().getIssueNumber(), getIssue().getIssueId(), articleDom);
            if (cejshHeadDom == null) {
                p.getStatus().error(article, "Validation error!", tranformationErrorHandler.getErrors().toString(), null);
                return null;
            }
            return new Article(article, cejshDom, cejshHeadDom, articleIssn);
        } catch (TransformerException ex) {
            p.getStatus().error(article, "Validation error!", getTranformationErrors().toString(), ex);
        } catch (Exception ex) {
            p.getStatus().error(article, "Unexpected error!", null, ex);
        }
        return null;
    }

    String getPackageIssn() {
        String issn = getIssue().getIssn();
        if (issn == null || issn.isEmpty()) {
            issn = getTitle().getIssn();
            if (issn == null || issn.isEmpty()) {
                issn = "NA";
            }
        }
        return issn;
    }

    Element createCejshHeadElement(String issn, String volume, String volumeId, String year,
            String issue, String issueId, Document articleDom) throws TransformerException {

        mods2cejshHead.setParameter("issn", issn);
        mods2cejshHead.setParameter("volume", volume);
        mods2cejshHead.setParameter("volumeId", volumeId);
        mods2cejshHead.setParameter("issue", issue);
        mods2cejshHead.setParameter("issueId", issueId);
        mods2cejshHead.setParameter("year", year);
        DOMResult cejshHeadDom = new DOMResult();
        tranformationErrorHandler.reset();
        mods2cejshHead.transform(new DOMSource(articleDom), cejshHeadDom);
        if (!tranformationErrorHandler.getErrors().isEmpty()) {
            return null;
        }
        Node resultNode = cejshHeadDom.getNode();
        return resultNode instanceof Document ? ((Document) resultNode).getDocumentElement() : null;
    }

    Element createCejshElement(String issn, String issueId, Document articleDom) throws TransformerException {
        mods2cejsh.setParameter("issn", issn);
        mods2cejsh.setParameter("issueId", issueId);
        DOMResult cejshDom = new DOMResult();
        tranformationErrorHandler.reset();
        mods2cejsh.transform(new DOMSource(articleDom), cejshDom);
        if (!tranformationErrorHandler.getErrors().isEmpty()) {
            return null;
        }
        Node resultNode = cejshDom.getNode();
        return resultNode instanceof Document ? ((Document) resultNode).getDocumentElement() : null;
    }

    public void writePackage(DigitalObjectElement packageElm, List<Article> articles, CejshContext p) {
        if (articles == null || articles.isEmpty()) {
            return;
        }
        try {
            Document doc = mergeElements(articles);

            String pkgName = createPackageName();
            File packageFolder = ExportUtils.createFolder(p.getOutput(), pkgName);
            pkgName = packageFolder.getName();
            File importFolder = new File(packageFolder, IMPORTS_NEW_FILENAME);
            importFolder.mkdirs();
            writeProperties(packageFolder, articles.size());

            File p0xml = new File(importFolder, P0XML_FILENAME);
            DOMSource domSource = new DOMSource(doc);
            bwmetaXsl.transform(domSource, new StreamResult(p0xml));
            // validate after writing to disk to permit to check the output
            List<String> validateErrors = validatePackage(domSource);
            if (validateErrors.isEmpty()) {
                writeZip(packageFolder, p.getStatus(), packageElm);
            } else {
                p.getStatus().error(packageElm, "Validation error!", ExportUtils.toString(validateErrors), null);
            }
        } catch (TransformerException ex) {
            p.getStatus().error(packageElm, "Transform error!", null, ex);
        } catch (Exception ex) {
            p.getStatus().error(packageElm, "Unexpected error!", null, ex);
        } finally {
        }
    }

    void writeZip(File pkgFile, File packageFolder) throws ZipException {
        ZipFile zipFile = new ZipFile(pkgFile);
        ZipParameters zipParameters = new ZipParameters();
        zipParameters.setEncryptionMethod(Zip4jConstants.ENC_METHOD_STANDARD);
        zipParameters.setCompressionMethod(Zip4jConstants.COMP_DEFLATE);
        zipParameters.setCompressionLevel(Zip4jConstants.DEFLATE_LEVEL_NORMAL);
        zipParameters.setIncludeRootFolder(false);
        zipFile.addFolder(packageFolder, zipParameters);
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

    List<String> validatePackage(Source bwmeta) throws SAXException, IOException {
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
        String issueNumber = safeFilename(getIssue().getIssueNumber());
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

    Document mergeElements(List<Article> articles) throws DOMException {
        Document doc = db.newDocument();
        Element root = doc.createElementNS(NS_BWMETA105, "bwmeta");
        for (Article article : articles) {
            Element cejshHead = article.getCejshHead();
            merge(doc, root, cejshHead);
        }
        for (Article article : articles) {
            Element cejsh = article.getCejsh();
            merge(doc, root, cejsh);
        }
        doc.appendChild(root);
        return doc;
    }

    void merge(Document doc, Element root, Element bwmeta) {
        NodeList nodes = bwmeta.getElementsByTagNameNS(NS_BWMETA105, "element");
        for (int i = 0; i < nodes.getLength(); i++) {
            Node n = doc.adoptNode(nodes.item(i));
            root.appendChild(n);
        }
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
        LOG.log(logLevel, String.valueOf(title));
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

    private ModsDefinition getMods(DigitalObjectElement elm, CejshContext p) {
        try {
            MetadataHandler<ModsDefinition> metadataHandler = elm.getHandler().metadata();
            DescriptionMetadata<ModsDefinition> dm = metadataHandler.getMetadata();
            ModsDefinition mods = dm.getData();
            return mods;
        } catch (DigitalObjectException ex) {
            p.getStatus().error(elm, "Missing MODS!", ex);
            return null;
        }
    }

    private Document getModsDom(DigitalObjectElement elm, CejshContext p) {
        try {
            MetadataHandler<ModsDefinition> metadataHandler = elm.getHandler().metadata();
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
//        private String year;

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

        private Element cejsh;
        private Element cejshHead;
        private DigitalObjectElement article;
        private String issn;

        public Article() {
        }

        public Article(DigitalObjectElement article, Element cejsh, Element cejshHead, String issn) {
            this.cejsh = cejsh;
            this.cejshHead = cejshHead;
            this.article = article;
            this.issn = issn;
        }

        public DigitalObjectElement getDigitalObject() {
            return article;
        }

        public Element getCejsh() {
            return cejsh;
        }

        public void setCejsh(Element cejsh) {
            this.cejsh = cejsh;
        }

        public Element getCejshHead() {
            return cejshHead;
        }

        public void setCejshHead(Element cejshHead) {
            this.cejshHead = cejshHead;
        }

        public String getIssn() {
            return issn;
        }

        public void setIssn(String issn) {
            this.issn = issn;
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

    /**
     * Collects transformation errors. Reset the handler before each transformation.
     */
    private static class TranformationErrorHandler implements ErrorListener {

        private final List<String> errors;

        public TranformationErrorHandler() {
            this.errors = new ArrayList<String>();
        }

        public void reset() {
            errors.clear();
        }

        public List<String> getErrors() {
            return errors;
        }

        @Override
        public void warning(TransformerException exception) throws TransformerException {
            errors.add(exception.getMessageAndLocation());
        }

        @Override
        public void error(TransformerException exception) throws TransformerException {
            errors.add(exception.getMessageAndLocation());
        }

        @Override
        public void fatalError(TransformerException exception) throws TransformerException {
            throw exception;
        }
    }

}
