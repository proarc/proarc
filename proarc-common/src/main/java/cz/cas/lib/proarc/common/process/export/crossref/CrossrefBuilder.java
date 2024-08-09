/*
 * Copyright (C) 2016 Jan Pokorsky
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
package cz.cas.lib.proarc.common.process.export.crossref;

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.process.export.ExportException;
import cz.cas.lib.proarc.common.process.export.ExportParams;
import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.process.export.mets.ValidationErrorHandler;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;
import cz.cas.lib.proarc.common.xml.SimpleLSResourceResolver;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import cz.cas.lib.proarc.common.xml.TransformErrorListener;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.NamePartDefinition;
import cz.cas.lib.proarc.mods.RoleTermDefinition;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
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
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Builds CrossRef XML files inside the output folder.
 *
 * @author Jan Pokorsky
 */
class CrossrefBuilder {

    private static Schema SCHEMA_CROSSREF;
    private static final String XSD_FILENAME = "crossref5.3.1.xsd";

    private final File outputFolder;
    private final XPathExpression issnPath;
    private final XPathExpression partNumberPath;
    private final XPathExpression dateIssuedPath;
    private final XPathExpression physicalFormPath;
    private final XPathExpression abbrevTitlePath;
    private final XPathExpression doiIdentifierPath;
    private final XPathExpression doiUrlPath;
    private final DocumentBuilder db;
    private final Transformer crosssrefXsl;
    private final TransformErrorListener tranformationErrorHandler;
    private final List<Document> articles = new ArrayList<Document>();
    private final Map<DigitalObjectElement, Document> docCache
            = new HashMap<DigitalObjectElement, Document>();
    private final XPathExpression titlePath;
    private final SimpleDateFormat exportDateFormat;
    private Validator crossrefValidator;
    private int pkgIndex;
    private ExportParams options;

    private static final Logger LOG = Logger.getLogger(CrossrefBuilder.class.getName());

    public CrossrefBuilder(File outputFolder, ExportParams options)
            throws Exception {
        this.options = options;
        this.outputFolder = outputFolder;
        this.exportDateFormat = new SimpleDateFormat("yyyyMMddHHmm");

        XPathFactory xPathFactory = ProarcXmlUtils.defaultXPathFactory();
        XPath xpath = xPathFactory.newXPath();
        xpath.setNamespaceContext(new SimpleNamespaceContext().add("m", ModsConstants.NS));
        issnPath = xpath.compile("m:mods/m:identifier[@type='issn' and not(@invalid)]");
        partNumberPath = xpath.compile("m:mods/m:titleInfo/m:partNumber");
        abbrevTitlePath = xpath.compile("m:mods/m:titleInfo/m:title[@type='abbreviated']");
        doiIdentifierPath = xpath.compile("m:mods/m:identifier[@type='doi']");
        doiUrlPath = xpath.compile("m:mods/m:identifier[@type='doi']/@typeURI");
        titlePath = xpath.compile("m:mods/m:titleInfo/m:title");
        dateIssuedPath = xpath.compile("m:mods/m:originInfo/m:dateIssued");
        physicalFormPath = xpath.compile("m:mods/m:physicalDescription/m:form");

        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        db = dbf.newDocumentBuilder();

        TransformerFactory xslFactory = TransformerFactory.newInstance();
        tranformationErrorHandler = new TransformErrorListener();
        crosssrefXsl = xslFactory.newTransformer(new StreamSource(
                CrossrefBuilder.class.getResource("mods_crossref.xsl").toExternalForm()));
        if (crosssrefXsl == null) {
            throw new TransformerConfigurationException("Cannot load XSL: " + "mods_crossref.xsl");
        }
        crosssrefXsl.setOutputProperty(OutputKeys.INDENT, "yes");
        crosssrefXsl.setErrorListener(tranformationErrorHandler);
        if (options.getJournalsInfoPath() == null || options.getJournalsInfoPath().length() == 0) {
            throw new Exception("Not configurated path : \"export.cejsh_crossref.journals.path=\".");
        }
        crosssrefXsl.setParameter("journalsInfo", options.getJournalsInfoPath());
    }

    public File createPackage(CrossrefPackage pkg) throws ExportException {
        crosssrefXsl.reset();
        articles.clear();
        File packageFile = null;
        DigitalObjectElement pkgElm = pkg.getPath().get(0);
        try {
            String pkgName = createPackageName(pkg);
            packageFile = new File(outputFolder, pkgName);

            processPath(pkg);
            List<DigitalObjectElement> articleElms = pkg.getArticles();
            for (DigitalObjectElement articleElm : articleElms) {
                Document articleMods = getModsDom(articleElm, false);
                if (articleMods != null) {
                    addArticle(articleMods);
                } else {
                    return null;
                }
            }
            Document mergedArticles = mergeArticles();
            TransformErrorListener errors = createCrossrefXml(
                    new DOMSource(mergedArticles), new StreamResult(packageFile));
            if (!errors.getErrors().isEmpty()) {
                throw new ExportException(pkgElm, "Transformation errors!",
                        errors.getErrors().toString(), null);
            }

            updatePackageFile(packageFile);

            List<String> validationErrors = validateCrossref(new StreamSource(packageFile));
            if (!validationErrors.isEmpty()) {
                throw new ExportException(pkgElm, "Validation errors!",
                        ExportUtils.toString(validationErrors), null);
            }
        } catch (ExportException ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ExportException(pkgElm, "Unexpected error!", null, ex);
        }
        return packageFile;
    }

    private void updatePackageFile(File packageFile) {
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(packageFile), StandardCharsets.UTF_8));
            StringBuffer buffer = new StringBuffer();
            String line = reader.readLine();
            while(line != null) {
                line = line.replaceAll("&lt;", "<");
                line = line.replaceAll("&gt;", ">");
                buffer.append(line);
                line = reader.readLine();
            }
            reader.close();
            Writer writer = new OutputStreamWriter(new FileOutputStream(packageFile), StandardCharsets.UTF_8);
            writer.append(buffer.toString());
            writer.close();
        } catch (Exception ex) {
            LOG.log(java.util.logging.Level.SEVERE, "Error with updating file", ex);
        }
    }

    /**
     * Transforms a collection of articles to the Crossref document.
     * @param src modsCollection in MODS format
     * @param dst Crossref document
     * @return the error handler
     */
    TransformErrorListener createCrossrefXml(Source src, Result dst) {
        try {
            tranformationErrorHandler.reset();
            crosssrefXsl.setParameter("export_time", exportDateFormat.format(new Date()));
            crosssrefXsl.transform(src, dst);
        } catch (TransformerException ex) {
            if (tranformationErrorHandler.getErrors().isEmpty()) {
                tranformationErrorHandler.getErrors().add(ex.getMessageAndLocation());
            }
        }
        return tranformationErrorHandler;
    }

    private void processPath(CrossrefPackage pkg) throws XPathExpressionException, ExportException, ParserConfigurationException, IOException, SAXException {
        List<DigitalObjectElement> path = pkg.getPath();
        Iterator<DigitalObjectElement> it = path.iterator();
        if (!it.hasNext()) {
            throw new ExportException(null, "No parent!", null, new IllegalStateException());
        }
        processIssue(it, pkg);
    }

    private static DigitalObjectElement readNext(
            Iterator<DigitalObjectElement> path, DigitalObjectElement last
    ) throws ExportException {
        if (path.hasNext()) {
            return path.next();
        }
        throw new ExportException(last, "No parent!", null, null);
    }

    private void processIssue(
            Iterator<DigitalObjectElement> path, CrossrefPackage pkg
    ) throws XPathExpressionException, ExportException, ParserConfigurationException, IOException, SAXException {
        DigitalObjectElement elm = path.next();
        if (NdkPlugin.MODEL_PERIODICALISSUE.equals(elm.getModelId())) {
            addIssue(elm);
            DigitalObjectElement parent = readNext(path, elm);
            if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(parent.getModelId())) {
                addVolume(getModsDom(parent));
                parent = readNext(path, parent);
                if (NdkPlugin.MODEL_PERIODICAL.equals(parent.getModelId())) {
                    addPeriodicalTitle(getModsDom(parent), parent.getPid());
                } else {
                    throw new ExportException(parent, "Expected a periodical title!", null, null);
                }
            } else if (NdkPlugin.MODEL_PERIODICAL.equals(parent.getModelId())) {
                addPeriodicalTitle(getModsDom(parent), parent.getPid());
            } else {
                throw new ExportException(parent, "Expected a periodical volume!", null, null);
            }
        } else if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(elm.getModelId())) {
            addVolumeWithoutIssue(getModsDom(elm), elm.getPid());
            DigitalObjectElement parent = readNext(path, elm);
            if (NdkPlugin.MODEL_PERIODICAL.equals(parent.getModelId())) {
                addPeriodicalTitle(getModsDom(parent), parent.getPid());
            } else {
                throw new ExportException(parent, "Expected a periodical title!", null, null);
            }
        } else {
            throw new ExportException(elm, "Expected a periodical issue or volume!", null, null);
        }
    }

    boolean addPeriodicalTitle(String issn, String title, String abbrevTitle, String media, String doi, String doiUrl, String titlePid) throws XPathExpressionException {
        crosssrefXsl.setParameter("issn", issn);
        crosssrefXsl.setParameter("abbrev_title", abbrevTitle);
        crosssrefXsl.setParameter("full_title", title);
        crosssrefXsl.setParameter("media_type", media);
        crosssrefXsl.setParameter("uuid_title", titlePid.substring(5));
        crosssrefXsl.setParameter("doi_title", doi);
        if (doiUrl != null) {
            crosssrefXsl.setParameter("doi_title_url", doiUrl);
        }
        return true;
    }

    private boolean addPeriodicalTitle(Document d, String titlePid) throws XPathExpressionException {
        // title issn - mods/identifier[type="issn"][0]
        // title - mods/titleInfo/title
        // abbreviated title - mods/titleInfo/title/@type="abbreviated"
        // physical form - mods/physicalDescription/form
        String issn = issnPath.evaluate(d);
        String abbrevTitle = abbrevTitlePath.evaluate(d);
        String title = titlePath.evaluate(d);
        String form = physicalFormPath.evaluate(d);
        String doi = doiIdentifierPath.evaluate(d);
        String doiUrl = doiUrlPath.evaluate(d);
        return addPeriodicalTitle(issn, title, abbrevTitle, form, doi, doiUrl, titlePid);
    }

    boolean addVolume(String partNumber, String dateIssued, String uuid) throws XPathExpressionException {
        crosssrefXsl.setParameter("volume", partNumber);
        if (dateIssued != null) {
            crosssrefXsl.setParameter("publication_date", dateIssued);
        }
        if (uuid != null) {
            crosssrefXsl.setParameter("export_uuid", uuid);
        }
        return true;
    }

    private void addVolume(Document d) throws XPathExpressionException, ExportException {
        // volume number - mods/titleInfo/partNumber
        String partNumber = partNumberPath.evaluate(d);
        addVolume(partNumber, null, null);
    }

    private void addVolumeWithoutIssue(Document d, String pid) throws XPathExpressionException, ExportException {
        // volume number - mods/titleInfo/partNumber
        // issue date - mods/originInfo/dateIssued
        String partNumber = partNumberPath.evaluate(d);
        String dateIssued = dateIssuedPath.evaluate(d);
        addVolume(partNumber, dateIssued, FoxmlUtils.pidAsUuid(pid));
    }

    boolean addIssue(String partNumber, String dateIssued, String issueUuid, String doi, String doiUrl, String contibutors) throws XPathExpressionException, ParserConfigurationException, IOException, SAXException {
        crosssrefXsl.setParameter("issue", partNumber);
        crosssrefXsl.setParameter("publication_date", dateIssued);
        crosssrefXsl.setParameter("export_uuid", issueUuid);
        crosssrefXsl.setParameter("doi_issue", doi);
        crosssrefXsl.setParameter("uuid_issue", issueUuid.substring(5));
        if (doiUrl != null) {
            crosssrefXsl.setParameter("doi_issue_url", doiUrl);
        }
        if (contibutors != null) {
            crosssrefXsl.setParameter("contributors", contibutors);
        }
        return true;
    }

    private void addIssue(DigitalObjectElement elm) throws XPathExpressionException, ParserConfigurationException, IOException, SAXException, ExportException {
        // issue number - mods/titleInfo/partNumber
        // issue date - mods/originInfo/dateIssued
        Document d = getModsDom(elm);
        String partNumber = partNumberPath.evaluate(d);
        String dateIssued = dateIssuedPath.evaluate(d);
        String doi = doiIdentifierPath.evaluate(d);
        String doiUrl = doiUrlPath.evaluate(d);
        String contributors = createContributors(getMods(elm));
        addIssue(partNumber, dateIssued, FoxmlUtils.pidAsUuid(elm.getPid()), doi, doiUrl, contributors);
    }

    private String createContributors(ModsDefinition mods) {
        StringBuilder builder = new StringBuilder();
        int count = 1;
        for (NameDefinition name : mods.getName()) {
            if (name.getRole() != null && !name.getRole().isEmpty()
                    && name.getRole().get(0).getRoleTerm() != null && !name.getRole().get(0).getRoleTerm().isEmpty()
                    && hasAcceptedRoleTerm(name.getRole().get(0).getRoleTerm().get(0))
                    && name.getNamePart() != null && !name.getNamePart().isEmpty()) {
                if ("corporate".equals(name.getType())) {
                    builder.append("<organization");
                    if (count == 1) {
                        builder.append(" sequence=\"first\"");
                    } else {
                        builder.append(" sequence=\"additional\"");
                    }

                    if ("aut".equals(name.getRole().get(0).getRoleTerm().get(0).getValue())
                            || "rev".equals(name.getRole().get(0).getRoleTerm().get(0).getValue())) {
                        builder.append(" contributor_role=\"author\"");
                    } else if ("edt".equals(name.getRole().get(0).getRoleTerm().get(0).getValue())) {
                        builder.append(" contributor_role=\"editor\"");
                    } else if ("trl".equals(name.getRole().get(0).getRoleTerm().get(0).getValue())) {
                        builder.append(" contributor_role=\"translator\"");
                    }
                    builder.append(">");

                    for (NamePartDefinition namePart : name.getNamePart()) {
                        builder.append(namePart.getValue());
                        break;
                    }
                    builder.append("</organization>");
                    count++;
                }
            }
        }

        for (NameDefinition name : mods.getName()) {
            if (name.getRole() != null && !name.getRole().isEmpty()
                    && name.getRole().get(0).getRoleTerm() != null && !name.getRole().get(0).getRoleTerm().isEmpty()
                    && hasAcceptedRoleTerm(name.getRole().get(0).getRoleTerm().get(0))
                    && name.getNamePart() != null && !name.getNamePart().isEmpty()) {
                if ("personal".equals(name.getType())) {
                    builder.append("<person_name");
                    if (count == 1) {
                        builder.append(" sequence=\"first\"");
                    } else {
                        builder.append(" sequence=\"additional\"");
                    }

                    if ("aut".equals(name.getRole().get(0).getRoleTerm().get(0).getValue())
                            || "rev".equals(name.getRole().get(0).getRoleTerm().get(0).getValue())) {
                        builder.append(" contributor_role=\"author\"");
                    } else if ("edt".equals(name.getRole().get(0).getRoleTerm().get(0).getValue())) {
                        builder.append(" contributor_role=\"editor\"");
                    } else if ("trl".equals(name.getRole().get(0).getRoleTerm().get(0).getValue())) {
                        builder.append(" contributor_role=\"translator\"");
                    }
                    builder.append(">");

                    for (NamePartDefinition namePart : name.getNamePart()) {
                        if (namePart.getType() != null && "given".equals(namePart.getType())) {
                            builder.append("<given_name>").append(namePart.getValue()).append("</given_name>");
                        }
                    }

                    for (NamePartDefinition namePart : name.getNamePart()) {
                        if (namePart.getType() == null || "family".equals(namePart.getType())) {
                            builder.append("<surname>").append(namePart.getValue()).append("</surname>");
                        }
                    }

                    if (name.getAffiliation() != null && !name.getAffiliation().isEmpty()
                            && name.getAffiliation().get(0).getValue() != null && !name.getAffiliation().get(0).getValue().isEmpty()) {
                        builder.append("<affiliations>").append("<institution>").append("<institution_id type=\"ror\">")
                                .append("https://ror.org/").append(name.getAffiliation().get(0).getValue())
                                .append("</institution_id>").append("</institution>").append("</affiliations>");
                    }
                    if (name.getNameIdentifier() != null && !name.getNameIdentifier().isEmpty()
                            && name.getNameIdentifier().get(0).getValue() != null && !name.getNameIdentifier().get(0).getValue().isEmpty()
                            && "orcid".equals(name.getNameIdentifier().get(0).getType())) {
                        builder.append("<ORCID authenticated=\"true\">")
                                .append("https://orcid.org/").append(name.getNameIdentifier().get(0).getValue())
                                .append("</ORCID>");
                    }

                    builder.append("</person_name>");
                    count++;
                }
            }
        }
        return builder.toString().isEmpty() ? null : builder.toString();
    }

    private boolean hasAcceptedRoleTerm(RoleTermDefinition roleTerm) {
        if (CodeOrText.CODE.equals(roleTerm.getType())) {
            return "aut".equals(roleTerm.getValue()) || "rev".equals(roleTerm.getValue()) || "edt".equals(roleTerm.getValue()) || "trl".equals(roleTerm.getValue());
        } else {
            return false;
        }
    }

    void addArticle(Document d) {
        articles.add(d);
    }

    Document mergeArticles() throws DOMException {
        Document doc = db.newDocument();
        Element root = doc.createElementNS(ModsConstants.NS, "modsCollection");
        for (Document article : articles) {
            Element modsElm = article.getDocumentElement();
            Node n = doc.adoptNode(modsElm);
            root.appendChild(n);
        }
        doc.appendChild(root);
        return doc;
    }

    String createPackageName(CrossrefPackage pkg) {
        String pkgName = String.format("batch_%04d.xml", pkgIndex++);
        return pkgName;
    }

    private Document getModsDom(DigitalObjectElement elm) throws ExportException {
        return getModsDom(elm, true);
    }

    private Document getModsDom(DigitalObjectElement elm, boolean useCache) throws ExportException {
        Document d = useCache ? docCache.get(elm) : null;
        if (d != null) {
            return d;
        }
        try {
            MetadataHandler<?> metadataHandler = elm.getHandler().metadata();
            DescriptionMetadata<String> dm = metadataHandler.getMetadataAsXml();
            String mods = dm.getData();
            Document modsDom = db.parse(new InputSource(new StringReader(mods)));
            if (useCache) {
                docCache.put(elm, d);
            }
            return modsDom;
        } catch (DigitalObjectException ex) {
            throw new ExportException(elm, "Missing MODS!", null, ex);
        } catch (SAXException ex) {
            throw new ExportException(elm, "Invalid MODS!", null, ex);
        } catch (IOException ex) {
            throw new ExportException(elm, "IO error!", null, ex);
        }
    }

    private ModsDefinition getMods(DigitalObjectElement elm) {
        try {
            DescriptionMetadata<ModsDefinition> dm = elm.getHandler().<ModsDefinition>metadata().getMetadata();
            ModsDefinition mods = dm.getData();
            return mods;
        } catch (DigitalObjectException e) {
            throw new RuntimeException(e);
        }
    }

    DocumentBuilder getDocumentBuilder() {
        return db;
    }

    /**
     * Validates a package against CrossRef schemas.
     */
    List<String> validateCrossref(Source crossref) throws SAXException, IOException {
        if (crossrefValidator == null) {
            crossrefValidator = getCrossrefSchema().newValidator();
            crossrefValidator.setErrorHandler(new ValidationErrorHandler());
        }
        List<String> errors = ((ValidationErrorHandler) crossrefValidator.getErrorHandler()).getValidationErrors();
        try {
            errors.clear();
            crossrefValidator.validate(crossref);
        } catch (SAXException ex) {
            errors.add(0, ex.getMessage());
        }
        return errors;
    }

    static Schema getCrossrefSchema() throws SAXException {
        if (SCHEMA_CROSSREF == null) {
            SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            factory.setResourceResolver(new SimpleLSResourceResolver()
                    .base(CrossrefBuilder.class)
            );
            SCHEMA_CROSSREF = factory.newSchema(CrossrefBuilder.class.getResource(XSD_FILENAME));
        }
        return SCHEMA_CROSSREF;
    }

}
