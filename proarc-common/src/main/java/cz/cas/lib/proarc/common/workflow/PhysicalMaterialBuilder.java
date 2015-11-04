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
package cz.cas.lib.proarc.common.workflow;

import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.export.mets.ValidationErrorHandler;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.workflow.model.PhysicalMaterial;
import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import java.io.IOException;
import java.io.StringReader;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 *
 * @author Jan Pokorsky
 */
class PhysicalMaterialBuilder {

    private final PhysicalMaterial m = new PhysicalMaterial();
    private XPath xpath;
    private DocumentBuilder db;
    private ValidationErrorHandler validationErrorHandler;

    public PhysicalMaterialBuilder() {
        initXml();
    }

    public PhysicalMaterial build() {
        return m;
    }

    public PhysicalMaterial build(String xml, CatalogConfiguration catalog) {
        setCatalog(catalog);
        setMetadata(xml);
        return m;
    }

    public PhysicalMaterialBuilder setCatalog(CatalogConfiguration catalog) {
        m.setSource(catalog.getUrl());
        return this;
    }

    public PhysicalMaterialBuilder setMetadata(String modsXml) {
        try {
            return setMetadataImpl(modsXml);
        } catch (Exception ex) {
            StringBuilder sb = new StringBuilder("Cannot set metadata!");
            for (String error : validationErrorHandler.getValidationErrors()) {
                sb.append('\n').append(error);
            }
            throw new IllegalStateException(sb.toString(), ex);
        }
    }

    private PhysicalMaterialBuilder setMetadataImpl(String modsXml) throws IOException, SAXException, XPathExpressionException {
        Document modsDom = db.parse(new InputSource(new StringReader(modsXml)));
        Element modsElm = (Element) xpath.evaluate(
                "m:mods | m:modsCollection/m:mods", modsDom, XPathConstants.NODE);
        String barcode = xpath.evaluate(
                "m:identifier[@type='barcode' and not(@invalid)]", modsElm);
        StringBuilder label = getTitle(new StringBuilder(), modsElm);
        m.setMetadata(modsXml);
        m.setBarcode(barcode);
        m.setField001(null);
        m.setLabel(label.length() == 0
                ? "?"
                : label.length() > 2000
                        ? label.substring(0, 2000) : label.toString());
        return this;
    }

    private StringBuilder getTitle(StringBuilder label, Element modsElm) throws XPathExpressionException {
        Element titleInfoElm = (Element) xpath.evaluate(
                "m:titleInfo[not(@type) and m:title/text()]", modsElm, XPathConstants.NODE);
        if (titleInfoElm != null) {
            String title = val(xpath.evaluate("m:title/text()", titleInfoElm));
            String subTitle = val(xpath.evaluate("m:subTitle/text()", titleInfoElm));
            label.append(title);
            if (subTitle != null) {
                label.append(" : ");
                label.append(subTitle);
            }
        }
        return label;
    }

    private void initXml() {
        try {
            XPathFactory xPathFactory = ProarcXmlUtils.defaultXPathFactory();
            xpath = xPathFactory.newXPath();
            xpath.setNamespaceContext(new SimpleNamespaceContext().add("m", ModsConstants.NS));
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            dbf.setSchema(ModsUtils.getSchema());
            db = dbf.newDocumentBuilder();
            validationErrorHandler = new ValidationErrorHandler();
            db.setErrorHandler(validationErrorHandler);
        } catch (Exception ex) {
            throw new IllegalStateException("Cannot initialize XML support!", ex);
        }
    }

    static String val(String v) {
        return v == null || v.isEmpty() ? null : v;
    }
}
