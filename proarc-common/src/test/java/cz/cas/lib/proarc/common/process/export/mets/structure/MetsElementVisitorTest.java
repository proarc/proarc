/*
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.process.export.mets.structure;

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.relation.Relations;
import cz.cas.lib.proarc.mets.DivType;
import java.util.Collections;
import javax.xml.parsers.DocumentBuilderFactory;
import org.easymock.EasyMock;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class MetsElementVisitorTest {

    @Test
    public void returnsNoScannerTemplateForPageWithoutDevice() throws Exception {
        Document document = newDocument();
        Element rdf = document.createElementNS(Relations.RDF_NS, "rdf:RDF");
        document.appendChild(rdf);
        Element description = document.createElementNS(Relations.RDF_NS, "rdf:Description");
        rdf.appendChild(description);

        IMetsElement element = EasyMock.createNiceMock(IMetsElement.class);
        MetsContext context = new MetsContext();
        context.setTypeOfStorage(Storage.AKUBRA);
        EasyMock.expect(element.getMetsContext()).andStubReturn(context);
        EasyMock.expect(element.getRelsExt()).andStubReturn(Collections.singletonList(rdf));
        EasyMock.replay(element);

        assertNull(MetsElementVisitor.getScannerMets(element));
    }

    @Test
    public void addsDonatorAsAnotherFundingNote() throws Exception {
        Document modsDocument = newDocument();
        Element mods = modsDocument.createElementNS(ModsConstants.NS, "mods:mods");
        modsDocument.appendChild(mods);
        Element existingNote = modsDocument.createElementNS(ModsConstants.NS, "mods:note");
        existingNote.setAttribute("type", "funding");
        existingNote.setTextContent("Existing funding note");
        mods.appendChild(existingNote);

        Element rdf = createRelsExt("info:fedora/donator:Ministry of Culture");

        MetsElementVisitor.addDonatorToMods(
                Collections.singletonList(mods), Collections.singletonList(rdf), "uuid:package", "uuid:package");

        NodeList notes = mods.getElementsByTagNameNS(ModsConstants.NS, "note");
        assertEquals(2, notes.getLength());
        assertEquals("Existing funding note", notes.item(0).getTextContent());
        assertEquals("funding", ((Element) notes.item(1)).getAttribute("type"));
        assertEquals("Ministry of Culture", notes.item(1).getTextContent());
    }

    @Test
    public void keepsModsUnchangedWithoutDonator() throws Exception {
        Document modsDocument = newDocument();
        Element mods = modsDocument.createElementNS(ModsConstants.NS, "mods:mods");
        modsDocument.appendChild(mods);

        Document relsDocument = newDocument();
        Element rdf = relsDocument.createElementNS(Relations.RDF_NS, "rdf:RDF");
        relsDocument.appendChild(rdf);

        MetsElementVisitor.addDonatorToMods(
                Collections.singletonList(mods), Collections.singletonList(rdf), "uuid:package", "uuid:package");

        assertEquals(0, mods.getElementsByTagNameNS(ModsConstants.NS, "note").getLength());
    }

    @Test
    public void keepsModsUnchangedForObjectInsidePackage() throws Exception {
        Document modsDocument = newDocument();
        Element mods = modsDocument.createElementNS(ModsConstants.NS, "mods:mods");
        modsDocument.appendChild(mods);

        Element rdf = createRelsExt("info:fedora/donator:Ministry of Culture");

        MetsElementVisitor.addDonatorToMods(
                Collections.singletonList(mods), Collections.singletonList(rdf), "uuid:page", "uuid:package");

        assertEquals(0, mods.getElementsByTagNameNS(ModsConstants.NS, "note").getLength());
    }

    @Test
    public void leavesTypeEmptyForGenericPage() throws Exception {
        DivType pageDiv = new DivType();
        new MetsElementVisitor().fillPageIndexOrder(pageElement(NdkPlugin.MODEL_PAGE, null), pageDiv);

        assertNull(pageDiv.getTYPE());
        assertEquals("1", pageDiv.getORDERLABEL());
        assertEquals(1, pageDiv.getORDER().intValue());
    }

    @Test
    public void requiresTypeForNdkAndSttPages() throws Exception {
        MetsElementVisitor visitor = new MetsElementVisitor();

        assertThrows(MetsExportException.class,
                () -> visitor.fillPageIndexOrder(pageElement(NdkPlugin.MODEL_NDK_PAGE, null), new DivType()));
        assertThrows(MetsExportException.class,
                () -> visitor.fillPageIndexOrder(pageElement(NdkPlugin.MODEL_NDK_PAGE, "  "), new DivType()));
        assertThrows(MetsExportException.class,
                () -> visitor.fillPageIndexOrder(pageElement(OldPrintPlugin.MODEL_PAGE, null), new DivType()));
    }

    @Test
    public void exportsExplicitNormalPageType() throws Exception {
        DivType pageDiv = new DivType();
        new MetsElementVisitor().fillPageIndexOrder(
                pageElement(NdkPlugin.MODEL_NDK_PAGE, "normalPage"), pageDiv);

        assertEquals("normalPage", pageDiv.getTYPE());
    }

    private static IMetsElement pageElement(String model, String pageType) throws Exception {
        Document document = newDocument();
        Element mods = document.createElementNS(ModsConstants.NS, "mods:mods");
        document.appendChild(mods);
        Element part = document.createElementNS(ModsConstants.NS, "mods:part");
        if (pageType != null) {
            part.setAttribute("type", pageType);
        }
        mods.appendChild(part);
        addDetail(document, part, "pageNumber", "1");
        addDetail(document, part, "pageIndex", "1");

        IMetsElement element = EasyMock.createNiceMock(IMetsElement.class);
        MetsContext context = new MetsContext();
        context.setTypeOfStorage(Storage.LOCAL);
        EasyMock.expect(element.getModsStream()).andStubReturn(Collections.singletonList(mods));
        EasyMock.expect(element.getModel()).andStubReturn(model);
        EasyMock.expect(element.getOriginalPid()).andStubReturn("uuid:page");
        EasyMock.expect(element.getMetsContext()).andStubReturn(context);
        EasyMock.replay(element);
        return element;
    }

    private static void addDetail(Document document, Element part, String type, String value) {
        Element detail = document.createElementNS(ModsConstants.NS, "mods:detail");
        detail.setAttribute("type", type);
        Element number = document.createElementNS(ModsConstants.NS, "mods:number");
        number.setTextContent(value);
        detail.appendChild(number);
        part.appendChild(detail);
    }

    private static Element createRelsExt(String resource) throws Exception {
        Document document = newDocument();
        Element rdf = document.createElementNS(Relations.RDF_NS, "rdf:RDF");
        document.appendChild(rdf);
        Element description = document.createElementNS(Relations.RDF_NS, "rdf:Description");
        rdf.appendChild(description);
        Element hasDonator = document.createElementNS(Relations.PROARC_RELS_NS, "proarc-rels:hasDonator");
        hasDonator.setAttributeNS(Relations.RDF_NS, "rdf:resource", resource);
        description.appendChild(hasDonator);
        return rdf;
    }

    private static Document newDocument() throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        return factory.newDocumentBuilder().newDocument();
    }
}
