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
import cz.cas.lib.proarc.common.storage.relation.Relations;
import java.util.Collections;
import javax.xml.parsers.DocumentBuilderFactory;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class MetsElementVisitorTest {

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
