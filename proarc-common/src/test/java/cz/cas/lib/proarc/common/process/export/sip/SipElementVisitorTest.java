/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.process.export.sip;

import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.StructMapType;
import java.util.Arrays;
import java.util.List;
import mockit.Expectations;
import mockit.Mocked;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SipElementVisitorTest {

    @Test
    void periodicalIssueWithSupplementsIncludesIssuePdf(
            @Mocked IMetsElement issue,
            @Mocked IMetsElement firstSupplement,
            @Mocked IMetsElement secondSupplement
    ) {
        new Expectations() {{
            issue.getChildren();
            result = Arrays.asList(firstSupplement, secondSupplement);
        }};

        List<IMetsElement> elements = SipElementVisitor.getPeriodicalElementsToExport(issue);

        assertEquals(Arrays.asList(issue, firstSupplement, secondSupplement), elements);
    }

    @Test
    void periodicalStructMapLinksIssueAndSupplementPdfs(
            @Mocked IMetsElement issue,
            @Mocked IMetsElement supplement,
            @Mocked MetsContext context
    ) {
        new Expectations() {{
            issue.getChildren();
            result = Arrays.asList(supplement);
            issue.getMetsContext();
            result = context;
            issue.getElementID();
            result = "ISSUE_0001";
            supplement.getMetsContext();
            result = context;
            supplement.getElementID();
            result = "SUPPL_0001";
            context.getPackageID();
            result = "sil-00002m";
        }};

        StructMapType source = createPeriodicalStructMap();

        StructMapType result = new SipElementVisitor().copyPeriodicalMap(source, issue);

        DivType resultIssue = result.getDiv().getDiv().get(0).getDiv().get(0);
        assertDocument(resultIssue.getDiv().get(0), "DOCUMENT_0001", "FILE_0001", "oc_sil-00002m_issue_0001");

        DivType resultSupplement = resultIssue.getDiv().get(1);
        assertEquals("SUPPLEMENT_0001", resultSupplement.getID());
        assertDocument(resultSupplement.getDiv().get(0), "DOCUMENT_0002", "FILE_0002", "oc_sil-00002m_suppl_0001");
    }

    private static StructMapType createPeriodicalStructMap() {
        DivType supplement = new DivType();
        supplement.setID("SUPPLEMENT_0001");
        DivType issue = new DivType();
        issue.setID("ISSUE_0001");
        issue.getDiv().add(supplement);
        DivType volume = new DivType();
        volume.setID("PERIODICAL_VOLUME_0001");
        volume.getDiv().add(issue);
        DivType title = new DivType();
        title.setID("PERIODICAL_TITLE_0001");
        title.getDiv().add(volume);
        StructMapType structMap = new StructMapType();
        structMap.setDiv(title);
        return structMap;
    }

    private static void assertDocument(DivType document, String documentId, String fileId, String fileReference) {
        assertEquals(documentId, document.getID());
        DivType file = document.getDiv().get(0);
        assertEquals(fileId, file.getID());
        FileType fileType = (FileType) file.getFptr().get(0).getFILEID();
        assertEquals(fileReference, fileType.getID());
    }
}
