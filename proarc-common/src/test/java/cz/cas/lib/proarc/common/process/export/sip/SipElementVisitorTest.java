/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.process.export.sip;

import com.yourmediashelf.fedora.foxml.DatastreamType;
import com.yourmediashelf.fedora.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.foxml.DigitalObject;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraUtils;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.StructMapType;
import jakarta.xml.bind.JAXBContext;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import mockit.Expectations;
import mockit.Mock;
import mockit.MockUp;
import mockit.Mocked;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SipElementVisitorTest {

    @Test
    void printedPeriodicalIssueWithEArticleAndPageUsesCollectionType(
            @Mocked IMetsElement issue,
            @Mocked IMetsElement article,
            @Mocked IMetsElement page
    ) {
        new Expectations() {{
            issue.getChildren(); result = Arrays.asList(article, page);
            issue.getModel(); result = NdkPlugin.MODEL_PERIODICALISSUE;
            article.getModel(); result = "info:fedora/" + NdkEbornPlugin.MODEL_EARTICLE;
            page.getElementType(); result = Const.PAGE;
        }};

        assertEquals("electronic_coll_journal", MetsElementVisitor.getPeriodicalType(issue));
    }

    @Test
    void periodicalWithoutPrintedPageKeepsElectronicType(
            @Mocked IMetsElement title,
            @Mocked IMetsElement article
    ) {
        new Expectations() {{
            title.getChildren(); result = Arrays.asList(article);
            title.getModel(); result = NdkEbornPlugin.MODEL_EPERIODICALISSUE;
        }};

        assertEquals("electronic_periodical", MetsElementVisitor.getPeriodicalType(title));
    }

    @Test
    void periodicalWithPageAndArticleUsesCollectionType(
            @Mocked IMetsElement title,
            @Mocked IMetsElement article,
            @Mocked IMetsElement page
    ) {
        new Expectations() {{
            title.getChildren(); result = Arrays.asList(article, page);
            title.getModel(); result = NdkEbornPlugin.MODEL_EPERIODICALISSUE;
            article.getModel(); result = NdkEbornPlugin.MODEL_EARTICLE;
            page.getElementType(); result = Const.PAGE;
        }};

        assertEquals("electronic_coll_journal", MetsElementVisitor.getPeriodicalType(title));
    }

    @Test
    void separateElectronicAndPrintedIssuesKeepTheirOwnTypes(
            @Mocked IMetsElement electronicIssue,
            @Mocked IMetsElement printedIssue,
            @Mocked IMetsElement article,
            @Mocked IMetsElement page
    ) {
        new Expectations() {{
            electronicIssue.getChildren(); result = Arrays.asList(article);
            electronicIssue.getModel(); result = NdkEbornPlugin.MODEL_EPERIODICALISSUE;
            printedIssue.getChildren(); result = Arrays.asList(page);
            page.getElementType(); result = Const.PAGE;
        }};

        assertEquals("electronic_periodical", MetsElementVisitor.getPeriodicalType(electronicIssue));
        assertEquals("Periodical", MetsElementVisitor.getPeriodicalType(printedIssue));
    }

    @Test
    void periodicalIssueWithSupplementsIncludesIssuePdf(
            @Mocked IMetsElement issue,
            @Mocked IMetsElement firstSupplement,
            @Mocked IMetsElement secondSupplement
    ) {
        new Expectations() {{
            issue.getChildren();
            result = Arrays.asList(firstSupplement, secondSupplement);
            issue.getSourceObject(); result = sourceObject(true);
            issue.getOriginalPid(); result = "uuid:issue"; minTimes = 0;
            firstSupplement.getSourceObject(); result = sourceObject(true);
            firstSupplement.getOriginalPid(); result = "uuid:first";
            secondSupplement.getSourceObject(); result = sourceObject(true);
            secondSupplement.getOriginalPid(); result = "uuid:second";
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
        MdSecType supplementMetadata = metadata("MODSMD_SUPPL_0001");
        new Expectations() {{
            issue.getChildren();
            result = Arrays.asList(supplement);
            issue.getMetsContext();
            result = context;
            issue.getElementID();
            result = "ISSUE_0001";
            issue.getSourceObject(); result = sourceObject(true);
            supplement.getMetsContext();
            result = context;
            supplement.getElementID();
            result = "SUPPL_0001";
            supplement.getSourceObject(); result = sourceObject(true);
            supplement.getModsMetsElement(); result = supplementMetadata;
            context.getPackageID();
            result = "sil-00002m";
        }};

        StructMapType source = createPeriodicalStructMap();
        source.getDiv().getDiv().get(0).getDiv().get(0).getDiv().get(0).getDMDID().add(supplementMetadata);

        StructMapType result = new SipElementVisitor().copyPeriodicalMap(source, issue);

        DivType resultIssue = result.getDiv().getDiv().get(0).getDiv().get(0);
        assertDocument(resultIssue.getDiv().get(0), "DOCUMENT_0001", "FILE_0001", "oc_sil-00002m_issue_0001");

        DivType resultSupplement = resultIssue.getDiv().get(1);
        assertEquals("SUPPLEMENT_0001", resultSupplement.getID());
        assertDocument(resultSupplement.getDiv().get(0), "DOCUMENT_0002", "FILE_0002", "oc_sil-00002m_suppl_0001");
    }

    @ParameterizedTest
    @CsvSource({"false, ART", "true, ART", "false, SUPPL", "true, SUPPL", "false, MIXED", "true, MIXED"})
    void periodicalFilesHaveUniqueIdsAndValidReferences(boolean issueHasRaw, String childType,
            @Mocked IMetsElement issue, @Mocked IMetsElement first, @Mocked IMetsElement second,
            @Mocked MetsContext context, @Mocked AkubraStorage storage,
            @Mocked AkubraStorage.AkubraObject object) throws Exception {
        String firstId = "MIXED".equals(childType) ? "ART_0001" : childType + "_0001";
        String secondId = "MIXED".equals(childType) ? "SUPPL_0001" : childType + "_0002";
        MdSecType firstMetadata = metadata("MODSMD_" + firstId);
        MdSecType secondMetadata = metadata("MODSMD_" + secondId);
        new Expectations() {{
            issue.getChildren(); result = Arrays.asList(first, second);
            issue.getSourceObject(); result = sourceObject(issueHasRaw);
            issue.getOriginalPid(); result = "uuid:issue"; minTimes = 0;
            issue.getElementID(); result = "ISSUE_0001"; minTimes = 0;
            issue.getMetsContext(); result = context; minTimes = 0;
            first.getSourceObject(); result = sourceObject(true);
            first.getOriginalPid(); result = "uuid:first";
            first.getElementID(); result = firstId;
            first.getMetsContext(); result = context;
            first.getModsMetsElement(); result = firstMetadata;
            second.getSourceObject(); result = sourceObject(true);
            second.getOriginalPid(); result = "uuid:second";
            second.getElementID(); result = secondId;
            second.getMetsContext(); result = context;
            second.getModsMetsElement(); result = secondMetadata;
            context.getPackageID(); result = "sil-000034";
            context.getTypeOfStorage(); result = Storage.AKUBRA;
            context.getAkubraStorage(); result = storage;
            storage.find(anyString); result = object;
        }};
        new MockUp<AkubraUtils>() {
            @Mock
            public InputStream getDatastreamDissemination(AkubraStorage.AkubraObject source, String id) {
                return new ByteArrayInputStream(new byte[]{1, 2, 3});
            }
        };

        List<IMetsElement> elements = SipElementVisitor.getPeriodicalElementsToExport(issue);
        assertEquals(issueHasRaw ? Arrays.asList(issue, first, second) : Arrays.asList(first, second), elements);
        FileVisitor visitor = new FileVisitor();
        // Exercise the same per-object file generation as insertIntoMets, including the container.
        visitor.prepareFileType(issue);
        for (IMetsElement child : issue.getChildren()) {
            visitor.prepareFileType(child);
        }
        List<FileType> files = visitor.files();
        assertEquals(issueHasRaw ? 3 : 2, files.size());
        assertEquals(files.size(), files.stream().map(FileType::getID).distinct().count());

        StructMapType source = createPeriodicalStructMap();
        DivType sourceIssue = source.getDiv().getDiv().get(0).getDiv().get(0);
        sourceIssue.getDiv().clear();
        for (String id : Arrays.asList(firstId, secondId)) {
            DivType child = new DivType();
            child.setID(id);
            sourceIssue.getDiv().add(child);
        }
        MdSecType metadata = new MdSecType();
        metadata.setID("MODS_0001");
        MdSecType.MdRef metadataRef = new MdSecType.MdRef();
        metadataRef.setLOCTYPE("URL");
        metadataRef.setMDTYPE("MODS");
        metadataRef.setHref("metadata/mods.xml");
        metadata.setMdRef(metadataRef);
        addMetadataReferences(source.getDiv(), metadata);
        sourceIssue.getDiv().get(0).getDMDID().add(firstMetadata);
        sourceIssue.getDiv().get(1).getDMDID().add(secondMetadata);
        StructMapType map = visitor.copyPeriodicalMap(source, issue);
        DivType resultIssue = map.getDiv().getDiv().get(0).getDiv().get(0);
        assertEquals(issueHasRaw ? 3 : 2, resultIssue.getDiv().size());
        int documentId = 1;
        if (issueHasRaw) {
            assertDocument(resultIssue.getDiv().get(0), "DOCUMENT_0001", "FILE_0001", "oc_sil-000034_issue_0001");
            documentId++;
        }
        for (int i = 0; i < 2; i++) {
            DivType child = resultIssue.getDiv().get(i + (issueHasRaw ? 1 : 0));
            String id = i == 0 ? firstId : secondId;
            assertEquals(id, child.getID());
            assertDocument(child.getDiv().get(0), String.format("DOCUMENT_%04d", documentId),
                    String.format("FILE_%04d", documentId), "oc_sil-000034_" + id.toLowerCase());
            documentId++;
        }

        Mets mets = new Mets();
        mets.getDmdSec().add(metadata);
        mets.getDmdSec().add(firstMetadata);
        mets.getDmdSec().add(secondMetadata);
        MetsType.FileSec fileSec = new MetsType.FileSec();
        fileSec.getFileGrp().add(visitor.fileGrpMapForTest());
        mets.setFileSec(fileSec);
        mets.getStructMap().add(map);
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
        JAXBContext.newInstance(Mets.class).createMarshaller().marshal(mets, document);
        try (InputStream schema = Mets.class.getResourceAsStream("mets.xsd")) {
            List<String> errors = MetsUtils.validateAgainstXSD(document, schema);
            assertTrue(errors.isEmpty(), errors.toString());
        }
    }

    private static void addMetadataReferences(DivType div, MdSecType metadata) {
        div.getDMDID().add(metadata);
        for (DivType child : div.getDiv()) {
            addMetadataReferences(child, metadata);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void mixedChildrenLinkTheirOwnPdfsWhenLogicalOrderDiffers(boolean issueHasRaw,
            @Mocked IMetsElement issue, @Mocked IMetsElement article1, @Mocked IMetsElement article2,
            @Mocked IMetsElement supplement1, @Mocked IMetsElement supplement2,
            @Mocked MetsContext context) {
        MdSecType articleMd1 = metadata("MODSMD_ART_0001");
        MdSecType articleMd2 = metadata("MODSMD_ART_0002");
        MdSecType supplementMd1 = metadata("MODSMD_SUPPL_0001");
        MdSecType supplementMd2 = metadata("MODSMD_SUPPL_0002");
        new Expectations() {{
            issue.getChildren(); result = Arrays.asList(article1, article2, supplement1, supplement2);
            issue.getSourceObject(); result = sourceObject(issueHasRaw);
            issue.getMetsContext(); result = context; minTimes = 0;
            issue.getElementID(); result = "ISSUE_0001"; minTimes = 0;
            article1.getModsMetsElement(); result = articleMd1;
            article1.getElementID(); result = "ART_0001";
            article1.getSourceObject(); result = sourceObject(true);
            article1.getMetsContext(); result = context;
            article2.getModsMetsElement(); result = articleMd2;
            article2.getElementID(); result = "ART_0002";
            article2.getSourceObject(); result = sourceObject(true);
            article2.getMetsContext(); result = context;
            supplement1.getModsMetsElement(); result = supplementMd1;
            supplement1.getElementID(); result = "SUPPL_0001";
            supplement1.getSourceObject(); result = sourceObject(true);
            supplement1.getMetsContext(); result = context;
            supplement2.getModsMetsElement(); result = supplementMd2;
            supplement2.getElementID(); result = "SUPPL_0002";
            supplement2.getSourceObject(); result = sourceObject(true);
            supplement2.getMetsContext(); result = context;
            context.getPackageID(); result = "sil-000034";
        }};
        StructMapType source = createPeriodicalStructMap();
        DivType sourceIssue = source.getDiv().getDiv().get(0).getDiv().get(0);
        sourceIssue.getDiv().clear();
        List<String> ids = Arrays.asList("SUPPLEMENT_0001", "SUPPLEMENT_0002", "ART_0001", "ART_0002");
        List<MdSecType> metadata = Arrays.asList(supplementMd1, supplementMd2, articleMd1, articleMd2);
        for (int i = 0; i < ids.size(); i++) {
            DivType child = new DivType();
            child.setID(ids.get(i));
            child.setTYPE(i < 2 ? "SUPPLEMENT" : "ARTICLE");
            child.getDMDID().add(metadata.get(i));
            sourceIssue.getDiv().add(child);
        }
        StructMapType result = new SipElementVisitor().copyPeriodicalMap(source, issue);
        DivType resultIssue = result.getDiv().getDiv().get(0).getDiv().get(0);
        int offset = issueHasRaw ? 1 : 0;
        assertEquals(4 + offset, resultIssue.getDiv().size());
        List<String> fileIds = Arrays.asList("suppl_0001", "suppl_0002", "art_0001", "art_0002");
        for (int i = 0; i < ids.size(); i++) {
            DivType child = resultIssue.getDiv().get(i + offset);
            assertEquals(ids.get(i), child.getID());
            assertEquals(i < 2 ? "SUPPLEMENT" : "ARTICLE", child.getTYPE());
            assertEquals(Arrays.asList(metadata.get(i)), child.getDMDID());
            int docId = i + offset + 1;
            assertDocument(child.getDiv().get(0), String.format("DOCUMENT_%04d", docId),
                    String.format("FILE_%04d", docId), "oc_sil-000034_" + fileIds.get(i));
        }
    }

    private static MdSecType metadata(String id) {
        MdSecType section = new MdSecType();
        section.setID(id);
        MdSecType.MdRef ref = new MdSecType.MdRef();
        ref.setLOCTYPE("URL");
        ref.setMDTYPE("MODS");
        ref.setHref("metadata/" + id + ".xml");
        section.setMdRef(ref);
        return section;
    }

    @Test
    void periodicalFilesAreDeduplicatedByPid(@Mocked IMetsElement issue,
            @Mocked IMetsElement first, @Mocked IMetsElement duplicate) {
        new Expectations() {{
            issue.getSourceObject(); result = sourceObject(false);
            issue.getChildren(); result = Arrays.asList(first, duplicate);
            first.getSourceObject(); result = sourceObject(true);
            first.getOriginalPid(); result = "uuid:article";
            duplicate.getSourceObject(); result = sourceObject(true);
            duplicate.getOriginalPid(); result = "uuid:article";
        }};
        assertEquals(Arrays.asList(first), SipElementVisitor.getPeriodicalElementsToExport(issue));
    }

    private static DigitalObject sourceObject(boolean hasRaw) {
        DigitalObject object = new DigitalObject();
        if (hasRaw) {
            DatastreamType raw = new DatastreamType();
            raw.setID("RAW");
            DatastreamVersionType version = new DatastreamVersionType();
            version.setMIMETYPE("application/pdf");
            raw.getDatastreamVersion().add(version);
            object.getDatastream().add(raw);
        }
        return object;
    }

    private static class FileVisitor extends SipElementVisitor {
        FileVisitor() {
            fileGrpMap = MetsUtils.initEbornFileGroups();
        }

        List<FileType> files() {
            return fileGrpMapForTest().getFile();
        }

        MetsType.FileSec.FileGrp fileGrpMapForTest() {
            return fileGrpMap.get(Const.OC_GRP_ID);
        }
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
