package cz.cas.lib.proarc.common.process.export.mets;

import cz.cas.lib.proarc.common.storage.CopyrightMdEditor;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType;
import cz.cas.lib.proarc.mets.StructMapType;
import cz.cas.lib.proarc.mets.StructLinkType.SmLink;
import jakarta.xml.bind.JAXBContext;
import java.io.StringWriter;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class CopyrightMdMetsTest {
    private Mets mets = new Mets();
    private MdSecType volumeMods = mods("VOLUME_0001");
    private MdSecType issueMods = mods("ISSUE_0001");
    private DivType volume = div("VOLUME_0001", volumeMods);
    private DivType issue = div("ISSUE_0001", issueMods);
    private DivType physicalIssue = div("DIV_P_0000", issueMods);

    private void prepare() {
        volume.setTYPE("PERIODICAL_VOLUME");
        issue.setTYPE("ISSUE");
        volume.getDiv().add(issue);
        map("LOGICAL", volume);
        map("PHYSICAL", physicalIssue);
        mets.getDmdSec().add(volumeMods);
        mets.getDmdSec().add(issueMods);
    }

    @Test void volumeRightsStayOnVolumeAndDoNotBecomeIssueRights() throws Exception {
        prepare();
        CopyrightMdMets.add(mets, copyright(), volumeMods, "VOLUME_0001", "PERIODICAL_VOLUME");
        assertEquals(1, mets.getAmdSec().size());
        MdSecType rights = mets.getAmdSec().get(0).getRightsMD().get(0);
        assertEquals("RIGHTS_VOLUME_0001", rights.getID());
        assertTrue(rights.getMdWrap().getLabel8().contains("PERIODICAL_VOLUME"));
        assertEquals(rights, volume.getADMID().get(0));
        assertTrue(issue.getADMID().isEmpty());
        assertTrue(physicalIssue.getADMID().isEmpty());
        StringWriter xml = new StringWriter();
        JAXBContext.newInstance(Mets.class).createMarshaller().marshal(mets, xml);
        assertTrue(xml.toString().contains("ADMID=\"RIGHTS_VOLUME_0001\""));
        assertTrue(xml.toString().contains("OTHERMDTYPE=\"CopyrightMD\""));
    }

    @Test void issueRightsLinkLogicalAndPhysicalStructureWithoutDuplicatingSections() throws Exception {
        prepare();
        CopyrightMdMets.add(mets, copyright(), issueMods, "ISSUE_0001", "ISSUE");
        CopyrightMdMets.add(mets, copyright(), issueMods, "ISSUE_0001", "ISSUE");
        assertEquals(1, mets.getAmdSec().size());
        assertEquals(1, issue.getADMID().size());
        assertEquals(issue.getADMID(), physicalIssue.getADMID());
        assertTrue(volume.getADMID().isEmpty());
    }

    @Test void linksExplicitPhysicalPagesButDoesNotAssignRightsToSharedContainer() throws Exception {
        prepare();
        physicalIssue.getDMDID().add(volumeMods);
        DivType page = div("PAGE_0001", null);
        physicalIssue.getDiv().add(page);
        MetsType.StructLink links = new MetsType.StructLink();
        SmLink edge = new SmLink();
        edge.setFrom(issue.getID());
        edge.setTo(page.getID());
        links.getSmLinkOrSmLinkGrp().add(edge);
        mets.setStructLink(links);
        CopyrightMdMets.add(mets, copyright(), issueMods, "ISSUE_0001", "ISSUE");
        assertTrue(physicalIssue.getADMID().isEmpty());
        assertEquals(issue.getADMID(), page.getADMID());
    }

    @Test void copyrightTraversalDoesNotCreateEmptyMetadataReferences() throws Exception {
        prepare();
        DivType logicalRoot = div("LOGICAL_ROOT", null);
        logicalRoot.getDiv().add(volume);
        mets.getStructMap().get(0).setDiv(logicalRoot);
        for (int i = 1; i <= 3; i++) {
            physicalIssue.getDiv().add(div("PAGE_000" + i, null));
        }
        CopyrightMdMets.add(mets, copyright(), issueMods, "ISSUE_0001", "ISSUE");
        StringWriter xml = new StringWriter();
        JAXBContext.newInstance(Mets.class).createMarshaller().marshal(mets, xml);
        assertFalse(xml.toString().contains("DMDID=\"\""), xml.toString());
        assertFalse(xml.toString().contains("ADMID=\"\""), xml.toString());
        assertTrue(xml.toString().contains("DMDID=\"MODSMD_ISSUE_0001\""));
        assertTrue(xml.toString().contains("ADMID=\"RIGHTS_ISSUE_0001\""));
    }

    private org.w3c.dom.Element copyright() throws Exception {
        return CopyrightMdEditor.parse("<copyright xmlns='" + CopyrightMdEditor.NS + "' copyright.status='unknown' publication.status='unknown'/>");
    }
    private static MdSecType mods(String id) { MdSecType mods = new MdSecType(); mods.setID("MODSMD_" + id); return mods; }
    private static DivType div(String id, MdSecType mods) { DivType div = new DivType(); div.setID(id); if (mods != null) { div.getDMDID().add(mods); } return div; }
    private void map(String type, DivType div) { StructMapType map = new StructMapType(); map.setTYPE(type); map.setDiv(div); mets.getStructMap().add(map); }
}
