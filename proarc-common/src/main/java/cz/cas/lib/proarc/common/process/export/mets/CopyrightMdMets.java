package cz.cas.lib.proarc.common.process.export.mets;

import cz.cas.lib.proarc.mets.AmdSecType;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.StructMapType;
import cz.cas.lib.proarc.mets.StructLinkType.SmLink;
import java.util.ArrayList;
import java.util.List;
import org.w3c.dom.Element;

/** Adds explicitly scoped rights; never copies rights to another document level. */
public final class CopyrightMdMets {
    private CopyrightMdMets() { }

    public static void add(Mets mets, Element copyright, MdSecType mods, String scope, String type) {
        String id = "RIGHTS_" + scope;
        if (mets.getAmdSec().stream().anyMatch(section -> section.getRightsMD().stream().anyMatch(rights -> id.equals(rights.getID())))) {
            return;
        }
        AmdSecType amd = new AmdSecType();
        amd.setID("AMD_COPYRIGHT_" + scope);
        MdSecType rights = new MdSecType();
        rights.setID(id);
        MdSecType.MdWrap wrap = new MdSecType.MdWrap();
        wrap.setMDTYPE("OTHER");
        wrap.setOTHERMDTYPE("CopyrightMD");
        wrap.setMIMETYPE("text/xml");
        wrap.setLabel8("CopyrightMD for " + type + " " + scope);
        MdSecType.MdWrap.XmlData data = new MdSecType.MdWrap.XmlData();
        Element exportedCopyright = (Element) copyright.cloneNode(true);
        exportedCopyright.removeAttributeNS(javax.xml.XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "schemaLocation");
        data.getAny().add(exportedCopyright);
        wrap.setXmlData(data);
        rights.setMdWrap(wrap);
        amd.getRightsMD().add(rights);
        mets.getAmdSec().add(amd);

        List<DivType> logicalTargets = new ArrayList<>();
        for (StructMapType map : mets.getStructMap()) {
            link(map.getDiv(), mods, rights, "PHYSICAL".equalsIgnoreCase(map.getTYPE()), logicalTargets);
        }
        // Explicit structLink mappings also identify the physical pages of an internal part.
        if (mets.getStructLink() != null) {
            for (Object entry : mets.getStructLink().getSmLinkOrSmLinkGrp()) {
                if (!(entry instanceof SmLink)) { continue; }
                SmLink edge = (SmLink) entry;
                if (logicalTargets.stream().anyMatch(div -> edge.getFrom().equals(div.getID()))) {
                    for (StructMapType map : mets.getStructMap()) {
                        if ("PHYSICAL".equalsIgnoreCase(map.getTYPE())) { linkPhysicalPage(map.getDiv(), edge.getTo(), rights); }
                    }
                }
            }
        }
    }

    private static void link(DivType div, MdSecType mods, MdSecType rights, boolean physical, List<DivType> logicalTargets) {
        if (div == null) { return; }
        // A shared physical container does not establish a scope for one of its several documents.
        if (div.isSetDMDID() && div.getDMDID().contains(mods) && (!physical || div.getDMDID().size() == 1)) {
            div.getADMID().add(rights);
            if (!physical) { logicalTargets.add(div); }
        }
        for (DivType child : div.getDiv()) { link(child, mods, rights, physical, logicalTargets); }
    }

    private static void linkPhysicalPage(DivType div, String id, MdSecType rights) {
        if (div == null) { return; }
        if (id.equals(div.getID()) && !div.getADMID().contains(rights)) { div.getADMID().add(rights); }
        for (DivType child : div.getDiv()) { linkPhysicalPage(child, id, rights); }
    }
}
