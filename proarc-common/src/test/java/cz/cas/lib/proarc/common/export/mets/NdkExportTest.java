/*
 * Copyright (C) 2014 Robert Simonovsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cas.lib.proarc.common.export.mets;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.dom.DOMSource;

import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap.XmlData;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import cz.cas.lib.proarc.premis.AgentComplexType;
import cz.cas.lib.proarc.premis.EventComplexType;
import cz.cas.lib.proarc.premis.PremisUtils;

public class NdkExportTest {
    @Rule
    public TemporaryFolder tmp = new TemporaryFolder();

    /**
     * Returns the source path for input documents
     *
     * @return
     */
    private String getTargetPath() {
        URL res = this.getClass().getResource(this.getClass().getSimpleName() + ".class");
        File fileName = new File(res.getFile());
        return fileName.getParent();
    }

    /**
     * Tests if the exception is thrown for invalid mets
     *
     */
    @Test
    public void testAmdSec() throws Exception {
        String sourceDirPath = getTargetPath() + File.separator +
                "monograph" + File.separator;
        File resultDir = tmp.newFolder("result" + "monograph");
        String path = sourceDirPath + "1ccbf6c5-b22c-4d89-b42e-8cd14101a737.xml";
        DigitalObject dbObj = MetsUtils.readFoXML(path);
        Configuration config = new BaseConfiguration();
        config.addProperty(NdkExportOptions.PROP_NDK_AGENT_CREATOR, "Creator");
        config.addProperty(NdkExportOptions.PROP_NDK_AGENT_ARCHIVIST, "Archivist");
        MetsContext context = new MetsContext();
        context.setPath(sourceDirPath);
        context.setFsParentMap(TestConst.parents);
        context.setOutputPath(resultDir.getAbsolutePath());
        context.setAllowNonCompleteStreams(true);
        context.setAllowMissingURNNBN(true);
        context.setConfig(NdkExportOptions.getOptions(config));
        MetsElement metsElement = MetsElement.getElement(dbObj, null, context, true);
        MetsElementVisitor visitor = new MetsElementVisitor();
        metsElement.accept(visitor);
        File amdSecFile = new File(resultDir.getAbsolutePath()+File.separator+"44589055-9fad-4a9f-b6a8-75be399f332d"+File.separator+"amdSec"+File.separator+"AMD_METS_44589055-9fad-4a9f-b6a8-75be399f332d_0000.xml");
        JAXBContext jaxbContextMets = JAXBContext.newInstance(Mets.class);
        Unmarshaller unmarshallerMets = jaxbContextMets.createUnmarshaller();
        Mets mets = (Mets) unmarshallerMets.unmarshal(amdSecFile);
        assertEquals("PAGE_0001", mets.getAmdSec().get(0).getID());
        List<MdSecType> techMDList = mets.getAmdSec().get(0).getTechMD();
        assertEquals(2, techMDList.size());
        for (MdSecType techMD : techMDList) {
        if ("MIX_002".equals(techMD.getID())) {
            XmlData mixData = techMD.getMdWrap().getXmlData();
            List<Element> mixElements = new ArrayList<Element>();
            mixElements.add((Element)mixData.getAny().get(0));
            Document mixDocument = MetsUtils.getDocumentFromList(mixElements);
                DOMSource mixSource = new DOMSource(mixDocument);
                Mix mix = MixUtils.unmarshal(mixSource, Mix.class);
                assertEquals("ProArc_URI", mix.getBasicDigitalObjectInformation().getObjectIdentifier().get(0).getObjectIdentifierType().getValue());
                assertEquals("JPEG", mix.getBasicDigitalObjectInformation().getCompression().get(0).getCompressionScheme().getValue());
                assertNotNull(mix.getChangeHistory().getImageProcessing().get(0).getDateTimeProcessed().getValue());
            } else
                if ("OBJ_002".equals(techMD.getID())) {
                    XmlData premisData = techMD.getMdWrap().getXmlData();
                    DOMSource premisSource = new DOMSource((Node) premisData.getAny().get(0));
                cz.cas.lib.proarc.premis.File premisType = PremisUtils.unmarshal(premisSource, cz.cas.lib.proarc.premis.File.class);
                assertEquals("info:fedora/uuid:2ff2dd0c-d438-4d95-940f-690ee0f44a4a/NDK_ARCHIVAL",
                        premisType.getObjectIdentifier().get(0).getObjectIdentifierValue());
                assertEquals("9b0a294cda0508b1a205a57fa66f9568",
                        premisType.getObjectCharacteristics().get(0).getFixity().get(0).getMessageDigest());
                assertEquals("ProArc", premisType.getObjectCharacteristics().get(0).getFixity().get(0).getMessageDigestOriginator());
                assertEquals("derivation", premisType.getRelationship().get(0).getRelationshipType());
                assertEquals("MC_creation_001",
                        premisType.getRelationship().get(0).getRelatedEventIdentification().get(0).getRelatedEventIdentifierValue());

                } else {
                Assert.fail("Unexpected node:" + techMD.getID());
                }
        }

        List<MdSecType> digiProvList = mets.getAmdSec().get(0).getDigiprovMD();
        assertEquals(2, digiProvList.size());
        for (MdSecType digiProv : digiProvList) {
            if ("EVT_002".equals(digiProv.getID())) {
                XmlData premisData = digiProv.getMdWrap().getXmlData();
                DOMSource premisSource = new DOMSource((Node) premisData.getAny().get(0));
                EventComplexType premisType = PremisUtils.unmarshal(premisSource, EventComplexType.class);
                assertEquals("MC_creation_001", premisType.getEventIdentifier().getEventIdentifierValue());
                assertEquals("migration/MC_creation", premisType.getEventDetail());
                assertEquals("ProArc_AgentID", premisType.getLinkingAgentIdentifier().get(0).getLinkingAgentIdentifierType());
                assertEquals("ProArc", premisType.getLinkingAgentIdentifier().get(0).getLinkingAgentIdentifierValue());
                assertEquals("info:fedora/uuid:2ff2dd0c-d438-4d95-940f-690ee0f44a4a/NDK_ARCHIVAL", premisType.getLinkingObjectIdentifier().get(0).getLinkingObjectIdentifierValue());
            } else if ("AGENT_001".equals(digiProv.getID())) {
                XmlData premisData = digiProv.getMdWrap().getXmlData();
                DOMSource premisSource = new DOMSource((Node) premisData.getAny().get(0));
                AgentComplexType premisType = PremisUtils.unmarshal(premisSource, AgentComplexType.class);
                assertEquals("ProArc_AgentID", premisType.getAgentIdentifier().get(0).getAgentIdentifierType());
                assertEquals("ProArc", premisType.getAgentIdentifier().get(0).getAgentIdentifierValue());
            } else {
                Assert.fail("Unexpected node:" + digiProv.getID());
            }
        }

        assertEquals(3, mets.getFileSec().getFileGrp().size());
        assertEquals("PHYSICAL", mets.getStructMap().get(0).getTYPE());
        assertEquals("MONOGRAPH_PAGE", mets.getStructMap().get(0).getDiv().getTYPE());
        assertEquals(3, mets.getStructMap().get(0).getDiv().getFptr().size());
    }
}
