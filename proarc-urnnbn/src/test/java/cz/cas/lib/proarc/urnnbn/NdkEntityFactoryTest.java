/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.urnnbn;

import cz.cas.lib.proarc.mix.MixType;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.urnnbn.model.registration.DigitalDocument;
import cz.cas.lib.proarc.urnnbn.model.registration.Import;
import cz.cas.lib.proarc.urnnbn.model.registration.Monograph;
import cz.cas.lib.proarc.urnnbn.model.registration.MonographVolume;
import cz.cas.lib.proarc.urnnbn.model.registration.OriginatorTypeType;
import cz.cas.lib.proarc.urnnbn.model.registration.OtherEntity;
import cz.cas.lib.proarc.urnnbn.model.registration.PeriodicalIssue;
import cz.cas.lib.proarc.urnnbn.model.registration.PeriodicalIssue.TitleInfo;
import cz.cas.lib.proarc.urnnbn.model.registration.PrimaryOriginator;
import cz.cas.lib.proarc.urnnbn.model.registration.Publication;
import cz.cas.lib.proarc.urnnbn.model.registration.TechnicalMetadata;
import java.math.BigInteger;
import javax.xml.bind.JAXB;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class NdkEntityFactoryTest {

    private NdkEntityFactory factory;

    public NdkEntityFactoryTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
        factory = new NdkEntityFactory();
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testCreatePeriodicalIssueImport() throws Exception {
        ModsDefinition periodicalMods = mods("registration/ndkPeriodical.xml");
        ModsDefinition periodicalIssueMods = mods("registration/ndkPeriodicalIssue.xml");
        ModsDefinition periodicalVolumeMods = mods("registration/ndkPeriodicalVolume.xml");
        MixType mix = mix("registration/mix.xml");
        Import imp = factory.createPeriodicalIssueImport(
                periodicalMods, periodicalVolumeMods, periodicalIssueMods, mix, null, false);
        assertNotNull(imp);
        PeriodicalIssue pi = imp.getPeriodicalIssue();
        assertNotNull("PeriodicalIssue", pi);
        assertEquals("ccnb", "cnb002372844", pi.getCcnb());
        assertNull("issn", pi.getIssn());
        assertNull("otherOriginator", pi.getOtherOriginator());

        PrimaryOriginator primaryOriginator = pi.getPrimaryOriginator();
        assertNotNull("primaryOriginator", primaryOriginator);
        assertEquals("primaryOriginator.type", OriginatorTypeType.CORPORATION, primaryOriginator.getType());
        assertEquals("primaryOriginator.value", "Okresní pedagogické středisko Hradec Králové", primaryOriginator.getValue());

        Publication pub = pi.getPublication();
        assertNotNull("publication", pub);
        assertEquals("place", "Hradec Králové", pub.getPlace());
        assertEquals("publisher", "Okresní pedagogické středisko", pub.getPublisher());
        assertEquals("1967", pub.getYear());

        TitleInfo titleInfo = pi.getTitleInfo();
        assertNotNull(titleInfo);
        assertEquals("Škola", titleInfo.getPeriodicalTitle());
        assertEquals("1", titleInfo.getVolumeTitle());
        assertEquals("1-2", titleInfo.getIssueTitle());

        DigitalDocument dd = imp.getDigitalDocument();
        assertNotNull("DigitalDocument", dd);
        assertEquals("uuid", dd.getRegistrarScopeIdentifiers().getId().get(0).getType());
        assertEquals("18ac200c-7e1a-4367-a089-71077231da1c", dd.getRegistrarScopeIdentifiers().getId().get(0).getValue());
        TechnicalMetadata tm = dd.getTechnicalMetadata();
        assertNotNull(tm);
//        assertEquals(new Double("0"), tm.getCompression().getRatio());
        assertEquals("Unknown", tm.getCompression().getValue());
        assertEquals("image/jp2", tm.getFormat().getValue());
        assertEquals("1.0", tm.getFormat().getVersion());
        assertEquals(new BigInteger("2721"), tm.getPictureSize().getHeight());
        assertEquals(new BigInteger("1985"), tm.getPictureSize().getWidth());
        assertEquals(new BigInteger("118"), tm.getResolution().getHorizontal());
        assertEquals(new BigInteger("118"), tm.getResolution().getVertical());
//        JAXB.marshal(imp, new StreamResult(System.out));
    }
    @Test
    public void testCreatePeriodicalIssueImport_Supplement() throws Exception {
        ModsDefinition periodicalMods = mods("registration/ndkPeriodical2.xml");
        ModsDefinition periodicalIssueMods = mods("registration/ndkPeriodical2VolumeSupplement.xml");
        ModsDefinition periodicalVolumeMods = mods("registration/ndkPeriodical2Volume.xml");
        MixType mix = mix("registration/mix.xml");
        Import imp = factory.createPeriodicalIssueImport(
                periodicalMods, periodicalVolumeMods, periodicalIssueMods, mix, null, false);
        assertNotNull(imp);
        PeriodicalIssue pi = imp.getPeriodicalIssue();
        assertNotNull(pi);
        assertEquals("ccnb", "cnb000356910", pi.getCcnb());
        assertEquals("issn", "0862-6545", pi.getIssn());
        assertNull("otherOriginator", pi.getOtherOriginator());

        TitleInfo titleInfo = pi.getTitleInfo();
        assertNotNull(titleInfo);
        assertEquals("Respekt", titleInfo.getPeriodicalTitle());
        assertEquals("10", titleInfo.getVolumeTitle());
        assertEquals("Bibliografický přehled článků za r. 1999", titleInfo.getIssueTitle());

        assertNotNull(imp.getDigitalDocument());
    }

    @Test
    public void testCreateMonographImport() throws Exception {
        ModsDefinition monographMods = mods("registration/ndkMonograph.xml");
        MixType mix = mix("registration/mix.xml");
        Import imp = factory.createMonographImport(monographMods, mix, null, false, false);
        assertNotNull(imp);
        Monograph mv = imp.getMonograph();
        assertNotNull(mv);
        assertEquals("cnb000389898", mv.getCcnb());
        assertNull(mv.getIsbn());
        assertEquals("Průvodce českým rájem", mv.getTitleInfo().getTitle());
        assertEquals("stopadesát výletů z Turnova ; [Jičín a okolí]", mv.getTitleInfo().getSubTitle());

        assertNotNull(imp.getDigitalDocument());
    }

    @Test
    public void testCreateMultipartMonographImport() throws Exception {
        ModsDefinition titleMods = mods("registration/ndkMultipartTitle.xml");
        ModsDefinition volumeMods = mods("registration/ndkMultipartVolume.xml");
        MixType mix = mix("registration/mix.xml");
        Import imp = factory.createMultipartMonographImport(titleMods, volumeMods, mix, null, false);
        assertNotNull(imp);
        MonographVolume mv = imp.getMonographVolume();
        assertNotNull(mv);
        assertEquals("cnb000963966", mv.getCcnb());
        assertEquals("80-7201-204-5 (1. díl)", mv.getIsbn());
        assertEquals("1", mv.getTitleInfo().getVolumeTitle());
        assertEquals("Průvodce judikaturou Evropského soudního dvora", mv.getTitleInfo().getMonographTitle());

        assertNotNull(imp.getDigitalDocument());
    }

    @Test
    public void testCreateMultipartMonographImport_Supplement() throws Exception {
        ModsDefinition titleMods = mods("registration/ndkMultipartTitle2.xml");
        ModsDefinition volumeMods = mods("registration/ndkMultipartTitle2Supplement.xml");
        MixType mix = mix("registration/mix.xml");
        Import imp = factory.createMultipartMonographImport(titleMods, volumeMods, mix, null, false);
        assertNotNull(imp);
        MonographVolume mv = imp.getMonographVolume();
        assertNotNull(mv);
        assertNull(mv.getCcnb());
        assertEquals("978-0-19-459881-1 (student's book : brož.)", mv.getIsbn());
        assertEquals("Student's book", mv.getTitleInfo().getVolumeTitle());
        assertEquals("English file", mv.getTitleInfo().getMonographTitle());

        assertNotNull(imp.getDigitalDocument());
    }

    @Test
    public void testCreateCartographicImport() throws Exception {
        ModsDefinition titleMods = mods("registration/ndkCartographic.xml");
        MixType mix = mix("registration/mix.xml");
        Import imp = factory.createCartographicImport(titleMods, mix, null);
        assertNotNull(imp);
        OtherEntity entity = imp.getOtherEntity();
        assertNotNull(entity);
        assertEquals("cnb002500216", entity.getCcnb());
        assertEquals("cartographic", entity.getDocumentType());
        assertEquals("978-80-7324-393-7 (složeno)", entity.getIsbn());
        assertEquals("Krkonoše", entity.getTitleInfo().getTitle());
        assertEquals("turistická mapa 1:50 000", entity.getTitleInfo().getSubTitle());
        assertNotNull(imp.getDigitalDocument());
    }

    @Test
    public void testCreateSheetMusicImport() throws Exception {
        ModsDefinition titleMods = mods("registration/ndkSheetMusic.xml");
        MixType mix = mix("registration/mix.xml");
        Import imp = factory.createSheetMusicImport(titleMods, mix, null);
        assertNotNull(imp);
        OtherEntity entity = imp.getOtherEntity();
        assertNotNull(entity);
        assertEquals("cnb002405684", entity.getCcnb());
        assertEquals("sheetmusic", entity.getDocumentType());
        assertEquals("978-80-7262-933-6 (brož.)", entity.getIsbn());
        assertEquals("Xpěvník", entity.getTitleInfo().getTitle());
        assertNull(entity.getTitleInfo().getSubTitle());
        assertNotNull(imp.getDigitalDocument());
    }

    static ModsDefinition mods(String filename) {
        return JAXB.unmarshal(NdkEntityFactoryTest.class.getResource(filename), ModsDefinition.class);
    }

    static MixType mix(String filename) {
        return JAXB.unmarshal(NdkEntityFactoryTest.class.getResource(filename), MixType.class);
    }

}
