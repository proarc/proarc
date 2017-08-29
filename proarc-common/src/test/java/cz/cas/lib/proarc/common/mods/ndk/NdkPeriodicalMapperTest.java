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
package cz.cas.lib.proarc.common.mods.ndk;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.DateOtherDefinition;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.IssuanceDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.StringReader;
import java.util.Arrays;
import java.util.List;
import javax.xml.transform.stream.StreamSource;
import org.easymock.EasyMock;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class NdkPeriodicalMapperTest {

    public NdkPeriodicalMapperTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testCreateMods() {
        ModsDefinition mods = new ModsDefinition();
        NdkPeriodicalMapper mapper = new NdkPeriodicalMapper();
        Context ctx = EasyMock.createMock(Context.class);
        EasyMock.expect(ctx.getPid()).andReturn("uuid:testId").anyTimes();
        EasyMock.replay(ctx);

        addDescriptionStandard(mods, "aacr");

        mapper.createMods(mods, ctx);
        List<IdentifierDefinition> identifiersResult = mods.getIdentifier();
        assertEquals(1, identifiersResult.size());
        IdentifierDefinition idResult = identifiersResult.get(0);
        assertEquals("uuid", idResult.getType());
        assertEquals("testId", idResult.getValue());

        assertEquals(1, mods.getTypeOfResource().size());
        assertEquals("text", mods.getTypeOfResource().get(0).getValue());

        assertEquals(1, mods.getGenre().size());
        assertEquals("title", mods.getGenre().get(0).getValue());

        assertEquals(1, mods.getOriginInfo().size());
        assertEquals(1, mods.getOriginInfo().get(0).getIssuance().size());
        assertEquals(IssuanceDefinition.CONTINUING, mods.getOriginInfo().get(0).getIssuance().get(0));
        assertEquals(0, mods.getOriginInfo().get(0).getPlace().size());
    }

    @Test
    public void testCreateMods_UpdateMods() {
        ModsDefinition mods = new ModsDefinition();
        NdkPeriodicalMapper mapper = new NdkPeriodicalMapper();
        Context ctx = EasyMock.createMock(Context.class);
        EasyMock.expect(ctx.getPid()).andReturn("uuid:testId").anyTimes();
        EasyMock.replay(ctx);

        addDescriptionStandard(mods, "aacr");

        mapper.createMods(mods, ctx);
        PlaceDefinition place = new PlaceDefinition();
        PlaceTermDefinition placeTerm = new PlaceTermDefinition();
        placeTerm.setValue("place");
        place.getPlaceTerm().add(placeTerm);
        mods.getOriginInfo().get(0).getPlace().add(place);
        DateDefinition date = new DateDefinition();
        date.setValue("2017");
        date.setEncoding("MARC21");
        mods.getOriginInfo().get(0).getDateIssued().add(date);

        mapper.createMods(mods, ctx);

        List<IdentifierDefinition> identifiersResult = mods.getIdentifier();
        assertEquals(1, identifiersResult.size());
        IdentifierDefinition idResult = identifiersResult.get(0);
        assertEquals("uuid", idResult.getType());
        assertEquals("testId", idResult.getValue());

        assertEquals(1, mods.getTypeOfResource().size());
        assertEquals("text", mods.getTypeOfResource().get(0).getValue());

        assertEquals(1, mods.getGenre().size());
        assertEquals("title", mods.getGenre().get(0).getValue());

        assertEquals(1, mods.getOriginInfo().size());
        assertEquals(1, mods.getOriginInfo().get(0).getIssuance().size());
        assertEquals(IssuanceDefinition.CONTINUING, mods.getOriginInfo().get(0).getIssuance().get(0));
        assertEquals(1, mods.getOriginInfo().get(0).getPlace().size());
        assertEquals(1, mods.getOriginInfo().get(0).getPlace().get(0).getPlaceTerm().size());
        assertEquals(CodeOrText.TEXT, mods.getOriginInfo().get(0).getPlace().get(0).getPlaceTerm().get(0).getType());
    }

    @Test
    public void testCreateMods_FixIssuance() {
        ModsDefinition mods = new ModsDefinition();
        NdkPeriodicalMapper mapper = new NdkPeriodicalMapper();
        Context ctx = EasyMock.createMock(Context.class);
        EasyMock.expect(ctx.getPid()).andReturn("uuid:testId").anyTimes();
        EasyMock.replay(ctx);

        OriginInfoDefinition oi = new OriginInfoDefinition();
        oi.getIssuance().add(IssuanceDefinition.SERIAL);
        DateDefinition date = new DateDefinition();
        date.setValue("123");
        date.setEncoding("MARC21");
        oi.getDateIssued().add(date);
        mods.getOriginInfo().add(oi);
        addDescriptionStandard(mods, "aacr");

        mapper.createMods(mods, ctx);

        assertEquals(Arrays.asList(IssuanceDefinition.CONTINUING), mods.getOriginInfo().get(0).getIssuance());
    }

    @Test
    public void testCreateLabel() {
        String xml = "<mods version='3.5' xmlns='http://www.loc.gov/mods/v3'>"
                + "<titleInfo>"
                    + "<nonSort>NS1</nonSort><title>T1</title><subTitle>S1</subTitle><partName>PNam1</partName><partNumber>PNum1</partNumber>"
                + "</titleInfo>"
                + "<titleInfo>"
                    + "<title>T2</title><subTitle>S2</subTitle>"
                + "</titleInfo>"
                + "</mods>";
        ModsDefinition mods = ModsUtils.unmarshalModsType(new StreamSource(new StringReader(xml)));
        NdkPeriodicalMapper mapper = new NdkPeriodicalMapper();

        String result = mapper.createObjectLabel(mods);
        assertEquals("NS1 T1: S1. PNum1. PNam1", result);
    }

    @Test
    public void testCreateDc() {
        String xml = "<mods version='3.5' xmlns='http://www.loc.gov/mods/v3'>"
                + "<titleInfo>"
                    + "<nonSort>NS1</nonSort><title>T1</title><subTitle>S1</subTitle><partName>PNam1</partName><partNumber>PNum1</partNumber>"
                + "</titleInfo>"
                + "<titleInfo>"
                    + "<title>T2</title><subTitle>S2</subTitle>"
                + "</titleInfo>"
                + "</mods>";
        ModsDefinition mods = ModsUtils.unmarshalModsType(new StreamSource(new StringReader(xml)));
        NdkPeriodicalMapper mapper = new NdkPeriodicalMapper();
        Context ctx = new Context("uuid:testId");

        OaiDcType result = mapper.createDc(mods, ctx);
        assertEquals("NS1 T1: S1", result.getTitles().get(0).getValue());
        assertEquals("T2: S2", result.getTitles().get(1).getValue());
        assertEquals("PNum1", result.getDescriptions().get(0).getValue());
        assertEquals("PNam1", result.getDescriptions().get(1).getValue());

    }

    @Test
    public void testCheckOriginInfo() {
        ModsDefinition mods = new ModsDefinition();
        NdkPeriodicalMapper mapper = new NdkPeriodicalMapper();
        Context ctx = EasyMock.createMock(Context.class);
        EasyMock.expect(ctx.getPid()).andReturn("uuid:testId").anyTimes();
        EasyMock.replay(ctx);

        addDescriptionStandard(mods,"rda");

        OriginInfoDefinition publication = new OriginInfoDefinition();
        publication.setEventType("publication");
        DateDefinition datePublication = new DateDefinition();
        datePublication.setValue("2007");
        datePublication.setEncoding("MARC21");
        publication.getDateIssued().add(datePublication);

        OriginInfoDefinition manufacture = new OriginInfoDefinition();
        manufacture.setEventType("manufacture");
        DateOtherDefinition dateManufacture = new DateOtherDefinition();
        dateManufacture.setValue("2006");
        manufacture.getDateOther().add(dateManufacture);

        OriginInfoDefinition copyright = new OriginInfoDefinition();
        copyright.setEventType("copyright");
        DateDefinition dateCopyright = new DateDefinition();
        dateCopyright.setValue("2008");
        copyright.getCopyrightDate().add(dateCopyright);

        mods.getOriginInfo().add(publication);
        mods.getOriginInfo().add(manufacture);
        mods.getOriginInfo().add(copyright);

        mapper.createMods(mods, ctx);
    }

    @Test
    public void testInvalidCheckOriginInfo() {
        ModsDefinition mods = new ModsDefinition();
        NdkPeriodicalMapper mapper = new NdkPeriodicalMapper();
        Context ctx = EasyMock.createMock(Context.class);
        EasyMock.expect(ctx.getPid()).andReturn("uuid:testId").anyTimes();
        EasyMock.replay(ctx);

        addDescriptionStandard(mods, "rda");

        OriginInfoDefinition emptyPublication = new OriginInfoDefinition();
        emptyPublication.setEventType("publication");
        DateDefinition datePublication = new DateDefinition();
        datePublication.setEncoding("MARC");
        emptyPublication.getDateIssued().add(datePublication);
        mods.getOriginInfo().add(emptyPublication);

        try{
            mapper.createMods(mods, ctx);
            Assert.fail("The validation error expected");
        } catch (IllegalArgumentException ex){
            String message = "Pole dateIssued nesmí být prázdné, pokud je vyplněno eventType = publication";
            assertEquals(message, ex.getMessage());
            mods.getOriginInfo().clear();
        }

        OriginInfoDefinition invalidPublication = new OriginInfoDefinition();
        invalidPublication.setEventType("publication");
        DateDefinition date = new DateDefinition();
        date.setValue("2007");
        date.setEncoding("MARC21");
        invalidPublication.getDateIssued().add(date);
        DateOtherDefinition dateInvalidPublication = new DateOtherDefinition();
        dateInvalidPublication.setValue("2001");
        invalidPublication.getDateOther().add(dateInvalidPublication);
        mods.getOriginInfo().add(invalidPublication);

        try{
            mapper.createMods(mods, ctx);
            Assert.fail("The validation error expected");
        } catch (IllegalArgumentException ex){
            String message = "Pole dateOther musí být prázdné, pokud je vyplněno eventType = publication";
            assertEquals(message, ex.getMessage());
            mods.getOriginInfo().clear();
        }
    }

    @Test
    public void testCheckRules(){
        ModsDefinition mods = new ModsDefinition();
        NdkPeriodicalMapper mapper = new NdkPeriodicalMapper();
        Context ctx = EasyMock.createMock(Context.class);
        EasyMock.expect(ctx.getPid()).andReturn("uuid:testId").anyTimes();
        EasyMock.replay(ctx);

        OriginInfoDefinition publication = new OriginInfoDefinition();
        publication.setEventType("publication");
        DateDefinition date = new DateDefinition();
        date.setValue("2007");
        date.setEncoding("MARC21");
        publication.getDateIssued().add(date);
        mods.getOriginInfo().add(publication);
        addPhysicalDescription(mods, "rdamedia", "bez media");
        addDescriptionStandard(mods,"rda");
        mapper.createMods(mods, ctx);

        try {
            addDescriptionStandard(mods, "aacr");
            mapper.createMods(mods, ctx);
            Assert.fail("Validation error expected");
        }catch (IllegalArgumentException ex){
            String message = "Pokud se zpracovává podle pravidel \"AACR\" potom musí být pole eventType prázdné.";
            assertEquals(message, ex.getMessage());
            mods.getOriginInfo().clear();
        }

        OriginInfoDefinition emptyOriginInfo = new OriginInfoDefinition();
        emptyOriginInfo.getDateIssued().add(date);
        mods.getOriginInfo().add(emptyOriginInfo);

        addPhysicalDescription(mods, "gmd", "print");
        addDescriptionStandard(mods, "aacr");
        mapper.createMods(mods, ctx);

        try {
            addDescriptionStandard(mods, "rda");
            mapper.createMods(mods, ctx);
            Assert.fail("Validation error expected");
        }catch (IllegalArgumentException ex){
            String message = "Pokud se zpracovává podle pravidel \"RDA\" potom v elementu physicalDescription musí být pole hodnota \"rdamedia\" nebo \"rdacarrier\".";
            assertEquals(message, ex.getMessage());
        }
    }

    /** Sets PhysicalDescriptions */
    private void addPhysicalDescription(ModsDefinition mods, String authority, String value){
        mods.getPhysicalDescription().clear();
        FormDefinition form = new FormDefinition();
        form.setValue(value);
        form.setAuthority(authority);
        PhysicalDescriptionDefinition pd = new PhysicalDescriptionDefinition();
        pd.getForm().add(form);
        mods.getPhysicalDescription().add(pd);
    }

    /** Sets DescriptionStandard */
    private void addDescriptionStandard(ModsDefinition mods, String rules) {
        mods.getRecordInfo().clear();
        RecordInfoDefinition recordInfo = new RecordInfoDefinition();
        StringPlusLanguagePlusAuthority descriptionStandard = new StringPlusLanguagePlusAuthority();
        descriptionStandard.setValue(rules);
        recordInfo.getDescriptionStandard().add(descriptionStandard);
        mods.getRecordInfo().add(recordInfo);
    }
}