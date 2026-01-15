/*
 * Copyright (C) 2017 Lukas Sykora
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
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.StringReader;
import java.util.List;
import javax.xml.transform.stream.StreamSource;
import org.easymock.EasyMock;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 *
 * @author Lukas Sykora
 */
public class NdkPeriodicalVolumeMapperTest {

    public NdkPeriodicalVolumeMapperTest() {
    }

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    @BeforeEach
    public void setUp() {
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testCreateMods() {
        ModsDefinition mods = new ModsDefinition();
        NdkPeriodicalVolumeMapper mapper = new NdkPeriodicalVolumeMapper();
        Context ctx = EasyMock.createMock(Context.class);
        EasyMock.expect(ctx.getPid()).andReturn("uuid:testId").anyTimes();
        EasyMock.replay(ctx);

        mapper.createMods(mods, ctx);
        List<IdentifierDefinition> identifiersResult = mods.getIdentifier();
        assertEquals(1, identifiersResult.size());
        IdentifierDefinition idResult = identifiersResult.get(0);
        assertEquals("uuid", idResult.getTypeString());
        assertEquals("testId", idResult.getValue());

        assertEquals(1, mods.getGenre().size());
        assertEquals("volume", mods.getGenre().get(0).getValue());
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
                + "<name usage = 'primary' type='personal'>"
                + "<namePart>Archivist</namePart>"
                + "<namePart type='family'>Novotny</namePart>"
                + "<namePart type='given'>Tomas</namePart>"
                + "<namePart type='date'>12.9.1999</namePart>"
                + "<role><roleTerm>cre</roleTerm></role></name>"
                + "</mods>";
        ModsDefinition mods = ModsUtils.unmarshalModsType(new StreamSource(new StringReader(xml)));
        NdkPeriodicalMapper mapper = new NdkPeriodicalMapper();
        NdkMapper.Context ctx = new NdkMapper.Context("uuid:testId");

        OaiDcType result = mapper.createDc(mods, ctx);
        assertEquals("NS1 T1: S1", result.getTitles().get(0).getValue());
        assertEquals("T2: S2", result.getTitles().get(1).getValue());
        assertEquals("PNum1", result.getDescriptions().get(0).getValue());
        assertEquals("PNam1", result.getDescriptions().get(1).getValue());
        assertEquals("Archivist Novotny, Tomas, 12.9.1999", result.getCreators().get(0).getValue());
    }
}
