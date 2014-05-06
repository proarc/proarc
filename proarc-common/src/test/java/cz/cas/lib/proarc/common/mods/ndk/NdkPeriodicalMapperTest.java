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

import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.IssuanceDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import java.util.Arrays;
import java.util.List;
import org.easymock.EasyMock;
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

        mapper.createMods(mods, ctx);
        PlaceDefinition place = new PlaceDefinition();
        PlaceTermDefinition placeTerm = new PlaceTermDefinition();
        placeTerm.setValue("place");
        place.getPlaceTerm().add(placeTerm);
        mods.getOriginInfo().get(0).getPlace().add(place);

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
        mods.getOriginInfo().add(oi);

        mapper.createMods(mods, ctx);

        assertEquals(Arrays.asList(IssuanceDefinition.CONTINUING), mods.getOriginInfo().get(0).getIssuance());
    }

}