/*
 * Copyright (C) 2018 Lukas Sykora
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
package cz.cas.lib.proarc.common.object.ndk;

import java.util.Locale;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author lukas.sykora
 */
public class NdkAudioPageMapperTest {

    public NdkAudioPageMapperTest() {
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
    public void testGetPageTypeLabel() {
        Locale.setDefault(Locale.ENGLISH);
        assertEquals("Normal Page", NdkAudioPageMapper.getPageTypeLabel("normalPage", Locale.getDefault()));
        assertEquals("NormalPage", NdkAudioPageMapper.getPageTypeLabel("", Locale.getDefault()));

        Locale.setDefault(new Locale("cs", "CZ"));
        assertEquals("Normální strana", NdkAudioPageMapper.getPageTypeLabel("normalPage", Locale.getDefault()));
        assertEquals("NormalPage", NdkAudioPageMapper.getPageTypeLabel("", Locale.getDefault()));
    }
}
