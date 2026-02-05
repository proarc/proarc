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
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 *
 * @author lukas.sykora
 */
public class NdkAudioPageMapperTest {

    public NdkAudioPageMapperTest() {
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
    public void testGetPageTypeLabel() {
        Locale.setDefault(Locale.ENGLISH);
        assertEquals("Normal Page", NdkAudioPageMapper.getPageTypeLabel("normalPage", Locale.getDefault()));
        assertEquals("NormalPage", NdkAudioPageMapper.getPageTypeLabel("", Locale.getDefault()));

        Locale.setDefault(new Locale("cs", "CZ"));
        assertEquals("Normální strana", NdkAudioPageMapper.getPageTypeLabel("normalPage", Locale.getDefault()));
        assertEquals("NormalPage", NdkAudioPageMapper.getPageTypeLabel("", Locale.getDefault()));
    }
}
