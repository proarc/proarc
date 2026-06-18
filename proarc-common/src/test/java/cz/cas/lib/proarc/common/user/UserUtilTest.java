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
package cz.cas.lib.proarc.common.user;

import java.util.Arrays;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class UserUtilTest {

    public UserUtilTest() {
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
    public void testToUserName() {
        assertEquals("ka_0_1_2-3_", UserUtil.toUserName("Kač0%1\"2-3."));
        assertEquals("u" + String.valueOf("###".hashCode()), UserUtil.toUserName("###"));
        assertEquals("a", UserUtil.toUserName("$a"));
        assertEquals("a", UserUtil.toUserName("$***a"));
        assertEquals("prefix_a", UserUtil.toUserName("prefix", "$a"));
        assertEquals("prefix_a_b", UserUtil.toUserName("prefix", "$***a+*b"));
        // long name
        char[] chars = new char[100];
        Arrays.fill(chars, 'A');
        String result = UserUtil.toUserName(String.valueOf(chars));
        assertTrue(UserUtil.isValidUserName(result), () -> result);
    }

    @Test
    public void testIsValidUserName() {
        assertTrue(UserUtil.isValidUserName("datel"));
        assertFalse(UserUtil.isValidUserName("group:remote_name"));
    }

    @Test
    public void testIsValidGroupPid() {
        assertTrue(UserUtil.isValidGroupPid("group:remote_name"));
        assertFalse(UserUtil.isValidGroupPid("datel"));
    }

}