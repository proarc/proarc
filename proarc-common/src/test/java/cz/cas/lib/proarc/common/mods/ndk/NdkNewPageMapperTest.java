/*
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.mods.ndk;

import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.mods.ModsDefinition;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class NdkNewPageMapperTest {

    @Test
    public void keepsMissingPageTypeEmpty() {
        NdkNewPageMapper mapper = new NdkNewPageMapper();

        ModsDefinition mods = mapper.createPage("1", "[1]", null, new Context("uuid:test"));

        assertNull(mapper.getType(mods));
        NdkMapper.PageModsWrapper wrapper = (NdkMapper.PageModsWrapper) mapper.toJsonObject(
                mods, new Context("uuid:test"));
        assertNull(wrapper.getPageType());
    }

    @Test
    public void keepsExplicitNormalPageType() {
        NdkNewPageMapper mapper = new NdkNewPageMapper();

        ModsDefinition mods = mapper.createPage("1", "[1]", "normalPage", new Context("uuid:test"));

        assertEquals("normalPage", mapper.getType(mods));
    }
}
