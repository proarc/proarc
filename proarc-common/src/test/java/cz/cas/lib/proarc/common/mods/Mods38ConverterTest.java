/*
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.mods;

import cz.cas.lib.proarc.common.mods.Mods38Converter.ConversionException;
import cz.cas.lib.proarc.common.mods.Mods38Converter.Status;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PublisherDefinition;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class Mods38ConverterTest {

    @Test
    public void convertsPublisherToAgent() throws Exception {
        ModsDefinition mods = mods("3.4", ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION);

        assertEquals(Status.CONVERTED,
                Mods38Converter.convertAndValidate(mods, "model:ndkmonographvolume"));

        OriginInfoDefinition originInfo = mods.getOriginInfo().get(0);
        assertEquals("3.8", mods.getVersion());
        assertTrue(originInfo.getPublisher().isEmpty());
        assertEquals("Publisher", originInfo.getAgent().get(0).getNamePart().get(0).getValue());
        assertEquals("producer",
                originInfo.getAgent().get(0).getRole().get(0).getRoleTerm().get(0).getValue());
    }

    @Test
    public void usesPublisherRoleForEmptyEventType() throws Exception {
        ModsDefinition mods = mods("3.6", " ");

        Mods38Converter.convertAndValidate(mods, "model:oldprintvolume");

        assertNull(mods.getOriginInfo().get(0).getEventType());
        assertEquals("publisher", mods.getOriginInfo().get(0).getAgent().get(0)
                .getRole().get(0).getRoleTerm().get(0).getValue());
    }

    @Test
    public void rejectsUnknownEventType() {
        ModsDefinition mods = mods("3.4", "unknown");
        assertThrows(ConversionException.class,
                () -> Mods38Converter.convertAndValidate(mods, "model:chroniclevolume"));
    }

    @Test
    public void rejectsUnsupportedVersion() {
        ModsDefinition mods = mods("3.5", null);
        assertThrows(ConversionException.class,
                () -> Mods38Converter.convertAndValidate(mods, "model:ndkpage"));
    }

    @Test
    public void skipsUnsupportedModel() throws Exception {
        ModsDefinition mods = mods("3.4", null);
        assertEquals(Status.UNSUPPORTED_MODEL,
                Mods38Converter.convertAndValidate(mods, "model:proarcobject"));
        assertEquals("3.4", mods.getVersion());
    }

    @Test
    public void acceptsValidCurrentVersionWithoutChangingIt() throws Exception {
        ModsDefinition mods = mods("3.8", null);

        assertEquals(Status.ALREADY_CONVERTED,
                Mods38Converter.convertAndValidate(mods, "model:ndkearticle"));
        assertEquals(1, mods.getOriginInfo().get(0).getPublisher().size());
        assertTrue(mods.getOriginInfo().get(0).getAgent().isEmpty());
    }

    @Test
    public void rejectsInvalidCurrentVersion() {
        ModsDefinition mods = mods("3.8", null);
        NameDefinition invalidAgent = new NameDefinition();
        invalidAgent.setType("invalid");
        mods.getOriginInfo().get(0).getAgent().add(invalidAgent);

        assertThrows(ConversionException.class,
                () -> Mods38Converter.convertAndValidate(mods, "model:ndkpage"));
    }

    private static ModsDefinition mods(String version, String eventType) {
        ModsDefinition mods = new ModsDefinition();
        mods.setVersion(version);
        OriginInfoDefinition originInfo = new OriginInfoDefinition();
        originInfo.setEventType(eventType);
        PublisherDefinition publisher = new PublisherDefinition();
        publisher.setValue("Publisher");
        originInfo.getPublisher().add(publisher);
        mods.getOriginInfo().add(originInfo);
        return mods;
    }
}
