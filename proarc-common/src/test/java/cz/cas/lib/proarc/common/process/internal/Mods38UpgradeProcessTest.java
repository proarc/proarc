/*
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.process.internal;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class Mods38UpgradeProcessTest {

    @Test
    public void logsUnsupportedModelCounts() {
        Mods38UpgradeProcess.Result result = new Mods38UpgradeProcess.Result();
        result.addUnsupportedModel("model:proarcobject");
        result.addUnsupportedModel("model:other");
        result.addUnsupportedModel("model:proarcobject");

        assertEquals("MODS 3.8 upgrade finished:" +
                        System.lineSeparator() + " - converted=0" +
                        System.lineSeparator() + " - alreadyConverted=0" +
                        System.lineSeparator() + " - unsupportedModels=3" +
                        System.lineSeparator() + " - failed=0" +
                        System.lineSeparator() + System.lineSeparator() +
                        "Unsupported models:" + System.lineSeparator() +
                        " - model:other 1" + System.lineSeparator() +
                        " - model:proarcobject 2",
                result.toLog());
    }
}
