/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.catalog;

import java.util.HashMap;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class AlmaCatalogTest {

    private final AlmaCatalog catalog = new AlmaCatalog("", "", null, new HashMap<>(), null);

    @Test
    public void readsMarcFromAniesArray() throws Exception {
        JSONObject bib = new JSONObject("{\"anies\":[\"<record><leader>value</leader></record>\"]}");

        assertEquals("<record><leader>value</leader></record>", catalog.getMarcResult(bib));
    }

    @Test
    public void readsMarcFromLegacyString() throws Exception {
        JSONObject bib = new JSONObject("{\"anies\":\"<record/>\"}");

        assertEquals("<record/>", catalog.getMarcResult(bib));
    }

    @Test
    public void ignoresEmptyAniesArray() throws Exception {
        JSONObject bib = new JSONObject("{\"anies\":[]}");

        assertNull(catalog.getMarcResult(bib));
    }
}
