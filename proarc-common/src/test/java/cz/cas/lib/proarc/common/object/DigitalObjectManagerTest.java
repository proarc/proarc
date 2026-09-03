/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class DigitalObjectManagerTest {

    @Test
    public void issueSeriesUsesQuestionMarkWhenPartNumberIsMissing() throws Exception {
        AtomicInteger calls = new AtomicInteger();
        DigitalObjectManager.CreateHandler handler = createHandler(calls, true, null);

        Field paramsField = DigitalObjectManager.CreateHandler.class.getDeclaredField("params");
        paramsField.setAccessible(true);
        Map<?, ?> params = (Map<?, ?>) paramsField.get(handler);

        assertEquals("?", params.get(DigitalObjectHandler.PARAM_PART_NUMBER));
    }

    @Test
    public void issueSeriesReturnsObjectsCreatedBeforeValidationError() throws Exception {
        AtomicInteger calls = new AtomicInteger();
        DigitalObjectManager.CreateHandler handler = createHandler(calls, true, 1);

        List<SearchViewItem> result = handler.create(true, true, null);

        assertEquals(2, result.size());
        assertEquals("uuid:1", result.get(0).getPid());
        assertEquals("uuid:2", result.get(1).getPid());
        assertEquals(3, calls.get());
    }

    @Test
    public void issueSeriesStillPropagatesTechnicalErrors() {
        AtomicInteger calls = new AtomicInteger();
        DigitalObjectManager.CreateHandler handler = createHandler(calls, false, 1);

        assertThrows(DigitalObjectException.class, () -> handler.create(true, true, null));
        assertEquals(3, calls.get());
    }

    private DigitalObjectManager.CreateHandler createHandler(
            AtomicInteger calls, boolean validationError, Integer partNumberFrom) {
        DigitalObjectManager manager = new DigitalObjectManager(null, null, null, null, null);
        UserProfile user = new UserProfile();
        user.setUserName("junit");
        DigitalObjectManager.CreateHandler handler = manager.new CreateHandler(
                "model:test", null, null, user, null, "") {
            @Override
            public SearchViewItem createDigitalObject(boolean createObject, boolean validation, String catalogId)
                    throws DigitalObjectException {
                int call = calls.incrementAndGet();
                if (call == 3) {
                    if (validationError) {
                        throw new DigitalObjectValidationException(
                                "uuid:3", null, "BIBLIO_MODS", "Invalid date issued", null);
                    }
                    throw new DigitalObjectException("uuid:3", "Storage failure");
                }
                return new SearchViewItem("uuid:" + call);
            }
        };
        return handler.issueSeries(null, null, Collections.emptyList(), false, Collections.emptyList(), partNumberFrom,
                "other", null, null, 4);
    }
}
