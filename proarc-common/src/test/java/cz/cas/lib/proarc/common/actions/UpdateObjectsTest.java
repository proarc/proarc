/*
 * Copyright (C) 2026 Lukas Sykora
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
package cz.cas.lib.proarc.common.actions;

import cz.cas.lib.proarc.common.storage.DigitalObjectValidationException;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import static cz.cas.lib.proarc.common.object.ndk.ModsRules.ERR_NDK_ORIGININFO_DATEISSSUED;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class UpdateObjectsTest {

    private final UpdateObjects updateObjects = new UpdateObjects(null, null, null);

    @ParameterizedTest
    @MethodSource("validDates")
    public void createDateIssuedAcceptsValidDate(String dateIssued) {
        assertDoesNotThrow(() -> updateObjects.createDateIssued(dateIssued));
    }

    private static Stream<String> validDates() {
        return Stream.of(
                "2026",
                "02.2026",
                "28.02.2026",
                "01.-02.2026",
                "12.2025-01.2026",
                "28.02.-01.03.2026",
                "28.02.2026-01.03.2026",
                "01.-02.03.2026"
        );
    }

    @ParameterizedTest
    @MethodSource("invalidDates")
    public void createDateIssuedRejectsInvalidDate(String dateIssued) {
        DigitalObjectValidationException exception = assertThrows(DigitalObjectValidationException.class,
                () -> updateObjects.createDateIssued(dateIssued));

        assertEquals(1, exception.getValidations().size());
        assertEquals(ERR_NDK_ORIGININFO_DATEISSSUED, exception.getValidations().get(0).getBundleKey());
        assertEquals(dateIssued, exception.getValidations().get(0).getValues()[0]);
    }

    private static Stream<String> invalidDates() {
        return Stream.of(
                "2026-02-28",
                "31.02.2026",
                "13.2026",
                "03.2026-02.2026",
                "02.03.2026-01.03.2026"
        );
    }

    @Test
    public void createDateIssuedAcceptsMissingDate() {
        assertDoesNotThrow(() -> updateObjects.createDateIssued(null));
        assertDoesNotThrow(() -> updateObjects.createDateIssued(""));
    }
}
