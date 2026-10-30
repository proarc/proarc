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
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.mods.ndk.MapperUtils;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;
import java.util.ArrayList;
import java.util.function.Function;
import java.io.StringReader;
import javax.xml.transform.stream.StreamSource;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

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

    private ModsDefinition repeatedBlocks() throws Exception {
        String block = "<titleInfo><title>Old</title><title>Other</title>"
                + "<subTitle>Old</subTitle><subTitle>Other</subTitle>"
                + "<partName>Old</partName><partName>Other</partName>"
                + "<partNumber>1</partNumber><partNumber>2</partNumber></titleInfo>"
                + "<originInfo><place><placeTerm>Old</placeTerm><placeTerm>Other</placeTerm></place>"
                + "<place><placeTerm>Third</placeTerm></place>"
                + "<agent><namePart>Old</namePart><namePart>Other</namePart></agent>"
                + "<agent><namePart>Third</namePart></agent>"
                + "<dateIssued>2025</dateIssued><dateIssued>2026</dateIssued></originInfo>"
                + "<location><physicalLocation>Old</physicalLocation><physicalLocation>Other</physicalLocation>"
                + "<shelfLocator>Old</shelfLocator><shelfLocator>Other</shelfLocator></location>";
        return ModsUtils.unmarshalModsType(new StreamSource(new StringReader("<mods xmlns=\"http://www.loc.gov/mods/v3\">"
                + block + block + "<note type=\"existing\">Existing note</note></mods>")));
    }

    private void apply(ModsDefinition mods, String field, Object value) throws Exception {
        Field parameter = UpdateObjects.class.getDeclaredField(field);
        parameter.setAccessible(true);
        parameter.set(updateObjects, value);
        apply(mods);
    }

    private void apply(ModsDefinition mods) throws Exception {
        Method method = UpdateObjects.class.getDeclaredMethod("updateMods", ModsDefinition.class, String.class);
        method.setAccessible(true);
        method.invoke(updateObjects, mods, NdkPlugin.MODEL_PERIODICALISSUE);
    }

    private static <B, T> List<T> values(List<B> blocks, Function<B, List<T>> field) {
        List<T> result = new ArrayList<>();
        for (B block : blocks) {
            result.addAll(field.apply(block));
        }
        return result;
    }

    private static List<? extends StringPlusLanguage> selectedValues(ModsDefinition mods, String field) {
        switch (field) {
            case "titleValue": return values(mods.getTitleInfo(), TitleInfoDefinition::getTitle);
            case "subTitleValue": return values(mods.getTitleInfo(), TitleInfoDefinition::getSubTitle);
            case "partNameValue": return values(mods.getTitleInfo(), TitleInfoDefinition::getPartName);
            case "partNumberValue": return values(mods.getTitleInfo(), TitleInfoDefinition::getPartNumber);
            case "dateIssuedValue": return values(mods.getOriginInfo(), OriginInfoDefinition::getDateIssued);
            case "siglaValue": return values(mods.getLocation(), LocationDefinition::getPhysicalLocation);
            case "signaturaValue": return values(mods.getLocation(), LocationDefinition::getShelfLocator);
            case "agentValue": return values(values(mods.getOriginInfo(), OriginInfoDefinition::getAgent), NameDefinition::getNamePart);
            case "placeValue": return values(values(mods.getOriginInfo(), OriginInfoDefinition::getPlace), PlaceDefinition::getPlaceTerm);
            default: throw new IllegalArgumentException(field);
        }
    }

    private static final List<String> FIELDS = List.of("titleValue", "subTitleValue", "partNameValue",
            "partNumberValue", "dateIssuedValue", "siglaValue", "signaturaValue", "agentValue", "placeValue");

    @ParameterizedTest
    @ValueSource(strings = {"titleValue", "subTitleValue", "partNameValue", "partNumberValue",
            "dateIssuedValue", "siglaValue", "signaturaValue", "agentValue", "placeValue"})
    public void replacesEveryOccurrenceAndPreservesUnselectedFields(String field) throws Exception {
        ModsDefinition mods = repeatedBlocks();
        ModsDefinition original = repeatedBlocks();
        Object value = "partNumberValue".equals(field) ? 10 : "New value";
        apply(mods, field, value);

        List<? extends StringPlusLanguage> result = selectedValues(mods, field);
        assertEquals(2, result.size());
        for (StringPlusLanguage item : result) {
            assertEquals(String.valueOf(value), item.getValue());
        }
        for (int i = 0; i < 2; i++) {
            ModsDefinition singleBlock = new ModsDefinition();
            singleBlock.getOriginInfo().add(mods.getOriginInfo().get(i));
            singleBlock.getTitleInfo().add(mods.getTitleInfo().get(i));
            singleBlock.getLocation().add(mods.getLocation().get(i));
            assertEquals(1, selectedValues(singleBlock, field).size());
        }
        for (String other : FIELDS) {
            if (!other.equals(field)) {
                assertEquals(selectedValues(original, other).stream().map(StringPlusLanguage::getValue).toList(),
                        selectedValues(mods, other).stream().map(StringPlusLanguage::getValue).toList(), other);
            }
        }
        assertEquals("Existing note", mods.getNote().get(0).getValue());
    }

    @Test
    public void replacesPublishersWithinEachOriginAndPreservesAttributes() throws Exception {
        ModsDefinition mods = repeatedBlocks();
        OriginInfoDefinition firstOrigin = mods.getOriginInfo().get(0);
        OriginInfoDefinition secondOrigin = mods.getOriginInfo().get(1);
        firstOrigin.setEventType("publication");
        secondOrigin.setEventType("production");
        NameDefinition firstAgent = firstOrigin.getAgent().get(0);
        NameDefinition secondAgent = secondOrigin.getAgent().get(0);
        firstAgent.setAuthority("first authority");
        secondAgent.setAuthority("second authority");

        apply(mods, "agentValue", "New publisher");

        assertEquals(2, mods.getOriginInfo().size());
        assertEquals(firstOrigin, mods.getOriginInfo().get(0));
        assertEquals(secondOrigin, mods.getOriginInfo().get(1));
        assertEquals(1, firstOrigin.getAgent().size());
        assertEquals(1, secondOrigin.getAgent().size());
        assertEquals(firstAgent, firstOrigin.getAgent().get(0));
        assertEquals(secondAgent, secondOrigin.getAgent().get(0));
        assertEquals("first authority", firstAgent.getAuthority());
        assertEquals("second authority", secondAgent.getAuthority());
        assertEquals("publisher", firstAgent.getRole().get(0).getRoleTerm().get(0).getValue());
        assertEquals("producer", secondAgent.getRole().get(0).getRoleTerm().get(0).getValue());
        assertEquals(1, firstAgent.getNamePart().size());
        assertEquals(1, secondAgent.getNamePart().size());
        assertEquals("New publisher", firstAgent.getNamePart().get(0).getValue());
        assertEquals("New publisher", secondAgent.getNamePart().get(0).getValue());
    }

    @ParameterizedTest
    @ValueSource(strings = {"titleValue", "subTitleValue", "partNameValue", "partNumberValue",
            "dateIssuedValue", "siglaValue", "signaturaValue", "agentValue", "placeValue"})
    public void collapsesRepeatedValuesWithinSingleBlockAndPreservesAttributes(String field) throws Exception {
        ModsDefinition mods = repeatedBlocks();
        mods.getOriginInfo().remove(1);
        mods.getTitleInfo().remove(1);
        mods.getLocation().remove(1);
        StringPlusLanguage first = selectedValues(mods, field).get(0);
        first.setLang("cs");

        Object value = "partNumberValue".equals(field) ? 10 : "New value";
        apply(mods, field, value);

        assertEquals(1, mods.getOriginInfo().size());
        assertEquals(1, selectedValues(mods, field).size());
        assertEquals(first, selectedValues(mods, field).get(0));
        assertEquals("cs", selectedValues(mods, field).get(0).getLang());
        assertEquals(String.valueOf(value), selectedValues(mods, field).get(0).getValue());
    }

    @Test
    public void appendsNoteOnceAndPreservesExistingNotesAndAttributes() throws Exception {
        ModsDefinition mods = repeatedBlocks();
        NoteDefinition existing = mods.getNote().get(0);
        apply(mods, "noteValue", "New note");
        apply(mods, "noteValue", "New note");
        apply(mods, "noteValue", "Existing note");
        assertEquals(2, mods.getNote().size());
        assertEquals(existing, mods.getNote().get(0));
        assertEquals("existing", existing.getType());
        assertEquals("Existing note", existing.getValue());
        assertEquals("New note", mods.getNote().get(1).getValue());
    }

    @Test
    public void partNumberIncrementsOncePerObject() throws Exception {
        ModsDefinition first = repeatedBlocks();
        ModsDefinition second = repeatedBlocks();
        apply(first, "partNumberValue", 10);
        apply(second);
        assertEquals("10", selectedValues(first, "partNumberValue").get(0).getValue());
        assertEquals("10", selectedValues(first, "partNumberValue").get(1).getValue());
        assertEquals("11", selectedValues(second, "partNumberValue").get(0).getValue());
        assertEquals("11", selectedValues(second, "partNumberValue").get(1).getValue());
    }

    @Test
    public void locationNormalizationDoesNotDuplicateReplacedSigla() throws Exception {
        ModsDefinition mods = repeatedBlocks();
        apply(mods, "siglaValue", "ABA001");
        MapperUtils.fillLocation(mods);
        assertEquals(2, selectedValues(mods, "siglaValue").size());
        for (LocationDefinition location : mods.getLocation()) {
            assertEquals(1, location.getPhysicalLocation().size());
            assertEquals("ABA001", location.getPhysicalLocation().get(0).getValue());
            assertEquals("siglaADR", location.getPhysicalLocation().get(0).getAuthority());
            assertEquals(2, location.getShelfLocator().size());
        }
        assertEquals(4, selectedValues(mods, "signaturaValue").size());
    }

    @ParameterizedTest
    @ValueSource(strings = {"titleValue", "subTitleValue", "partNameValue", "partNumberValue",
            "dateIssuedValue", "siglaValue", "signaturaValue", "agentValue", "placeValue", "noteValue"})
    public void createsMissingFields(String field) throws Exception {
        ModsDefinition mods = new ModsDefinition();
        Object value = "partNumberValue".equals(field) ? 10 : "New value";
        apply(mods, field, value);
        if ("noteValue".equals(field)) {
            assertEquals("New value", mods.getNote().get(0).getValue());
        } else {
            assertEquals(1, selectedValues(mods, field).size());
            assertEquals(String.valueOf(value), selectedValues(mods, field).get(0).getValue());
        }
    }

    @Test
    public void missingParametersLeaveMetadataUntouched() throws Exception {
        ModsDefinition mods = repeatedBlocks();
        String original = ModsUtils.toXml(mods, false);
        apply(mods);
        assertEquals(original, ModsUtils.toXml(mods, false));
    }
}
