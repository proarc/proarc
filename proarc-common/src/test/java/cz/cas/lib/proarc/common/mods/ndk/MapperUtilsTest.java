/*
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.mods.ndk;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import cz.cas.lib.proarc.common.xml.Transformers;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import java.io.StringReader;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import javax.xml.transform.stream.StreamSource;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class MapperUtilsTest {

    @ParameterizedTest
    @CsvSource({
        "20260603074411.0, 2026-06-03T07:44:11.000",
        "20260603074411.1, 2026-06-03T07:44:11.100",
        "20260603074411.123, 2026-06-03T07:44:11.123",
        "20260603074411., 2026-06-03T07:44:11.000",
        "20260603074411, 2026-06-03T07:44:11.000",
        "20260603T074411.0, 2026-06-03T07:44:11.000",
        "20260603T074411.1, 2026-06-03T07:44:11.100",
        "20260603T074411, 20260603T074411",
        "2026-06-03T07:44:11.000+02:00, 2026-06-03T07:44:11.000+02:00",
        "2026-06-03T07:44:11Z, 2026-06-03T07:44:11Z",
        "20260230074411.0, 20260230074411.0"
    })
    public void testFillRecordInfoNormalizesCatalogTimestamps(String value, String expected) {
        ModsDefinition mods = new ModsDefinition();
        RecordInfoDefinition recordInfo = new RecordInfoDefinition();
        DateDefinition creationDate = new DateDefinition();
        creationDate.setEncoding("iso8601");
        creationDate.setValue(value);
        recordInfo.getRecordCreationDate().add(creationDate);
        DateDefinition changeDate = new DateDefinition();
        changeDate.setEncoding("iso8601");
        changeDate.setValue(value);
        recordInfo.getRecordChangeDate().add(changeDate);
        mods.getRecordInfo().add(recordInfo);

        MapperUtils.fillRecordInfo(mods, NdkMetadataHandler.OPERATION_NEW);

        assertEquals(expected, recordInfo.getRecordCreationDate().get(0).getValue());
        assertEquals(expected, recordInfo.getRecordChangeDate().get(0).getValue());
        assertEquals("iso8601", recordInfo.getRecordChangeDate().get(0).getEncoding());
    }

    @Test
    public void testFillRecordInfoNormalizesImportedMarcTimestamp() throws Exception {
        String marc = "<record xmlns=\"http://www.loc.gov/MARC21/slim\">"
                + "<leader>00000nam a2200000 a 4500</leader>"
                + "<controlfield tag=\"005\">20260603074411.0</controlfield>"
                + "</record>";
        ModsDefinition mods = ModsUtils.unmarshalModsType(new Transformers(null).transform(
                new StreamSource(new StringReader(marc)), Transformers.Format.MarcxmlAsMods3));

        MapperUtils.fillRecordInfo(mods, NdkMetadataHandler.OPERATION_NEW);

        assertEquals("2026-06-03T07:44:11.000",
                mods.getRecordInfo().get(0).getRecordChangeDate().get(0).getValue());
    }

    @Test
    public void testFillRecordInfoWithMarcDate() {
        ModsDefinition mods = new ModsDefinition();
        RecordInfoDefinition recordInfo = new RecordInfoDefinition();
        DateDefinition creationDate = new DateDefinition();
        creationDate.setEncoding("marc");
        creationDate.setValue("240229");
        recordInfo.getRecordCreationDate().add(creationDate);
        DateDefinition changeDate = new DateDefinition();
        changeDate.setEncoding("marc");
        changeDate.setValue("261029");
        recordInfo.getRecordChangeDate().add(changeDate);
        mods.getRecordInfo().add(recordInfo);

        MapperUtils.fillRecordInfo(mods, NdkMetadataHandler.OPERATION_NEW);

        assertEquals(1, recordInfo.getRecordCreationDate().size());
        assertEquals("iso8601", recordInfo.getRecordCreationDate().get(0).getEncoding());
        assertEquals("20240229T000000", recordInfo.getRecordCreationDate().get(0).getValue());
        assertEquals(1, recordInfo.getRecordChangeDate().size());
        assertEquals("iso8601", recordInfo.getRecordChangeDate().get(0).getEncoding());
        assertEquals("20261029T000000", recordInfo.getRecordChangeDate().get(0).getValue());
    }

    @ParameterizedTest
    @ValueSource(strings = {"------", "      ", "230229", "261332"})
    public void testFillRecordInfoWithInvalidCatalogDate(String date) throws Exception {
        String marc = "<record xmlns=\"http://www.loc.gov/MARC21/slim\">"
                + "<leader>00000nam a2200000 a 4500</leader>"
                + "<controlfield tag=\"008\">" + date + "s2026    xr            000 0 cze d</controlfield>"
                + "</record>";
        Transformers transformers = new Transformers(null);
        ModsDefinition mods = ModsUtils.unmarshalModsType(transformers.transform(
                new StreamSource(new StringReader(marc)), Transformers.Format.MarcxmlAsMods3));
        RecordInfoDefinition recordInfo = mods.getRecordInfo().get(0);
        assertEquals(date, recordInfo.getRecordCreationDate().get(0).getValue());
        LocalDateTime before = LocalDateTime.now().withNano(0);

        MapperUtils.fillRecordInfo(mods, NdkMetadataHandler.OPERATION_NEW);

        LocalDateTime after = LocalDateTime.now().withNano(0);
        assertEquals(1, recordInfo.getRecordCreationDate().size());
        DateDefinition creationDate = recordInfo.getRecordCreationDate().get(0);
        assertEquals("iso8601", creationDate.getEncoding());
        LocalDateTime result = LocalDateTime.parse(creationDate.getValue(),
                DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss"));
        assertFalse(result.isBefore(before));
        assertFalse(result.isAfter(after));
    }
}
