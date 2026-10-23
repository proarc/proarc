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
package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.foxml.management.DatastreamProfile;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.Yes;
import java.util.List;
import java.util.Set;
import mockit.Expectations;
import mockit.Mocked;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import static org.junit.jupiter.api.Assertions.*;

class SolrLogMetadataTest {
    @Mocked ProArcObject object;
    @Mocked RelationEditor relations;
    @Mocked ModsStreamEditor modsEditor;

    @Test
    void readsModelDeviceAndOnlySiglaPhysicalLocations() throws Exception {
        ModsDefinition mods = new ModsDefinition();
        for (String value : List.of("10.1234/article", " 10.1234/article ", " ")) {
            cz.cas.lib.proarc.mods.IdentifierDefinition doi = new cz.cas.lib.proarc.mods.IdentifierDefinition();
            doi.setType("doi"); doi.setValue(value); mods.getIdentifier().add(doi);
        }
        LocationDefinition location = new LocationDefinition();
        mods.getLocation().add(location);
        for (String sigla : List.of(" ABA001 ", "ABA001", "BOA001")) {
            PhysicalLocationDefinition physical = new PhysicalLocationDefinition();
            physical.setAuthority("siglaADR");
            physical.setValue(sigla);
            location.getPhysicalLocation().add(physical);
        }
        PhysicalLocationDefinition ordinaryLocation = new PhysicalLocationDefinition();
        ordinaryLocation.setValue("Reading room");
        location.getPhysicalLocation().add(ordinaryLocation);
        PhysicalLocationDefinition legacySigla = new PhysicalLocationDefinition();
        legacySigla.setValue("OSA001");
        location.getPhysicalLocation().add(legacySigla);
        DatastreamProfile profile = new DatastreamProfile();
        profile.setDsID(ModsStreamEditor.DATASTREAM_ID);
        new Expectations() {{
            relations.getModel(); result = "model:ndkmonographunit";
            relations.getDevice(); result = "device:scanner";
            relations.getSoftware(); result = "software:scanner-suite";
            object.getStreamProfile(null); result = List.of(profile);
            modsEditor.read(); result = mods;
        }};
        SolrLogMetadata metadata = SolrLogMetadata.from(object);
        assertEquals("model:ndkmonographunit", metadata.model());
        assertEquals(Set.of("10.1234/article"), metadata.dois());
        assertEquals(Set.of("device:scanner"), metadata.devices());
        assertEquals(Set.of("software:scanner-suite"), metadata.software());
        assertThrows(UnsupportedOperationException.class, () -> metadata.software().add("other"));
        assertEquals(Set.of("ABA001", "BOA001", "OSA001"), metadata.siglas());
        assertThrows(UnsupportedOperationException.class, () -> metadata.siglas().add("XXX001"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"model:ndkperiodicalissue", "model:ndkeperiodicalissue"})
    void capturesIssueMapperLabelAndUrnNbn(String model) throws Exception {
        ModsDefinition mods = new ModsDefinition();
        var identifier = new cz.cas.lib.proarc.mods.IdentifierDefinition();
        identifier.setType("urn:nbn"); identifier.setValue(" urn:nbn:cz:issue ");
        mods.getIdentifier().add(identifier);
        var titleInfo = new cz.cas.lib.proarc.mods.TitleInfoDefinition();
        var number = new cz.cas.lib.proarc.mods.StringPlusLanguage(); number.setValue("12");
        titleInfo.getPartNumber().add(number); mods.getTitleInfo().add(titleInfo);
        var originInfo = new cz.cas.lib.proarc.mods.OriginInfoDefinition();
        var date = new cz.cas.lib.proarc.mods.DateDefinition(); date.setValue("12.12.2025");
        originInfo.getDateIssued().add(date); mods.getOriginInfo().add(originInfo);
        DatastreamProfile profile = new DatastreamProfile(); profile.setDsID(ModsStreamEditor.DATASTREAM_ID);
        new Expectations() {{
            relations.getModel(); result = model;
            object.getStreamProfile(null); result = List.of(profile);
            modsEditor.read(); result = mods;
        }};
        var metadata = SolrLogMetadata.from(object);
        assertEquals("12, 12.12.2025", metadata.label());
        assertEquals(Set.of("urn:nbn:cz:issue"), metadata.urnNbns());
    }

    @ParameterizedTest
    @ValueSource(strings = {"doi", "DOI", "urnnbn", "URN:NBN"})
    void excludesInvalidIdentifiersWhenValidAndInvalidValuesCoexist(String type) throws Exception {
        ModsDefinition mods = new ModsDefinition();
        for (String value : List.of("valid-id", " valid-id ", " ")) {
            IdentifierDefinition id = new IdentifierDefinition();
            id.setType(type); id.setValue(value); mods.getIdentifier().add(id);
        }
        IdentifierDefinition invalid = new IdentifierDefinition();
        invalid.setType(type); invalid.setValue("obsolete-id"); invalid.setInvalid(Yes.YES);
        mods.getIdentifier().add(invalid);
        DatastreamProfile profile = new DatastreamProfile(); profile.setDsID(ModsStreamEditor.DATASTREAM_ID);
        new Expectations() {{
            relations.getModel(); result = "model:ndkperiodicalissue";
            object.getStreamProfile(null); result = List.of(profile);
            modsEditor.read(); result = mods;
        }};
        var metadata = SolrLogMetadata.from(object);
        assertEquals(Set.of("valid-id"), "doi".equalsIgnoreCase(type) ? metadata.dois() : metadata.urnNbns());
    }

    @ParameterizedTest
    @ValueSource(strings = {"doi", "urnnbn", "urn:nbn"})
    void neverFallsBackToInvalidIdentifierWhenNoValidValueExists(String type) throws Exception {
        ModsDefinition mods = new ModsDefinition();
        IdentifierDefinition invalid = new IdentifierDefinition();
        invalid.setType(type); invalid.setValue("obsolete-id"); invalid.setInvalid(Yes.YES);
        mods.getIdentifier().add(invalid);
        DatastreamProfile profile = new DatastreamProfile(); profile.setDsID(ModsStreamEditor.DATASTREAM_ID);
        new Expectations() {{
            relations.getModel(); result = "model:ndkperiodicalissue";
            object.getStreamProfile(null); result = List.of(profile);
            modsEditor.read(); result = mods;
        }};
        var metadata = SolrLogMetadata.from(object);
        assertTrue(metadata.dois().isEmpty()); assertTrue(metadata.urnNbns().isEmpty());
    }

    @Test
    void objectsWithoutModsStillHaveTheirModel() throws Exception {
        new Expectations() {{
            relations.getModel(); result = "model:device";
            object.getStreamProfile(null); result = List.of();
        }};
        SolrLogMetadata metadata = SolrLogMetadata.from(object);
        assertEquals("model:device", metadata.model());
        assertTrue(metadata.devices().isEmpty());
        assertTrue(metadata.software().isEmpty());
        assertTrue(metadata.siglas().isEmpty());
    }
}
