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
package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ModsDataHandlerTest {

    @TempDir
    Path tempDir;

    @Test
    public void prefersSongMetadataForTrack() throws Exception {
        TitleInfoDefinition songTitle = new TitleInfoDefinition();
        ModsDefinition songMods = new ModsDefinition();
        songMods.getTitleInfo().add(songTitle);
        ModsDefinition documentMods = new ModsDefinition();
        documentMods.getTitleInfo().add(new TitleInfoDefinition());

        TestModsDataHandler handler = createHandler();
        handler.add(NdkAudioPlugin.MODEL_SONG, songMods);
        handler.add(NdkAudioPlugin.MODEL_MUSICDOCUMENT, documentMods);

        ModsDefinition trackMods = handler.createDefaultMetadata(
                "uuid:track", NdkAudioPlugin.MODEL_TRACK, null, null);

        assertSame(songTitle, trackMods.getTitleInfo().get(0));
        assertEquals(List.of(NdkAudioPlugin.MODEL_SONG), handler.getSearchedModels());
    }

    @Test
    public void fallsBackToMusicDocumentMetadataForTrack() throws Exception {
        TitleInfoDefinition documentTitle = new TitleInfoDefinition();
        IdentifierDefinition issueNumber = new IdentifierDefinition();
        issueNumber.setType("issue number");
        ModsDefinition documentMods = new ModsDefinition();
        documentMods.getTitleInfo().add(documentTitle);
        documentMods.getIdentifier().add(issueNumber);

        TestModsDataHandler handler = createHandler();
        handler.add(NdkAudioPlugin.MODEL_MUSICDOCUMENT, documentMods);

        ModsDefinition trackMods = handler.createDefaultMetadata(
                "uuid:track", NdkAudioPlugin.MODEL_TRACK, null, null);

        assertSame(documentTitle, trackMods.getTitleInfo().get(0));
        assertTrue(trackMods.getIdentifier().contains(issueNumber));
        assertEquals(List.of(
                NdkAudioPlugin.MODEL_SONG,
                NdkAudioPlugin.MODEL_MUSICDOCUMENT), handler.getSearchedModels());
    }

    private TestModsDataHandler createHandler() throws Exception {
        Map<String, String> properties = new HashMap<>();
        properties.put(AppConfiguration.PROPERTY_APP_HOME, tempDir.toString());
        AppConfiguration appConfiguration = AppConfigurationFactory.getInstance().create(properties);
        return new TestModsDataHandler(appConfiguration);
    }

    private static final class TestModsDataHandler extends ModsDataHandler {

        private final Map<String, ModsDefinition> metadataByModel = new LinkedHashMap<>();
        private final List<String> searchedModels = new ArrayList<>();

        TestModsDataHandler(AppConfiguration appConfiguration) {
            super(appConfiguration);
        }

        void add(String model, ModsDefinition mods) {
            metadataByModel.put(model, mods);
        }

        List<String> getSearchedModels() {
            return searchedModels;
        }

        @Override
        ModsDefinition findEnclosingObject(
                String searchModelId, DigitalObjectHandler parentHandler, Job parentJob
        ) throws DigitalObjectException {
            searchedModels.add(searchModelId);
            return metadataByModel.get(searchModelId);
        }
    }
}
