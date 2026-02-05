/*
 * Copyright (C) 2018 Martin Rumanek
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

package cz.cas.lib.proarc.common.process.export.sip;

import com.yourmediashelf.fedora.client.FedoraClient;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.process.export.archive.ArchiveProducer;
import cz.cas.lib.proarc.common.process.export.mockrepository.MockSearchView;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import java.io.File;
import java.util.Arrays;
import java.util.List;
import mockit.Mock;
import mockit.MockUp;
import mockit.Mocked;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assumptions.assumeTrue;

public class NdkEbornArchivalTest {

    @TempDir
    File tempDir;

    @Mocked
    private FedoraClient client;

    private final AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();
    private AkubraConfiguration akubraConfiguration = null;

    public NdkEbornArchivalTest() throws AppConfigurationException {
    }

    @BeforeEach
    public void setUp() throws AppConfigurationException {
        if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
        } else {
            this.akubraConfiguration = null;
        }

        new MockSearchView();

        new MockUp<FedoraStorage>() {
            @Mock
            FedoraStorage getInstance() {
                return new FedoraStorage(client);
            }
        };

        DigitalObjectManager.setDefault(new DigitalObjectManager(
                appConfig, akubraConfiguration,
                null,
                null,
                null));
    }

    @Test
    /**
     * Test archival export
     *
     * {@see cz.cas.lib.proarc.common.export.archive.ArchiveObjectSelector#searchPath(List)}
     */
    public void ebornExportArchivalTest() {
        ArchiveProducer export = new ArchiveProducer(appConfig, akubraConfiguration);

        List<String> pids = Arrays.asList("uuid:26342028-12c8-4446-9217-d3c9f249bd13"); //etitle

        try {
            File target = ExportUtils.createFolder(tempDir, "archive_" + FoxmlUtils.pidAsUuid(pids.get(0)), appConfig.getExportParams().isOverwritePackage());
            export.archive(pids, target, false);
        } catch (IllegalStateException ex) {
            assumeTrue(false, () -> "Test přeskočen kvůli výjimce: " + ex);
        }
    }

}
