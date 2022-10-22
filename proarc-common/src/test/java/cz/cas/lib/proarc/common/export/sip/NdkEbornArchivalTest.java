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

package cz.cas.lib.proarc.common.export.sip;

import com.yourmediashelf.fedora.client.FedoraClient;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.export.archive.ArchiveProducer;
import cz.cas.lib.proarc.common.export.mockrepository.MockSearchView;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import java.io.File;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ErrorCollector;
import org.junit.rules.TemporaryFolder;
import mockit.Mock;
import mockit.MockUp;
import mockit.Mocked;

public class NdkEbornArchivalTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Mocked
    private FedoraClient client;

    private final AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();
    private AkubraConfiguration akubraConfiguration = null;

    @Rule
    public ErrorCollector collector = new ErrorCollector();

    public NdkEbornArchivalTest() throws AppConfigurationException {
    }

    @Before
    public void setUp() throws AppConfigurationException {
        if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
        } else {
            this.akubraConfiguration = null;
        }

        new MockSearchView();

        new MockUp<RemoteStorage>() {
            @Mock
            RemoteStorage getInstance() {
                return new RemoteStorage(client);
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
            File target = ExportUtils.createFolder(folder.getRoot(), "archive_" + FoxmlUtils.pidAsUuid(pids.get(0)), appConfig.getExportOptions().isOverwritePackage());
            export.archive(pids, target, false);
        } catch (IllegalStateException ex) {
            collector.addError(ex.getCause());
        }
    }

}
