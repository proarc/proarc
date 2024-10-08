/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.cas.lib.proarc.common.process.export;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.storage.FedoraTestSupport;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import java.io.File;
import java.util.Arrays;
import java.util.List;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class DataStreamExportTest {

    private final AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();
    private AkubraConfiguration akubraConfiguration = null;

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);

    public DataStreamExportTest() throws Exception {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() throws AppConfigurationException {
        if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
        } else {
            this.akubraConfiguration = null;
        }
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testExport() throws Exception {
        FedoraTestSupport fedora = new FedoraTestSupport();
        fedora.cleanUp();
        fedora.ingest(DataStreamExportTest.class.getResource("Kramerius4ExportTestPage.xml"));

        File output = temp.getRoot();
        boolean hierarchy = true;
        List<String> pids = Arrays.asList("uuid:f74f3cf3-f3be-4cac-95da-8e50331414a2");
        List<String> dsIds = Arrays.asList(StringEditor.OCR_ID, "PREVIEW");
        DataStreamExport instance = new DataStreamExport(fedora.getRemoteStorage(), appConfig, akubraConfiguration);
        File target = instance.export(output, hierarchy, pids, dsIds, null);
        assertNotNull(target);

        File ocr = new File(target, DataStreamExport.filename(pids.get(0), dsIds.get(0)));
        assertTrue(ocr.exists());
        assertTrue(ocr.length() == 3);

        File preview = new File(target, DataStreamExport.filename(pids.get(0), dsIds.get(1)));
        assertTrue(preview.exists());
    }

}
