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

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.process.export.ExportResultLog.ExportResult;
import cz.cas.lib.proarc.common.process.export.ExportResultLog.ResultError;
import cz.cas.lib.proarc.common.process.export.ExportResultLog.ResultStatus;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import java.io.File;
import java.util.Arrays;
import java.util.Date;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class ExportUtilsTest {

    private final AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();

    @TempDir
    File tempDir;

    public ExportUtilsTest() throws Exception {
    }

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    @BeforeEach
    public void setUp() {
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testCreateFolder() {
        File parent = tempDir;
        String name = FoxmlUtils.pidAsUuid("uuid:0bcf9933-84e5-460f-9e94-d798b724d394");
        File expResult = new File(parent, name);
        File result = ExportUtils.createFolder(parent, name, appConfig.getExportParams().isOverwritePackage());
        assertEquals(expResult, result);

        expResult = new File(parent, name + "_1");
        result = ExportUtils.createFolder(parent, name, appConfig.getExportParams().isOverwritePackage());
        assertEquals(expResult, result);
    }

    @Test
    public void testCreateFolderFailure() {
        File parent = tempDir;
        String name = "uuid:0bcf9933-84e5-460f-9e94-d798b724d394";
        ExportUtils.createFolder(parent, name, appConfig.getExportParams().isOverwritePackage());
    }

    @Test
    public void testExportResult() throws Exception {
        File target = tempDir;
        ExportResult export = new ExportResult();
        export.setStatus(ResultStatus.OK);
        export.setInputPid("pid1");
        export.getError().add(new ResultError("childPid1", "error1"));
        export.getError().add(new ResultError("childpid2", new IllegalStateException("error2")));
        export.getError().add(new ResultError("childPid3", "msg", "error3"));
        export.getError().add(new ResultError("childPid4", "msg", Arrays.asList("validation1", "validation2")));
        export.setEnd(new Date());

        ExportResultLog log = new ExportResultLog();
        log.getExports().add(export);

        ExportUtils.writeExportResult(target, log);

        File statusFile = new File(target, ExportUtils.PROARC_EXPORT_STATUSLOG);
        assertTrue(statusFile.exists());
        String result = FileUtils.readFileToString(statusFile);
//        System.out.println(result);
        assertTrue(result.contains("error1"), () -> result);
        assertTrue(result.contains("error2"), () -> result);
        assertTrue(result.contains("error3"), () -> result);
        assertTrue(result.contains("validation1"), () -> result);
        assertTrue(result.contains("validation2"), () -> result);
    }
}
