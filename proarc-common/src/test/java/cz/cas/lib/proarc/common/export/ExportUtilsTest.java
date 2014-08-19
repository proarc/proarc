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
package cz.cas.lib.proarc.common.export;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.export.ExportResultLog.ExportResult;
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultError;
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultStatus;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import java.io.File;
import java.util.Date;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class ExportUtilsTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);

    public ExportUtilsTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testCreateFolder() {
        File parent = temp.getRoot();
        String name = FoxmlUtils.pidAsUuid("uuid:0bcf9933-84e5-460f-9e94-d798b724d394");
        File expResult = new File(parent, name);
        File result = ExportUtils.createFolder(parent, name);
        assertEquals(expResult, result);

        expResult = new File(parent, name + "_1");
        result = ExportUtils.createFolder(parent, name);
        assertEquals(expResult, result);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testCreateFolderFailure() {
        File parent = temp.getRoot();
        String name = "uuid:0bcf9933-84e5-460f-9e94-d798b724d394";
        ExportUtils.createFolder(parent, name);
    }

    @Test
    public void testExportResult() {
        File target = temp.getRoot();
        ExportResult export = new ExportResult();
        export.setStatus(ResultStatus.OK);
        export.setInputPid("pid1");
        export.getError().add(new ResultError("childPid1", "error1"));
        export.getError().add(new ResultError("childpid2", new IllegalStateException("error2")));
        export.setEnd(new Date());

        ExportResultLog log = new ExportResultLog();
        log.getExports().add(export);

        ExportUtils.writeExportResult(target, log);
    }
}
