/*
 * Copyright (C) 2017 Jakub Kremlacek
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

package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.File;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Jakub Kremlacek
 */
public class TiffAsJp2ImporterTest {

    @TempDir
    File tempDir;

    private UserProfile junit;

    @BeforeEach
    public void setUp() throws Exception {
        junit = new UserProfile();
        junit.setUserName("junit");
        File root = tempDir;
        System.out.println("root: " + root.toString());
    }

    @Test
    public void testConsumeInvalidFileSet() throws Exception {
        TiffAsJp2Importer importer = new TiffAsJp2Importer(null);
        File testfile = new File(tempDir, "test.db");
        FileSet set = new FileSet("test");

        testfile.createNewFile();

        assertTrue(testfile.exists());
        assertNotNull(importer);

        set.getFiles().add(new FileSet.FileEntry(testfile));

        BatchManager.BatchItemObject object = importer.consume(set, null);

        assertNull(object);
    }
}
