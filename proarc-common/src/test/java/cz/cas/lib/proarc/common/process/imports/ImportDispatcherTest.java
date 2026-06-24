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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.process.BatchManager;
import java.io.File;
import java.sql.Timestamp;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;
import mockit.Delegate;
import mockit.Expectations;
import mockit.Mocked;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;


/**
 *
 * @author Jan Pokorsky
 */
public class ImportDispatcherTest {

    @TempDir
    File tempDir;

    public ImportDispatcherTest() {
    }

    @Test
    public void testSingleThread(@Mocked BatchManager batchManager, @Mocked ImportProfile profile, @Mocked AppConfiguration config) throws Exception {
        ImportDispatcher instance = new ImportDispatcher();
        instance.init();
        final Object monitor = new Object();
        final AtomicInteger taskFinishedCounter = new AtomicInteger(0);
        final TaskHandler[] tasks = new TaskHandler[3];

        TaskHandler task1 = new TaskHandler() {

            @Override
            public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {
                assertFalse(tasks[0].started);
                assertFalse(tasks[1].started);
                assertFalse(tasks[2].started);
                super.start(importConfig, batchManager, config);
                assertTrue(tasks[0].started);
                assertTrue(tasks[0].finished);
                assertEquals(1, taskFinishedCounter.incrementAndGet());
            }

            @Override
            protected void processTask() {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException ex) {
                    Logger.getLogger(ImportDispatcherTest.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        };

        TaskHandler task2 = new TaskHandler() {

            @Override
            public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {
                assertTrue(tasks[0].finished);
                assertFalse(tasks[1].started);
                assertFalse(tasks[2].started);
                super.start(importConfig, batchManager, config);
                assertTrue(tasks[1].started);
                assertTrue(tasks[1].finished);
                assertEquals(2, taskFinishedCounter.incrementAndGet());
                throw new IllegalStateException("task2 failure");
            }
        };

        TaskHandler task3 = new TaskHandler() {

            @Override
            public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {
                assertTrue(tasks[0].finished);
                assertTrue(tasks[1].finished);
                assertFalse(tasks[2].started);
                super.start(importConfig, batchManager, config);
                assertTrue(tasks[2].started);
                assertTrue(tasks[2].finished);
                assertEquals(3, taskFinishedCounter.incrementAndGet());
                synchronized (monitor) {
                    monitor.notifyAll();
                }
            }
        };

        tasks[0] = task1;
        tasks[1] = task2;
        tasks[2] = task3;

        expectProcesses(batchManager, profile, task1, task2, task3);
        instance.addImport(createProcess(task1, batchManager, profile, config, 1));
        instance.addImport(createProcess(task2, batchManager, profile, config, 2));
        instance.addImport(createProcess(task3, batchManager, profile, config, 3));

        synchronized (monitor) {
            monitor.wait(1000);
        }
        assertEquals(3, taskFinishedCounter.get());
        assertTrue(tasks[0].finished);
        assertTrue(tasks[1].finished);
        assertTrue(tasks[2].finished);
    }

    @Test
    public void testStopSingleThread(@Mocked BatchManager batchManager, @Mocked ImportProfile profile, @Mocked AppConfiguration config) throws Exception {
        ImportDispatcher instance = new ImportDispatcher();
        instance.init();
        final Object monitor = new Object();
        final AtomicInteger taskFinishedCounter = new AtomicInteger(0);
        final TaskHandler[] tasks = new TaskHandler[2];

        TaskHandler task1 = new TaskHandler() {

            @Override
            public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {
                assertFalse(tasks[0].started);
                assertFalse(tasks[1].started);
                super.start(importConfig, batchManager, config);
                assertTrue(tasks[0].started);
                assertTrue(tasks[0].finished);
                assertEquals(1, taskFinishedCounter.incrementAndGet());
            }

            @Override
            protected void processTask() {
                synchronized (monitor) {
                    // wake up main thread
                    monitor.notifyAll();
                }
                int size = 0;
                for (int i = Integer.MIN_VALUE; i < Integer.MAX_VALUE; i++) {
                    size += i / 2 * 3;
                    if (Thread.interrupted()) {
                        canceled = true;
                        return;
                    }
                }
                fail("task 1 should not finished, " + Thread.interrupted());
                System.out.println(size);
            }
        };

        TaskHandler task2 = new TaskHandler() {

            @Override
            public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) {
                fail("task 2 should not start, " + Thread.interrupted());
            }
        };

        tasks[0] = task1;
        tasks[1] = task2;

        expectProcesses(batchManager, profile, task1, task2);
        instance.addImport(createProcess(task1, batchManager, profile, config, 1));
        instance.addImport(createProcess(task2, batchManager, profile, config, 2));

        synchronized (monitor) {
            monitor.wait(100);
        }
        instance.stop(100, TimeUnit.MILLISECONDS);
        assertEquals(1, taskFinishedCounter.get());
        assertTrue(tasks[0].started);
        assertTrue(tasks[0].canceled);
        assertFalse(tasks[1].started);


    }

    private ImportProcess createProcess(TaskHandler handler, BatchManager batchManager, ImportProfile profile, AppConfiguration config, int id) {
        File importFolder = new File(tempDir, "import-" + id);
        assertTrue(importFolder.mkdir());
        Batch batch = new Batch();
        batch.setId(id);
        batch.setCreate(new Timestamp(System.currentTimeMillis() + id));
        batch.setFolder(importFolder.getAbsolutePath());
        batch.setPriority(Batch.PRIORITY_MEDIUM);
        batch.setState(Batch.State.IMPORT_PLANNED);
        ImportProcess.ImportOptions options = new ImportProcess.ImportOptions(
                importFolder, null, null, false, null, profile, Batch.PRIORITY_MEDIUM);
        options.setBatch(batch);
        return new ImportProcess(options, batchManager, config);
    }

    private void expectProcesses(BatchManager batchManager, ImportProfile profile, TaskHandler... handlers) throws Exception {
        new Expectations() {{
            profile.createImporter();
            result = new Delegate<ImportHandler>() {
                int index;

                ImportHandler createImporter() {
                    return handlers[index++];
                }
            };
            minTimes = 0;
            profile.getDefaultImportFolder();
            result = true;
            minTimes = 0;
            batchManager.getFolderStatus((Batch) any);
            result = null;
            minTimes = 0;
            batchManager.update((Batch) any);
            result = new Delegate<Batch>() {
                Batch update(Batch batch) {
                    return batch;
                }
            };
            minTimes = 0;
        }};
    }

    private static class TaskHandler implements ImportHandler {

        volatile boolean started;
        volatile boolean finished;
        volatile boolean canceled;

        @Override
        public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {
            started = true;
            try {
                processTask();
            } finally {
                finished = true;
            }
        }

        @Override
        public int estimateItemNumber(ImportProcess.ImportOptions importConfig) {
            return 1;
        }

        @Override
        public boolean isImportable(File folder) {
            return true;
        }

        protected void processTask() {

        }

    }
}
