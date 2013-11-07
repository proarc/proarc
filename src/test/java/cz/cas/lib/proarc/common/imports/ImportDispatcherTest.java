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
package cz.cas.lib.proarc.common.imports;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportDispatcherTest {

    public ImportDispatcherTest() {
    }

    @Test
    public void testSingleThread() throws InterruptedException {
        ImportDispatcher instance = new ImportDispatcher();
        instance.init();
        final Object monitor = new Object();
        final AtomicInteger taskFinishedCounter = new AtomicInteger(0);
        final Task[] tasks = new Task[3];

        Task task1 = new Task() {

            @Override
            public void run() {
                assertFalse(tasks[0].started);
                assertFalse(tasks[1].started);
                assertFalse(tasks[2].started);
                super.run();
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

        Task task2 = new Task() {

            @Override
            public void run() {
                assertTrue(tasks[0].finished);
                assertFalse(tasks[1].started);
                assertFalse(tasks[2].started);
                super.run();
                assertTrue(tasks[1].started);
                assertTrue(tasks[1].finished);
                assertEquals(2, taskFinishedCounter.incrementAndGet());
                throw new IllegalStateException("task2 failure");
            }
        };

        Task task3 = new Task() {

            @Override
            public void run() {
                assertTrue(tasks[0].finished);
                assertTrue(tasks[1].finished);
                assertFalse(tasks[2].started);
                super.run();
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

        instance.addTask(task1);
        instance.addTask(task2);
        instance.addTask(task3);

        synchronized (monitor) {
            monitor.wait(1000);
        }
        assertEquals(3, taskFinishedCounter.get());
        assertTrue(tasks[0].finished);
        assertTrue(tasks[1].finished);
        assertTrue(tasks[2].finished);
    }

    @Test
    public void testStopSingleThread() throws InterruptedException {
        ImportDispatcher instance = new ImportDispatcher();
        instance.init();
        final Object monitor = new Object();
        final AtomicInteger taskFinishedCounter = new AtomicInteger(0);
        final Task[] tasks = new Task[2];

        Task task1 = new Task() {

            @Override
            public void run() {
                assertFalse(tasks[0].started);
                assertFalse(tasks[1].started);
                super.run();
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
                        return ;
                    }
                }
                fail("task 1 should not finished, " + Thread.interrupted());
                System.out.println(size);
            }
        };

        Task task2 = new Task() {

            @Override
            public void run() {
                fail("task 2 should not start, " + Thread.interrupted());
            }
        };

        tasks[0] = task1;
        tasks[1] = task2;

        instance.addTask(task1);
        instance.addTask(task2);

        synchronized (monitor) {
            monitor.wait(100);
        }
        instance.stop(100, TimeUnit.MILLISECONDS);
        assertEquals(1, taskFinishedCounter.get());
        assertTrue(tasks[0].started);
        assertTrue(tasks[0].canceled);
        assertFalse(tasks[1].started);


    }

    private static class Task implements Runnable {

        volatile boolean started;
        volatile boolean finished;
        volatile boolean canceled;

        @Override
        public void run() {
            started = true;
            try {
                processTask();
            } finally {
                finished = true;
            }
        }

        protected void processTask() {

        }

    }
}
