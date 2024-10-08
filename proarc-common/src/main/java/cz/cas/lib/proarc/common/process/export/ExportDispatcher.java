/*
 * Copyright (C) 2023 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.export;

import java.lang.Thread.UncaughtExceptionHandler;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Dispatcher controls scheduling of {@link ExportProcess}.
 *
 * For now it runs processes in single thread to preserve memory resources.
 *
 * @author Lukas Sykora
 */
public final class ExportDispatcher {

    private static final Logger LOG = Logger.getLogger(ExportDispatcher.class.getName());
    private static ExportDispatcher INSTANCE = new ExportDispatcher();

    private ExecutorService pool;
    private final int threadCount;

    public ExportDispatcher() {
        this(1);
    }

    ExportDispatcher(int threadCount) {
        if (threadCount < 1) {
            throw new IllegalArgumentException("threadCount: " + threadCount);
        }
        this.threadCount = threadCount;
    }

    public static ExportDispatcher getDefault() {
        return INSTANCE;
    }

    public static void setDefault(ExportDispatcher dispatcher) {
        INSTANCE = dispatcher;
    }

    public void init() {
        pool = newThreadPool();
    }

    public void stop() {
        stop(60, TimeUnit.SECONDS);
    }

    public void stopNow() {
        stop(5, TimeUnit.SECONDS);
    }

    public void stop(long timeout, TimeUnit unit) {
        if (pool == null) {
            return ;
        }
        pool.shutdown(); // Disable new tasks from being submitted
        try {
            // Wait a while for existing tasks to terminate
            if (!pool.awaitTermination(timeout, unit)) {
                pool.shutdownNow(); // Cancel currently executing tasks
                // Wait a while for tasks to respond to being cancelled
                if (!pool.awaitTermination(timeout, unit)) {
                    LOG.severe("ExportDispatcher thread pool did not terminate");
                }
            }
        } catch (InterruptedException ie) {
            // (Re-)Cancel if current thread also interrupted
            pool.shutdownNow();
            // Preserve interrupt status
            Thread.currentThread().interrupt();
        }
    }

    public Future<ExportProcess> addExport(ExportProcess task) {
        return addTask(task);
    }

    <T extends Runnable> Future<T> addTask(T task) {
        checkRunning();
        return pool.submit(new ExceptionHandlingTask(task), task);
    }

    private void checkRunning() {
        if (pool == null) {
            throw new IllegalStateException("needs init");
        }
        if (pool.isShutdown()) {
            restart();
        }
    }

    public void restart() {
        if (pool != null && pool.isShutdown()) {
            pool = newThreadPool();
        }
    }

    private ExecutorService newThreadPool() {
        ExecutorService executorService = Executors.newFixedThreadPool(threadCount, new ExportDispatcherThreadFactory());
        return executorService;
    }

    private static final class ExceptionHandlingTask implements Runnable {

        private final Runnable delegate;

        public ExceptionHandlingTask(Runnable delegate) {
            this.delegate = delegate;
        }

        @Override
        public void run() {
            try {
                delegate.run();
            } catch (Throwable t) {
                Thread.currentThread().getUncaughtExceptionHandler().uncaughtException(Thread.currentThread(), t);
            }
        }

    }

    private static final class ExportDispatcherThreadFactory implements ThreadFactory {

        private final ThreadFactory factory;

        public ExportDispatcherThreadFactory() {
            factory = Executors.defaultThreadFactory();
        }

        @Override
        public Thread newThread(Runnable r) {
            Thread thread = factory.newThread(r);
            String name = ExportDispatcher.class.getSimpleName() + '-' + thread.getName();
            thread.setName(name);
            UncaughtExceptionHandler uncaughtExceptionHandler = thread.getUncaughtExceptionHandler();
            thread.setUncaughtExceptionHandler(new ExportDispatcherExceptionHandler(uncaughtExceptionHandler));
            return thread;
        }

    }

    private static final class ExportDispatcherExceptionHandler implements UncaughtExceptionHandler {

        private final UncaughtExceptionHandler delegate;

        public ExportDispatcherExceptionHandler(UncaughtExceptionHandler delegate) {
            this.delegate = delegate;
        }

        @Override
        public void uncaughtException(Thread t, Throwable e) {
            LOG.log(Level.SEVERE, t.getName(), e);
//            if (delegate != null) {
//                delegate.uncaughtException(t, e);
//            }
        }

    }
}
