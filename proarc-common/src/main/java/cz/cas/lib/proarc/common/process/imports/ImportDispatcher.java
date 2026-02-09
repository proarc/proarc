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

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.process.WorkWindow;
import java.lang.Thread.UncaughtExceptionHandler;
import java.sql.Timestamp;
import java.util.Comparator;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Dispatcher controls scheduling of {@link ImportProcess import processes}.
 *
 * For now it runs processes in single thread to preserve memory resources.
 *
 * @author Jan Pokorsky
 */
public final class ImportDispatcher {

    private static final Logger LOG = Logger.getLogger(ImportDispatcher.class.getName());
    private static ImportDispatcher INSTANCE = new ImportDispatcher();

    private ExecutorService pool;
    private final int threadCount;

    public ImportDispatcher() {
        this(1);
    }

    ImportDispatcher(int threadCount) {
        if (threadCount < 1) {
            throw new IllegalArgumentException("threadCount: " + threadCount);
        }
        this.threadCount = threadCount;
    }

    public static ImportDispatcher getDefault() {
        return INSTANCE;
    }

    public static void setDefault(ImportDispatcher dispatcher) {
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
                    LOG.severe("ImportDispatcher thread pool did not terminate");
                }
            }
        } catch (InterruptedException ie) {
            // (Re-)Cancel if current thread also interrupted
            pool.shutdownNow();
            // Preserve interrupt status
            Thread.currentThread().interrupt();
        }
    }

    public Future<ImportProcess> addImport(ImportProcess task) {
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
        ExecutorService executorService = new ThreadPoolExecutor(threadCount, threadCount, 0L,
                TimeUnit.MILLISECONDS, new PriorityBlockingQueue<Runnable>(11, new PriorityFutureComparator ()), new ImportDispatcherThreadFactory()) {

            @Override
            protected <T> RunnableFuture<T> newTaskFor(Runnable runnable, T value) {
                RunnableFuture<T> newTaskFor = super.newTaskFor(runnable, value);
                return new PriorityFuture<>(newTaskFor, ((ImportProcess) value), ((ImportProcess) value).getBatch().getPriority(), ((ImportProcess) value).getBatch().getCreate()
                );
            }

        };
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

    private static final class ImportDispatcherThreadFactory implements ThreadFactory {

        private final ThreadFactory factory;

        public ImportDispatcherThreadFactory() {
            factory = Executors.defaultThreadFactory();
        }

        @Override
        public Thread newThread(Runnable r) {
            Thread thread = factory.newThread(r);
            String name = ImportDispatcher.class.getSimpleName() + '-' + thread.getName();
            thread.setName(name);
            UncaughtExceptionHandler uncaughtExceptionHandler = thread.getUncaughtExceptionHandler();
            thread.setUncaughtExceptionHandler(new ImportDispatcherExceptionHandler(uncaughtExceptionHandler));
            return thread;
        }

    }

    private static final class ImportDispatcherExceptionHandler implements UncaughtExceptionHandler {

        private final UncaughtExceptionHandler delegate;

        public ImportDispatcherExceptionHandler(UncaughtExceptionHandler delegate) {
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

    private class PriorityFutureComparator  implements Comparator<Runnable> {

        @Override
        public int compare(Runnable o1, Runnable o2) {
            if (o1 == null && o2 == null) {
                return 0;
            } else if (o1 == null) {
                return -1;
            } else if (o2 ==null) {
                return 1;
            } else {
                PriorityFuture<?> p1 = (PriorityFuture<?>) o1;
                PriorityFuture<?> p2 = (PriorityFuture<?>) o2;

                boolean workingTime = WorkWindow.isWorkingTime();

                if (workingTime) {
                    if (p1.isDeferred() && !p2.isDeferred()) return 1;
                    if (!p1.isDeferred() && p2.isDeferred()) return -1;
                }

                int priorityO1 = transform(p1.getPriority());
                int priorityO2 = transform(p2.getPriority());

                // -1 pro to, co ma bezet nejdriv
                // 0 pokud maji stejnou prioritu --> pote rozhoduje cas vzniku
                // 1 pro to, co ma bezet naposled
                return priorityO1 > priorityO2 ? -1 : (priorityO1 == priorityO2 ? compareTimestamp(o1, o2) : 1);
            }
        }

        private int compareTimestamp(Runnable o1, Runnable o2) {
            Timestamp timestampO1 = ((PriorityFuture) o1).getCreatedDate();
            Timestamp timestampO2 = ((PriorityFuture) o2).getCreatedDate();

            return timestampO1.compareTo(timestampO2);
        }

        private int transform(String priority) {
            if (priority == null) {
                return 0;
            } else if (Batch.PRIORITY_HIGHEST.equals(priority)) {
                return 2;
            } else if (Batch.PRIORITY_HIGH.equals(priority)) {
                return 1;
            } else if (Batch.PRIORITY_MEDIUM.equals(priority)) {
                return 0;
            } else if (Batch.PRIORITY_LOW.equals(priority)) {
                return -1;
            } else if (Batch.PRIORITY_LOWEST.equals(priority)) {
                return -2;
            } else {
                return 0;
            }
        }
    }

    private class PriorityFuture<T> implements RunnableFuture<T> {

        private RunnableFuture<T> src;
        private ImportProcess process;
        private String priority;
        private Timestamp createdDate;

        public PriorityFuture(RunnableFuture<T> src, ImportProcess process, String priority, Timestamp createdDate
        ) {
            this.src = src;
            this.process = process;
            this.priority = priority;
            this.createdDate = createdDate;
        }

        public String getPriority() {
            return priority;
        }

        public Timestamp getCreatedDate() {
            return createdDate;
        }

        public ImportProcess getProcess() {
            return process;
        }

        public boolean isDeferred() {
            return !WorkWindow.isNotAllowed(process.getBatch());
        }

        @Override
        public void run() {
            src.run();
        }

        @Override
        public boolean cancel(boolean mayInterruptIfRunning) {
            return src.cancel(mayInterruptIfRunning);
        }

        @Override
        public boolean isCancelled() {
            return src.isCancelled();
        }

        @Override
        public boolean isDone() {
            return src.isDone();
        }

        @Override
        public T get() throws InterruptedException, ExecutionException {
            return src.get();
        }

        @Override
        public T get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
            return src.get(timeout, unit);
        }
    }
}
