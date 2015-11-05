/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.server;

import cz.cas.lib.proarc.authentication.Authenticators;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireConfiguration;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireDaoFactory;
import cz.cas.lib.proarc.common.fedora.FedoraStorageInitializer;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportDispatcher;
import cz.cas.lib.proarc.common.imports.ImportProcess;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.sql.DbUtils;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import javax.naming.NamingException;
import javax.sql.DataSource;

/**
 * Initializes the application.
 *
 * @author Jan Pokorsky
 */
public final class ProarcInitializer {

    private static final Logger LOG = Logger.getLogger(ProarcInitializer.class.getName());
    private static final ProarcInitializer INSTANCE = new ProarcInitializer();

    private final ExecutorService executor;
    private Future<Void> asyncTask;
    private DaoFactory daoFactory;

    public static ProarcInitializer getInstance() {
        return INSTANCE;
    }

    private ProarcInitializer() {
        executor = Executors.newSingleThreadExecutor();
    }

    /**
     * Passes when the initialization is done.
     * @throws init failure
     */
    public void isReady() throws Exception {
        asyncTask.get(2, TimeUnit.MINUTES);
    }

    /**
     * Starts the initialization.
     *
     * @param env config params
     */
    public void start(Map<String, String> env) {
//        LOG.info("Starting " + AppConfiguration.FULL_VERSION);
        AppConfiguration config = initConfig(env);
        initProarcModel(config);
        DataSource proarcSource = initProarcDb();
        initUsers(config, proarcSource, daoFactory);
        initImport(config, daoFactory);
        DigitalObjectManager.setDefault(new DigitalObjectManager(
                config, ImportBatchManager.getInstance(), null,
                MetaModelRepository.getInstance(), UserUtil.getDefaultManger()));
        Authenticators.setInstance(new Authenticators(config.getAuthenticators()));
        initWorkflow(config, daoFactory, UserUtil.getDefaultManger());
        asyncTask = executor.submit(new Callable<Void>() {

            @Override
            public Void call() throws Exception {
                asyncInitialization();
                return null;
            }
        });
        executor.shutdown();
    }

    /**
     * Releases referenced resources.
     */
    public void destroy() {
//        LOG.info("Destroing " + AppConfiguration.FULL_VERSION);
        ImportDispatcher importDispatcher = ImportDispatcher.getDefault();
        importDispatcher.stop();
        daoFactory = null;
    }

    /**
     * The asynchronous initialization. It helps to access the Fedora without
     * blocking the servlet context initialization in case
     * it runs in the same container.
     */
    private void asyncInitialization() {
        FedoraStorageInitializer rsi = new FedoraStorageInitializer(RemoteStorage.getInstance());
        rsi.init();
        UserUtil.initDefaultAdmin();
    }

    /**
     * Creates configuration of the application. The lookup of default properties
     * searches servlet init parameters, system properties and system environment.
     *
     * @param env servlet params
     * @return the configuration
     */
    private AppConfiguration initConfig(Map<String, String> env) {
        try {
            AppConfigurationFactory configFactory = AppConfigurationFactory.getInstance();
            AppConfiguration config = configFactory.create(env);
            configFactory.setDefaultInstance(config);

            config.copyConfigTemplate(config.getConfigHome());

            return configFactory.defaultInstance();
        } catch (AppConfigurationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private void initProarcModel(AppConfiguration config) {
        MetaModelRepository.setInstance(config.getPlugins());
    }

    private DataSource initProarcDb() {
        try {
            DataSource proarcSource = DbUtils.getProarcSource();
            daoFactory = new EmpireDaoFactory(EmpireConfiguration.postgres(proarcSource));
            daoFactory.init();
            return proarcSource;
        } catch (NamingException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private void initUsers(AppConfiguration config, DataSource source, DaoFactory daoFactory) {
        try {
            UserManager manager = UserUtil.createUserManagerPostgressImpl(config, source, daoFactory);
            UserUtil.setDefaultManger(manager);
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private void initImport(AppConfiguration config, DaoFactory daoFactory) {
        ImportBatchManager.setInstance(config, daoFactory);
        ImportBatchManager ibm = ImportBatchManager.getInstance();
        ImportDispatcher importDispatcher = new ImportDispatcher();
        ImportDispatcher.setDefault(importDispatcher);
        importDispatcher.init();
        ImportProcess.resumeAll(ibm, importDispatcher, config);
    }

    private void initWorkflow(AppConfiguration config, DaoFactory daoFactory, UserManager users) {
        try {
            File workflowFile = config.getWorkflowConfiguration();
            WorkflowProfiles.copyDefaultFile(config.getWorkflowConfiguration());
            WorkflowProfiles.setInstance(new WorkflowProfiles(workflowFile));
            WorkflowManager.setInstance(new WorkflowManager(
                    WorkflowProfiles.getInstance(), daoFactory, users));
        } catch (Exception ex) {
            throw new IllegalStateException("The workflow initialization failed!", ex);
        }
    }

}
