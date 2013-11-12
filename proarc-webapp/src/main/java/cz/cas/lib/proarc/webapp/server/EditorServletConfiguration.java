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
package cz.cas.lib.proarc.webapp.server;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireConfiguration;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireDaoFactory;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportDispatcher;
import cz.cas.lib.proarc.common.imports.ImportProcess;
import cz.cas.lib.proarc.common.sql.DbUtils;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.naming.NamingException;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.sql.DataSource;

/**
 * Initializes and destroys web application
 *
 * @author Jan Pokorsky
 */
public final class EditorServletConfiguration implements ServletContextListener {

    private static final Logger LOG = Logger.getLogger(EditorServletConfiguration.class.getName());

    private DaoFactory daoFactory;

    @Override
    public void contextInitialized(ServletContextEvent sce) {
        LOG.fine("contextInitialized");

        AppConfiguration config = initConfig(sce.getServletContext());
        DataSource proarcSource = initProarcDb();
        initUsers(config, proarcSource);
        initImport(config);

    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {
        ImportDispatcher importDispatcher = ImportDispatcher.getDefault();
        importDispatcher.stop();
        LOG.fine("contextDestroyed");
    }

    /**
     * Creates configuration of the application. The lookup of default properties
     * searches servlet init parameters, system properties and system environment.
     *
     * @param ctx servlet context
     * @return the configuration
     */
    private AppConfiguration initConfig(ServletContext ctx) {
        try {
            AppConfigurationFactory configFactory = AppConfigurationFactory.getInstance();
            Map<String, String> env = new HashMap<String, String>();
            readServletParameter(AppConfiguration.PROPERTY_APP_HOME, ctx, env);
            AppConfiguration config = configFactory.create(env);
            configFactory.setDefaultInstance(config);

            config.copyConfigTemplate(config.getConfigHome());

            return configFactory.defaultInstance();
        } catch (AppConfigurationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private static String readServletParameter(String name, ServletContext ctx, Map<String, String> env) {
        String val = ctx.getInitParameter(name);
        if (val != null) {
            LOG.log(Level.INFO, "Init parameter {0}: {1}", new Object[]{name, val});
            env.put(name, val);
        }
        return val;
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

    private void initUsers(AppConfiguration config, DataSource source) {
        try {
            UserManager manager = UserUtil.createUserManagerPostgressImpl(config, source);
            UserUtil.setDefaultManger(manager);
            UserUtil.initDefaultAdmin();
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private void initImport(AppConfiguration config) {
        ImportBatchManager.setInstance(config, daoFactory);
        ImportBatchManager ibm = ImportBatchManager.getInstance();
        ImportDispatcher importDispatcher = new ImportDispatcher();
        ImportDispatcher.setDefault(importDispatcher);
        importDispatcher.init();
        ImportProcess.resumeAll(ibm, importDispatcher);
    }

}
