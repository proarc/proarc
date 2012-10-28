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
package cz.incad.pas.editor.server;

import cz.incad.pas.editor.server.config.AppConfiguration;
import cz.incad.pas.editor.server.config.AppConfigurationException;
import cz.incad.pas.editor.server.config.AppConfigurationFactory;
import cz.incad.pas.editor.server.imports.ImportBatchManager;
import cz.incad.pas.editor.server.imports.ImportDispatcher;
import cz.incad.pas.editor.server.imports.ImportProcess;
import cz.incad.pas.editor.server.sql.DbUtils;
import cz.incad.pas.editor.server.user.UserManager;
import cz.incad.pas.editor.server.user.UserUtil;
import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
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

    private AppConfiguration initConfig(ServletContext ctx) {
        try {
            AppConfigurationFactory configFactory = AppConfigurationFactory.getInstance();
            AppConfiguration config = configFactory.create(ctx);
            configFactory.setDefaultInstance(config);
            return configFactory.defaultInstance();
        } catch (AppConfigurationException ex) {
            throw new IllegalStateException(ex);
        }
    }
    
    private DataSource initProarcDb() {
        try {
            DataSource proarcSource = DbUtils.getProarcSource();
            Connection c = proarcSource.getConnection();
            try {
                c.setAutoCommit(true);
                DbUtils.getProarcSchemaVersion(c);
            } finally {
                c.close();
            }
            return proarcSource;
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
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
        ImportBatchManager ibm = ImportBatchManager.getInstance(config);
        ImportDispatcher importDispatcher = new ImportDispatcher();
        ImportDispatcher.setDefault(importDispatcher);
        importDispatcher.init();
        ImportProcess.resumeAll(ibm, importDispatcher);
    }

}
