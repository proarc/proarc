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

import cz.incad.pas.editor.server.config.PasConfiguration;
import cz.incad.pas.editor.server.config.PasConfigurationException;
import cz.incad.pas.editor.server.config.PasConfigurationFactory;
import cz.incad.pas.editor.server.imports.ImportBatchManager;
import cz.incad.pas.editor.server.imports.ImportDispatcher;
import cz.incad.pas.editor.server.imports.ImportProcess;
import java.util.logging.Logger;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

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

        PasConfiguration config = initConfig(sce.getServletContext());
        initImport(config);

    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {
        ImportDispatcher importDispatcher = ImportDispatcher.getDefault();
        importDispatcher.stop();
        LOG.fine("contextDestroyed");
    }

    private PasConfiguration initConfig(ServletContext ctx) {
        try {
            PasConfigurationFactory configFactory = PasConfigurationFactory.getInstance();
            PasConfiguration config = configFactory.create(ctx);
            configFactory.setDefaultInstance(config);
            return configFactory.defaultInstance();
        } catch (PasConfigurationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private void initImport(PasConfiguration config) {
        ImportBatchManager ibm = ImportBatchManager.getInstance(config);
        ImportDispatcher importDispatcher = new ImportDispatcher();
        ImportDispatcher.setDefault(importDispatcher);
        importDispatcher.init();
        ImportProcess.resumeAll(ibm, importDispatcher);
    }

}
