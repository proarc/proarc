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
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
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

    private ProarcInitializer asyncInitializer;

    @Override
    public void contextInitialized(ServletContextEvent sce) {
        LOG.fine("contextInitialized");

        Map<String, String> env = new HashMap<String, String>();
        readServletParameter(AppConfiguration.PROPERTY_APP_HOME, sce.getServletContext(), env);
        asyncInitializer = ProarcInitializer.getInstance();
        asyncInitializer.start(env);
    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {
        asyncInitializer.destroy();
        asyncInitializer = null;
        LOG.fine("contextDestroyed");
    }

    private static String readServletParameter(String name, ServletContext ctx, Map<String, String> env) {
        String val = ctx.getInitParameter(name);
        if (val != null) {
            LOG.log(Level.INFO, "Init parameter {0}: {1}", new Object[]{name, val});
            env.put(name, val);
        }
        return val;
    }

}
