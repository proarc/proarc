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
package cz.incad.pas.editor.server.config;

import java.util.HashMap;
import java.util.Map;
import javax.servlet.ServletContext;

/**
 *
 * @author Jan Pokorsky
 */
public final class PasConfigurationFactory {

    private static final PasConfigurationFactory INSTANCE = new PasConfigurationFactory();

    private PasConfiguration defaultInstance;

    public static PasConfigurationFactory getInstance() {
        return INSTANCE;
    }

    public PasConfiguration create() {
        return create(new HashMap<String, String>());
    }

    public PasConfiguration create(ServletContext ctx) {
        Map<String, String> env = new HashMap<String, String>();
        env.put(PasConfiguration.CONFIG_FOLDER, ctx.getInitParameter(PasConfiguration.CONFIG_FOLDER));
        return create(env);
    }

    public PasConfiguration create(Map<String, String> environment) {
        if (!environment.containsKey(PasConfiguration.USER_HOME)) {
            environment.put(PasConfiguration.USER_HOME, System.getProperty(PasConfiguration.USER_HOME));
        }
        PasConfiguration pc = new PasConfiguration(environment);
        return pc;
    }

    /** XXX replace with guice */
    public PasConfiguration defaultInstance() {
        if (defaultInstance == null) {
            defaultInstance = create();
        }
        return defaultInstance;
    }

}
