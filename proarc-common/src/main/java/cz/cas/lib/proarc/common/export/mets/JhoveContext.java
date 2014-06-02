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
package cz.cas.lib.proarc.common.export.mets;

import edu.harvard.hul.ois.jhove.App;
import edu.harvard.hul.ois.jhove.JhoveBase;
import java.io.File;
import org.apache.commons.io.FileUtils;

/**
 * The context helps to share JHOVE configuration.
 *
 * @author Jan Pokorsky
 */
public class JhoveContext {

    private JhoveBase jhoveBase;
    private File jhoveConfig;
    private App jhoveApp;

    JhoveContext(JhoveBase jhoveBase, File jhoveConfig, App jhoveApp) {
        this.jhoveBase = jhoveBase;
        this.jhoveConfig = jhoveConfig;
        this.jhoveApp = jhoveApp;
    }

    JhoveBase getJhoveBase() {
        return jhoveBase;
    }

    public File getConfigFolder() {
        return jhoveConfig;
    }

    App getJhoveApp() {
        return jhoveApp;
    }

    /**
     * Removes temporary resources.
     */
    public void destroy() {
        jhoveApp = null;
        jhoveBase = null;
        FileUtils.deleteQuietly(jhoveConfig);
    }

}
