/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.export.cejsh;

import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConversionException;

/**
 * The CEJSH configuration.
 *
 * @author Jan Pokorsky
 */
public class CejshConfig {

    private static final Logger LOG = Logger.getLogger(CejshConfig.class.getName());
    static final String PROP_XSLCEJSH_URL = "cejsh.mods_to_cejsh.path";
    static final String PROP_XSLCEJSHHEAD_URL = "cejsh.mods_to_cejsh_head.path";
    static final String PROP_JOURNALS_URL = "cejsh.journals.path";
    static final String PROP_DEBUG = "cejsh.debug";

    public static CejshConfig from(Configuration conf) {
        CejshConfig cc = new CejshConfig();
        cc.setXslCejshUrl(conf.getString(PROP_XSLCEJSH_URL, null));
        cc.setXslCejshHeadUrl(conf.getString(PROP_XSLCEJSHHEAD_URL, null));
//        cc.setJournalUrl(conf.getString(PROP_JOURNALS_URL, null));
        try {
            boolean debug = conf.getBoolean(PROP_DEBUG, Boolean.FALSE);
            cc.setLogLevel(debug ? Level.INFO : Level.FINE);
        } catch (ConversionException ex) {
            LOG.log(Level.SEVERE, PROP_DEBUG, ex);
        }
        return cc;
    }

    private String xslCejshUrl;
    private String xslCejshHeadUrl;
    private String journalUrl;
    private Level logLevel = Level.FINE;

    public String getXslCejshUrl() {
        return xslCejshUrl != null ? xslCejshUrl : cpResource("mods_to_cejsh.xsl");
    }

    public void setXslCejshUrl(String xslCejshUrl) {
        this.xslCejshUrl = xslCejshUrl;
    }

    public String getXslCejshHeadUrl() {
        return xslCejshHeadUrl != null ? xslCejshHeadUrl : cpResource("mods_to_cejsh_head.xsl");
    }

    public void setXslCejshHeadUrl(String xslCejshHeadUrl) {
        this.xslCejshHeadUrl = xslCejshHeadUrl;
    }

    public String getJournalUrl() {
        return journalUrl != null ? journalUrl : cpResource("cejsh_journals.xml");
    }

    public void setJournalUrl(String journalUrl) {
        this.journalUrl = journalUrl;
    }

    public Level getLogLevel() {
        return logLevel;
    }

    public void setLogLevel(Level logLevel) {
        this.logLevel = logLevel;
    }

    private static String cpResource(String name) {
        URL r = CejshConfig.class.getResource(name);
        if (r == null) {
            URL rr = CejshConfig.class.getResource(".");
            throw new IllegalArgumentException("Missing " + name + " inside " + rr.toExternalForm());
        }
        return r.toExternalForm();
    }

}
