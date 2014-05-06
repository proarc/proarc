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
package cz.cas.lib.proarc.authentication;

import cz.cas.lib.proarc.authentication.desa.DESAAuthenticator;
import cz.cas.lib.proarc.authentication.proarc.ProArcAuthenticator;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

/**
 * The authenticators configuration.
 *
 * @author Jan Pokorsky
 */
public final class Authenticators {

    public static final String PROPERTY_AUTHENTICATORS = "authenticators";
    /** The internal authenticator. */
    public static final String TYPE_PROARC = "proarc";
    /** The DESA authenticator. For now it stands for both type and ID. */
    public static final String TYPE_DESA = "desa";

    private static final Logger LOG = Logger.getLogger(Authenticators.class.getName());
    private static Authenticators INSTANCE;

    private final Configuration conf;

    public static Authenticators getInstance() {
        if (INSTANCE == null) {
            throw new IllegalStateException("set instance first!");
        }
        return INSTANCE;
    }

    public static void setInstance(Authenticators authenticators) {
        INSTANCE = authenticators;
    }

    public Authenticators(Configuration conf) {
        this.conf = conf;
    }

    public List<Authenticator> getAuthenticators() {
        List<Object> authenticatorIds = conf.getList(PROPERTY_AUTHENTICATORS);
        LinkedHashSet<Object> ids = new LinkedHashSet<Object>(authenticatorIds);
        // ensure the ProArc authenticator used as a last resort
        ids.remove(TYPE_PROARC);
        ids.add(TYPE_PROARC);
        ArrayList<Authenticator> authenticators = new ArrayList<Authenticator>(ids.size());
        for (Object id : ids) {
            if (TYPE_PROARC.equals(id)) {
                authenticators.add(new ProArcAuthenticator());
            } else if (TYPE_DESA.equals(id)) {
                authenticators.add(new DESAAuthenticator());
            } else {
                LOG.warning("Unknown authenticator: " + id);
            }
        }
        return authenticators;
    }

    public String getLoginType() {
        List<Object> authenticatorIds = conf.getList(PROPERTY_AUTHENTICATORS);
        for (Object aid : authenticatorIds) {
            if (TYPE_DESA.equals(aid)) {
                return TYPE_DESA;
            }
        }
        return TYPE_PROARC;
    }

}
