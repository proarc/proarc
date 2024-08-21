/*
 * Copyright (C) 2024 Lukas Sykora
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

package cz.cas.lib.proarc.common.actions;

import cz.cas.lib.proarc.common.catalog.updateCatalog.AlephXmlUpdateCatalog;
import cz.cas.lib.proarc.common.catalog.updateCatalog.UpdateCatalog;
import cz.cas.lib.proarc.common.catalog.updateCatalog.VerbisUpdateCatalog;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import java.io.IOException;
import java.util.logging.Logger;
import org.codehaus.jettison.json.JSONException;


/**
 * Class that operate different type of catalogsUpdate
 *
 * @author Lukas Sykora
 */
public class CatalogRecord {

    private static final Logger LOG = Logger.getLogger(CatalogRecord.class.getName());

    private AppConfiguration appConfig;
    private AkubraConfiguration akubraConfiguration;

    public CatalogRecord(AppConfiguration appConfig, AkubraConfiguration akubraConfiguration) {
        this.appConfig = appConfig;
        this.akubraConfiguration = akubraConfiguration;
    }


    public boolean update(String catalogId, String pid) throws DigitalObjectException, JSONException, IOException {

        String field001 = UpdateCatalog.getObjectField001(pid);
        if (field001 == null || field001.isEmpty()) {
            throw new DigitalObjectException(pid, "Missing field 001");
        }

        CatalogConfiguration bCatalog = appConfig.getCatalogs().findConfiguration(catalogId);

        if (bCatalog != null) {
            UpdateCatalog updateCatalog = null;
            if (VerbisUpdateCatalog.ID.equals(bCatalog.getCatalogUpdateType())) {
                updateCatalog = new VerbisUpdateCatalog(appConfig, akubraConfiguration);
            } else if (AlephXmlUpdateCatalog.ID.equals(bCatalog.getCatalogUpdateType())) {
                updateCatalog = new AlephXmlUpdateCatalog(appConfig, akubraConfiguration);
            } else {
                LOG.severe("Unsupported updateType for catalog configuration id " + catalogId);
                throw new IOException("Unsupported updateType for catalog configuration id " + catalogId);
            }
            return updateCatalog.process(bCatalog, field001, pid);
        } else {
            LOG.severe("No catalog configuration for id " + catalogId);
            throw new IOException("No catalog configuration for id " + catalogId);
        }
    }
}
