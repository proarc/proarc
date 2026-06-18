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

package cz.cas.lib.proarc.common.catalog.updateCatalog;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.RecordIdentifierDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import java.io.IOException;
import java.util.logging.Logger;
import org.json.JSONException;


/**
 * Class that decides this catalogsUpdate class should be used.
 *
 * @author Lukas Sykora
 */
public class UpdateCatalog {

    private static final Logger LOG = Logger.getLogger(UpdateCatalog.class.getName());

    protected AppConfiguration appConfiguration;
    protected AkubraConfiguration akubraConfiguration;
    protected UserProfile user;

    public UpdateCatalog(AppConfiguration appConfiguration, AkubraConfiguration akubraConfiguration) {
        this.appConfiguration = appConfiguration;
        this.akubraConfiguration = akubraConfiguration;
    }

    protected boolean allowUpdateRecord(CatalogConfiguration catalog) {
        return false;
    }

    public boolean process(CatalogConfiguration bCatalog, String field001, String pid) throws DigitalObjectException, JSONException, IOException {
        throw new IOException("Method is not implemented");
    }

    public static String getObjectField001(String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fo = dom.find(pid, null);

        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();

        for (RecordInfoDefinition recordInfo : mods.getRecordInfo()) {
            for (RecordIdentifierDefinition recordInfoIdentifier : recordInfo.getRecordIdentifier()) {
                return recordInfoIdentifier.getValue();
            }
        }
        return null;
    }
}
