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

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.fi.muni.xkremser.editor.client.mods.ModsCollectionClient;
import cz.fi.muni.xkremser.editor.server.mods.ModsCollection;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.util.BiblioModsUtils;
import cz.cas.lib.proarc.webapp.client.rpc.ModsGwtRecord;
import cz.cas.lib.proarc.webapp.client.rpc.ModsGwtService;
import cz.cas.lib.proarc.webapp.server.rest.ImportResource;
import cz.cas.lib.proarc.webapp.server.rest.RestException;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import java.io.IOException;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.ServletException;
import javax.servlet.UnavailableException;

/**
 * Simple MODS provider.
 *
 * TODO: Exceptions? (IAE is handled as server error 500 now).
 * TODO: restrict user access
 *
 * @author Jan Pokorsky
 */
public class ModsGwtServiceProvider extends RemoteServiceServlet implements ModsGwtService {

    private static final Logger LOG = Logger.getLogger(ModsGwtServiceProvider.class.getName());

    private AppConfiguration appConfig;
    private RemoteStorage repository;

    @Override
    public void init() throws ServletException {
        super.init();
        try {
            this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
            this.repository = RemoteStorage.getInstance(appConfig);
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, null, ex);
            throw new UnavailableException("Cannot access Fedora");
        } catch (AppConfigurationException ex) {
            LOG.log(Level.SEVERE, "Invalid editor configuration.", ex);
            throw new UnavailableException("Invalid editor configuration.");
        }
    }

    @Override
    public ModsGwtRecord read(String id, Integer batchId) {
        try {
            FedoraObject fbject = findFedoraObject(id, batchId, false);
            ModsStreamEditor editor = new ModsStreamEditor(fbject);
            ModsType mods = editor.read();
            String xml = ModsUtils.toXml(mods, true);
            int xmlHash = xml.hashCode();
            LOG.log(Level.INFO, "id: {0}, hash: {2}, MODS: {1}", new Object[]{id, xml, xmlHash});
            ModsCollection modsCollection = new ModsCollection();
            modsCollection.setMods(Arrays.asList(mods));
            ModsCollectionClient modsClient = BiblioModsUtils.toModsClient(modsCollection);
            return new ModsGwtRecord(modsClient, editor.getLastModified(), xmlHash);
        } catch (DigitalObjectException ex) {
            throw new IllegalStateException(ex);
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Writes MODS to storage.
     *
     * XXX implement null id handling if necessary
     * XXX implement modification recognition; we could send XML hash in read response and check it here; if unmodified ignore write.
     *     As ModsCollectionClient does not support modification status it would probably require to introduce method isModified(ModsCollectionClient, xmlHash):boolean
     *
     * @param id digital object id
     * @param modsClient MODS
     * @return
     */
    @Override
    public String write(String id, Integer batchId, ModsGwtRecord record) {
        SessionContext session = SessionContext.from(getThreadLocalRequest());
        String oldId = id;
        LOG.log(Level.INFO, "id: {0}, modsClient: {1}, hash: {2}", new Object[]{id, record.getMods(), record.getXmlHash()});
        ModsCollection mods = BiblioModsUtils.toMods(record.getMods());
        ModsType modsType = mods.getMods().get(0);
        String xml = ModsUtils.toXml(modsType, true);
        int xmlHash = xml.hashCode();
        LOG.log(Level.INFO, "id: {0}, hash: {2}, MODS: {1}", new Object[]{id, xml, xmlHash});
        if (xmlHash == record.getXmlHash()) {
            return id;
        }

        try {
            FedoraObject fObject = findFedoraObject(id, batchId, false);
            ModsStreamEditor editor = new ModsStreamEditor(fObject);
            editor.write(modsType, record.getTimestamp(), session.asFedoraLog());
            // DC
            String model = new RelationEditor(fObject).getModel();
            DcStreamEditor dcEditor = new DcStreamEditor(fObject);
            dcEditor.write(modsType, model, dcEditor.getLastModified(), session.asFedoraLog());
            fObject.setLabel(ModsUtils.getLabel(modsType, model));
            fObject.flush();
        } catch (DigitalObjectException ex) {
            throw new IllegalStateException(ex);
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }

        LOG.log(Level.INFO, "written id: {0}, old id: {1}", new Object[]{id, oldId});

        return id;
    }

    @Override
    @Deprecated
    public String getXml(ModsCollectionClient modsCollection) {
        LOG.log(Level.INFO, "modsClient: {0}", new Object[]{modsCollection});
        ModsCollection mods = BiblioModsUtils.toMods(modsCollection);
        String xml = BiblioModsUtils.toXML(mods);
//        sleep(10);
        return xml;
    }

    private void sleep(int timeInSec) {
        long start = 0;
        try {
            LOG.info("sleep");
            start = System.currentTimeMillis();
            Thread.sleep(timeInSec * 1000);
        } catch (InterruptedException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
            LOG.log(Level.INFO, "wake up: {0} ms", (System.currentTimeMillis() - start));
    }

    // XXX temorary; unify with DigitalObjectResource.findFedoraObject to DigitalObject API
    private FedoraObject findFedoraObject(String pid, Integer batchId, boolean readonly) throws IOException {
        FedoraObject fobject;
        if (batchId != null) {
            ImportBatchManager importManager = ImportBatchManager.getInstance();
            BatchItemObject item = importManager.findBatchObject(batchId, pid);
            if (item == null) {
                throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
            }
            if (!readonly) {
                Batch batch = importManager.get(batchId);
                ImportResource.checkBatchState(batch);
            }
            fobject = new LocalStorage().load(pid, item.getFile());
        } else {
            fobject = RemoteStorage.getInstance(appConfig).find(pid);
        }
        return fobject;
    }

}
