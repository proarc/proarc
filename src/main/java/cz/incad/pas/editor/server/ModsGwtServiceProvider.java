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

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import cz.fi.muni.xkremser.editor.client.mods.ModsCollectionClient;
import cz.fi.muni.xkremser.editor.server.mods.ModsCollection;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.util.BiblioModsUtils;
import cz.incad.pas.editor.client.rpc.ModsGwtRecord;
import cz.incad.pas.editor.client.rpc.ModsGwtService;
import cz.incad.pas.editor.server.config.PasConfiguration;
import cz.incad.pas.editor.server.config.PasConfigurationException;
import cz.incad.pas.editor.server.config.PasConfigurationFactory;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
import cz.incad.pas.editor.server.mods.ModsUtils;
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

    private PasConfiguration pasConfig;
    private RemoteStorage repository;

    @Override
    public void init() throws ServletException {
        super.init();
        try {
            this.pasConfig = PasConfigurationFactory.getInstance().defaultInstance();
            this.repository = RemoteStorage.getInstance(pasConfig);
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, null, ex);
            throw new UnavailableException("Cannot access Fedora");
        } catch (PasConfigurationException ex) {
            LOG.log(Level.SEVERE, "Invalid editor configuration.", ex);
            throw new UnavailableException("Invalid editor configuration.");
        }
    }

    @Override
    public ModsGwtRecord read(String id) {
//        if (true) {
//            throw new IllegalArgumentException("Invalid id: " + id);
//        }
        RemoteObject remote = repository.find(id);
        ModsStreamEditor editor = new ModsStreamEditor(remote);
        ModsType mods = editor.read();
        if (mods == null) {
            throw new IllegalArgumentException("Invalid id: " + id);
        }
        String xml = ModsUtils.toXml(mods, true);
        int xmlHash = xml.hashCode();
        LOG.log(Level.INFO, "id: {0}, hash: {2}, MODS: {1}", new Object[]{id, xml, xmlHash});
//        sleep(10);
        ModsCollection modsCollection = new ModsCollection();
        modsCollection.setMods(Arrays.asList(mods));
        ModsCollectionClient modsClient = BiblioModsUtils.toModsClient(modsCollection);
        return new ModsGwtRecord(modsClient, editor.getLastModified(), xmlHash);
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
    public String write(String id, ModsGwtRecord record) {
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

        RemoteObject remote = repository.find(id);
        ModsStreamEditor editor = new ModsStreamEditor(remote);
        editor.write(modsType, record.getTimestamp());
        // DC, XXX RELS-EXT required to get model
        DcStreamEditor dcEditor = new DcStreamEditor(remote);
        try {
            dcEditor.write(modsType, "XXX-model", dcEditor.getLastModified());
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
        remote.flush();

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


}
