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
import cz.fi.muni.xkremser.editor.server.mods.DetailType;
import cz.fi.muni.xkremser.editor.server.mods.IdentifierType;
import cz.fi.muni.xkremser.editor.server.mods.ModsCollection;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.fi.muni.xkremser.editor.server.mods.PartType;
import cz.fi.muni.xkremser.editor.server.mods.TitleInfoType;
import cz.fi.muni.xkremser.editor.server.util.BiblioModsUtils;
import cz.incad.pas.editor.client.rpc.ModsGwtService;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.ServletException;

/**
 * Simple MODS provider.
 *
 * TODO: connect to fedora
 * TODO: implement concurrent access (timestamps)
 * TODO: Exceptions? (IAE is handled as server error 500 now).
 * TODO: restrict user access
 *
 * @author Jan Pokorsky
 */
public class ModsGwtServiceProvider extends RemoteServiceServlet implements ModsGwtService {

    private static final Logger LOG = Logger.getLogger(ModsGwtServiceProvider.class.getName());

    private static final Map<String, ModsCollection> STORAGE = new HashMap<String, ModsCollection>();

    @Override
    public void init() throws ServletException {
        ObjectFactory objFactory = new ObjectFactory();

        TitleInfoType titleInfoType = new TitleInfoType();
        titleInfoType.getTitleOrSubTitleOrPartNumber().add(objFactory.createBaseTitleInfoTypeTitle("Title Sample"));
        IdentifierType identifierType = new IdentifierType();
        identifierType.setType("UUID");
        identifierType.setValue("UUID-test value");

        DetailType detailType = new DetailType();
        detailType.setType("pageNumber");
        detailType.getNumberOrCaptionOrTitle().add(objFactory.createDetailTypeNumber("3"));
        PartType partType = new PartType();
        partType.setType("TitlePage");
        partType.getDetailOrExtentOrDate().add(detailType);

        ModsType modsType = new ModsType();
        modsType.setModsGroup(Arrays.<Object>asList(titleInfoType, partType, identifierType));

        ModsCollection mods = new ModsCollection();
        mods.setMods(Arrays.asList(modsType));

        STORAGE.put("id:sample", mods);
    }


    @Override
    public ModsCollectionClient read(String id) {
        if (true) {
            throw new IllegalArgumentException("Invalid id: " + id);
        }
        ModsCollection mods;
        synchronized (STORAGE) {
            mods = STORAGE.get(id);
            if (mods == null) {
                throw new IllegalArgumentException("Invalid id: " + id);
            }
        }
        LOG.log(Level.INFO, "id: {0}, MODS: {1}", new Object[]{id, BiblioModsUtils.toXML(mods)});
//        sleep(10);
        return BiblioModsUtils.toModsClient(mods);
    }

    @Override
    public String write(String id, ModsCollectionClient modsClient) {
        String oldId = id;
        LOG.log(Level.INFO, "id: {0}, modsClient: {1}", new Object[]{id, modsClient});
        ModsCollection mods = BiblioModsUtils.toMods(modsClient);
        String xml = BiblioModsUtils.toXML(mods);
        LOG.log(Level.INFO, "id: {0}, MODS: {1}", new Object[]{id, xml});

        synchronized(STORAGE) {
            if (id == null) {
                id = "id:" + STORAGE.size();
            }
            STORAGE.put(id, mods);
        }

        LOG.log(Level.INFO, "written id: {0}, old id: {1}", new Object[]{id, oldId});

        return id;
    }

    @Override
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
