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
import cz.fi.muni.xkremser.editor.server.mods.IdentifierType;
import cz.fi.muni.xkremser.editor.server.mods.ModsCollection;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.util.BiblioModsUtils;
import cz.incad.pas.editor.client.rpc.ModsGwtRecord;
import cz.incad.pas.editor.client.rpc.ModsGwtService;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository.ModsRecord;
import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXB;

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

    /**
     * Creates new MODS.
     * 
     * @param pid PID of the digital object referencing MODS
     * @param modsXml prepared MODS e.g. from remote catalog or {@code null}
     */
    public static ModsCollection newMods(String pid, String modsXml) {
//        LOG.log(Level.INFO, "new pid: {0}, mods: {1}", new Object[]{pid, modsXml});
        ModsType modsType;
        if (modsXml == null || modsXml.isEmpty()) {
            modsType = new ModsType();
        } else {
            try {
                modsType = JAXB.unmarshal(new ByteArrayInputStream(modsXml.getBytes("UTF-8")), ModsType.class);
            } catch (UnsupportedEncodingException ex) {
                throw new IllegalStateException(ex);
            }
        }
        ModsCollection mods = new ModsCollection();
        mods.setMods(Arrays.asList(modsType));

        // add uuid as identifier
        IdentifierType identifierType = new IdentifierType();
        identifierType.setType("uuid");
        identifierType.setValue(pid.substring("uuid:".length()));

        modsType.getModsGroup().add(identifierType);
        return mods;
    }
    
//    @Override
//    public void init() throws ServletException {
//        ObjectFactory objFactory = new ObjectFactory();
//
//        TitleInfoType titleInfoType = new TitleInfoType();
//        titleInfoType.getTitleOrSubTitleOrPartNumber().add(objFactory.createBaseTitleInfoTypeTitle("Title Sample"));
//        IdentifierType identifierType = new IdentifierType();
//        identifierType.setType("UUID");
//        identifierType.setValue("UUID-test value");
//
//        DetailType detailType = new DetailType();
//        detailType.setType("pageNumber");
//        detailType.getNumberOrCaptionOrTitle().add(objFactory.createDetailTypeNumber("3"));
//        PartType partType = new PartType();
//        partType.setType("TitlePage");
//        partType.getDetailOrExtentOrDate().add(detailType);
//
//        ModsType modsType = new ModsType();
//        modsType.setModsGroup(Arrays.<Object>asList(titleInfoType, partType, identifierType));
//
//        ModsCollection mods = new ModsCollection();
//        mods.setMods(Arrays.asList(modsType));
//
//        STORAGE.put("id:sample", mods);
//    }


    @Override
    public ModsGwtRecord read(String id) {
//        if (true) {
//            throw new IllegalArgumentException("Invalid id: " + id);
//        }
        DigitalObjectRepository repository = DigitalObjectRepository.getInstance();
        ModsRecord modsRec = repository.getMods(id);
        if (modsRec == null) {
            throw new IllegalArgumentException("Invalid id: " + id);
        }
        ModsCollection mods = modsRec.getMods();
        String xml = mods == null ? null : BiblioModsUtils.toXML(mods);
        int xmlHash = xml == null ? 0 : xml.hashCode();
        LOG.log(Level.INFO, "id: {0}, hash: {2}, MODS: {1}", new Object[]{id, xml, xmlHash});
//        sleep(10);
        ModsCollectionClient modsClient = mods == null
                ? new ModsCollectionClient() : BiblioModsUtils.toModsClient(mods);
        return new ModsGwtRecord(modsClient, modsRec.getTimestamp(), xmlHash);
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
        String xml = BiblioModsUtils.toXML(mods);
        int xmlHash = xml.hashCode();
        LOG.log(Level.INFO, "id: {0}, hash: {2}, MODS: {1}", new Object[]{id, xml, xmlHash});

        DigitalObjectRepository repository = DigitalObjectRepository.getInstance();
        repository.updateMods(new ModsRecord(id, mods, System.currentTimeMillis()), 1);

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
