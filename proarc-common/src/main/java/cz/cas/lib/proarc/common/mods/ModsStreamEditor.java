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
package cz.cas.lib.proarc.common.mods;

import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.common.mods.custom.Mapping;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.custom.PageMapper;
import cz.cas.lib.proarc.common.mods.custom.PageMapper.Page;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.io.StringReader;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

/**
 * MODS editor implements I/O over Fedora data stream.
 *
 * @author Jan Pokorsky
 */
public final class ModsStreamEditor {

    public static final String DATASTREAM_ID = "BIBLIO_MODS";
    public static final String DATASTREAM_FORMAT_URI = ModsConstants.NS;
    public static final String DATASTREAM_LABEL = "MODS description";

    private final XmlStreamEditor editor;
    private final FedoraObject object;

    private static XmlStreamEditor createEditor(FedoraObject object) {
        XmlStreamEditor editor = object.getEditor(
                FoxmlUtils.inlineProfile(DATASTREAM_ID, DATASTREAM_FORMAT_URI, DATASTREAM_LABEL));
        return editor;
    }

    public ModsStreamEditor(FedoraObject object) {
        this(createEditor(object), object);
    }

    public ModsStreamEditor(XmlStreamEditor editor, FedoraObject object) {
        this.editor = editor;
        this.object = object;
    }

    public ModsDefinition read() throws DigitalObjectException {
        Source src = editor.read();
        if (src == null) {
            // it should never arise; it would need to create datastream again with default data
            throw new DigitalObjectException(object.getPid(), "MODS not initialized!");
        }
        return ModsUtils.unmarshalModsType(src);
    }

    public String readAsString() throws DigitalObjectException {
        ModsDefinition mods = read();
        if (mods != null) {
            return ModsUtils.toXml(mods, true);
        }
        return null;
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    public void write(ModsDefinition mods, long timestamp, String message) throws DigitalObjectException {
        EditorResult marshaled = editor.createResult();
        ModsUtils.marshal(marshaled, mods, true);
        editor.write(marshaled, timestamp, message);
    }

    @Deprecated
    public static ModsDefinition defaultMods33(String pid) {
        ModsDefinition mods = new ModsDefinition();
        mods.setVersion(ModsUtils.VERSION);
        IdentifierDefinition identifierType = new IdentifierDefinition();
        identifierType.setType("uuid");
        String uuid = FoxmlUtils.pidAsUuid(pid);
        identifierType.setValue(uuid);
        mods.getIdentifier().add(identifierType);
        return mods;
    }

    public static ModsDefinition defaultMods(String pid) {
        ModsDefinition mods = new ModsDefinition();
        mods.setVersion(ModsUtils.VERSION);
        addPid(mods, pid);
        return mods;
    }

    public static ModsDefinition addPid(ModsDefinition mods, String pid) {
        String uuid = FoxmlUtils.pidAsUuid(pid);
        for (IdentifierDefinition id : mods.getIdentifier()) {
            if ("uuid".equals(id.getType()) && uuid.equals(id.getValue())) {
                return mods;
            }
        }
        IdentifierDefinition id = new IdentifierDefinition();
        id.setValue(uuid);
        id.setType("uuid");
        mods.getIdentifier().add(0, id);
        return mods;
    }

    @Deprecated
    public ModsDefinition createPage(String pid, String pageIndex, String pageNumber, String pageType) {
        ModsDefinition mods = defaultMods33(pid);
        PageMapper mapper = new PageMapper();
        Page page = mapper.map(mods);
        page.setType(pageType);
        page.setIndex(pageIndex);
        page.setNumber(pageNumber);
        mapper.map(mods, page);
        return mods;
    }

    @Deprecated
    public static ModsDefinition create(String pid, String model) {
        ModsDefinition mods = defaultMods33(pid);
        return create(pid, model, mods);
    }

    @Deprecated
    public static ModsDefinition create33(String pid, String model, String xml) {
        ModsDefinition mods = ModsUtils.unmarshalModsType(new StreamSource(new StringReader(xml)));
        // XXX normalize MODS?
        addPid(mods, pid);
        return create(pid, model, mods);
    }

    @Deprecated
    public static ModsDefinition create(String pid, String model, ModsDefinition mods) {
        MetaModel metaModel = MetaModelRepository.getInstance().find(model);
        if (metaModel != null) {
            String mapper = metaModel.getModsCustomEditor();
            Mapping mapping = new Mapping();
            Object customData = mapping.read(mods, mapper);
            mapping.update(mods, customData, mapper);
        }
        return mods;
    }

}
