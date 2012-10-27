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
package cz.incad.pas.editor.server.mods;

import cz.fi.muni.xkremser.editor.server.mods.IdentifierType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.incad.pas.editor.server.fedora.DigitalObjectException;
import cz.incad.pas.editor.server.fedora.FedoraObject;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalXmlStreamEditor;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteXmlStreamEditor;
import cz.incad.pas.editor.server.fedora.XmlStreamEditor;
import cz.incad.pas.editor.server.fedora.XmlStreamEditor.EditorResult;
import cz.incad.pas.editor.server.mods.custom.Mapping;
import cz.incad.pas.editor.server.mods.custom.PageMapper;
import cz.incad.pas.editor.server.mods.custom.PageMapper.Page;
import cz.incad.pas.editor.server.rest.DigitalObjectResource.MetaModel;
import cz.incad.pas.editor.server.rest.DigitalObjectResource.MetaModelRepository;
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
    public static final String DATASTREAM_FORMAT_URI = "http://www.loc.gov/mods/v3";
    public static final String DATASTREAM_LABEL = "MODS description";

    private final XmlStreamEditor editor;
    private final FedoraObject object;

    private static XmlStreamEditor createEditor(FedoraObject object) {
        XmlStreamEditor editor;
        if (object instanceof LocalObject) {
            editor = new LocalXmlStreamEditor((LocalObject) object, DATASTREAM_ID, DATASTREAM_FORMAT_URI, DATASTREAM_LABEL);
        } else if (object instanceof RemoteObject) {
            editor = new RemoteXmlStreamEditor(
                    (RemoteObject) object,
                    RemoteXmlStreamEditor.inlineProfile(DATASTREAM_ID, DATASTREAM_FORMAT_URI, DATASTREAM_LABEL));
        } else {
            throw new IllegalArgumentException("Unsupported fedora object: " + object.getClass());
        }
        return editor;
    }

    public ModsStreamEditor(FedoraObject object) {
        this(createEditor(object), object);
    }

    ModsStreamEditor(XmlStreamEditor editor, FedoraObject object) {
        this.editor = editor;
        this.object = object;
    }

    public ModsType read() throws DigitalObjectException {
        Source src = editor.read();
        if (src == null) {
            // it should never arise; it would need to create datastream again with default data
            throw new DigitalObjectException(object.getPid(), "MODS not initialized!");
        }
        return ModsUtils.unmarshalModsType(src);
    }

    public String readAsString() throws DigitalObjectException {
        ModsType mods = read();
        if (mods != null) {
            return ModsUtils.toXml(mods, true);
        }
        return null;
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    public void write(ModsType mods, long timestamp, String message) throws DigitalObjectException {
        EditorResult marshaled = editor.createResult();
        ModsUtils.marshal(marshaled, mods, true);
        editor.write(marshaled, timestamp, message);
    }

    public static ModsType defaultMods(String pid) {
        ModsType mods = new ModsType();
        mods.setVersion("3.4");
        IdentifierType identifierType = new IdentifierType();
        identifierType.setType("uuid");
        identifierType.setValue(pid.substring("uuid:".length()));
        mods.getModsGroup().add(identifierType);
        return mods;
    }

    public ModsType createPage(String pid, String pageIndex, String pageNumber, String pageType) {
        ModsType mods = defaultMods(pid);
        PageMapper mapper = new PageMapper();
        Page page = mapper.map(mods);
        page.setType(pageType);
        page.setIndex(pageIndex);
        page.setNumber(pageNumber);
        mapper.map(mods, page);
        return mods;
    }

    public void updatePage(ModsType mods, String pageIndex, String pageNumber, String pageType) {
        PageMapper mapper = new PageMapper();
        Page page = mapper.map(mods);
        if (pageIndex != null) {
            page.setIndex(pageIndex.isEmpty() ? null : pageIndex);
        }
        if (pageNumber != null) {
            page.setNumber(pageNumber.isEmpty() ? null : pageNumber);
        }
        if (pageType != null) {
            page.setType(pageType.isEmpty() ? null : pageType);
        }
        mapper.map(mods, page);
    }

    public ModsType create(String pid, String model) {
        ModsType mods = defaultMods(pid);
        return create(pid, model, mods);
    }
    
    public ModsType create(String pid, String model, String xml) {
        ModsType mods = ModsUtils.unmarshalModsType(new StreamSource(new StringReader(xml)));
        // XXX normalize MODS?
        return create(pid, model, mods);
    }

    public ModsType create(String pid, String model, ModsType mods) {
        MetaModel metaModel = MetaModelRepository.getInstance().find(model);
        if (metaModel != null) {
            String mapper = metaModel.getEditor();
            Mapping mapping = new Mapping();
            Object customData = mapping.read(mods, mapper);
            mapping.update(mods, customData, mapper);
        }
        return mods;
    }

}
