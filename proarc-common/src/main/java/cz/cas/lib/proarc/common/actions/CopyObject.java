/*
 * Copyright (C) 2019 Lukas Sykora
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

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapperFactory;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.mods.*;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Create digital copy of selected model
 *
 * @author Lukas Sykora
 */
public class CopyObject {
    public static String pidOld = "";
    public static String pidNew = "";
    public static String modelId = "";
    public static AppConfiguration appConfig = null;
    public static DigitalObjectValidationException ex = null;
    public static UserProfile user = null;

    public static final String ERR_COPYVALUE_MISSINGVALUE = "Err_CopyValue_MissingValue";

    public CopyObject(AppConfiguration appConfig, UserProfile user, String pidOld, String modelId) {
        this.appConfig = appConfig;
        this.pidOld = pidOld;
        this.modelId = modelId;
        this.pidNew = FoxmlUtils.createPid();
        this.user = user;
        this.ex = new DigitalObjectValidationException(pidOld, 1001010,
                "Copy Object", "Copy Object validation", null);
    }

    public List<SearchView.Item> copy() throws DigitalObjectException {
        checkValues();
        try {
            RemoteStorage rstorage = RemoteStorage.getInstance(appConfig);
            RemoteStorage.RemoteObject robject = rstorage.find(pidOld);
            MetsContext metsContext = buildContext(robject, null, null, rstorage);
            String parentId = MetsUtils.getParent(pidOld, metsContext.getRemoteStorage());
            DigitalObjectManager dom = DigitalObjectManager.getDefault();

            DigitalObjectManager.CreateHandler handler = dom.create(modelId, pidNew, parentId, user, null, "create new object with pid: " + pidNew);
            List<SearchView.Item> items = null;
            items = handler.create();
            return items;
        }
         catch (Exception ex) {
            //throw new DigitalObjectValidationException(this.pidOld, null, "Nepodarilo se ziskat objekt z DB", "001", ex);
             throw new DigitalObjectException(this.pidOld, "Nepodarilo se zkopirovat objekt", ex);
        }
    }

    public void copyMods(DigitalObjectManager dom) throws DigitalObjectException {
        FedoraObject foOld = dom.find(pidOld, null);
        XmlStreamEditor streamEditorOld = foOld.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditorOld = new ModsStreamEditor(streamEditorOld, foOld);
        ModsDefinition modsOld = modsStreamEditorOld.read();


        FedoraObject foNew = dom.find(pidNew, null);
        XmlStreamEditor streamEditorNew = foNew.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditorNew = new ModsStreamEditor(streamEditorNew, foNew);

        modsStreamEditorNew.write(modsOld, modsStreamEditorNew.getLastModified(), null);
        foNew.flush();
        }

    private void repairStreams(DigitalObjectManager dom) throws DigitalObjectException {
        //repair ModsDatastream
        FedoraObject foNew = dom.find(pidNew, null);
        XmlStreamEditor streamEditorNew = foNew.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditorNew = new ModsStreamEditor(streamEditorNew, foNew);
        ModsDefinition mods = modsStreamEditorNew.read();
        repairIdentifiers(mods);
        removePartNumber(mods);
        removeDateIssued(mods);
        modsStreamEditorNew.write(mods, modsStreamEditorNew.getLastModified(), null);

        //repair DcDatastream
        DigitalObjectHandler handler = new DigitalObjectHandler(foNew, MetaModelRepository.getInstance());
        NdkMapperFactory mapperFactory = new NdkMapperFactory();
        NdkMapper mapper = mapperFactory.get(modelId);
        mapper.setModelId(modelId);
        NdkMapper.Context context = new NdkMapper.Context(handler);
        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = handler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
        dcr.setDc(dc);
        dcEditor.write(handler, dcr, null);

        //repair Label
        String label = mapper.toLabel(mods);
        foNew.setLabel(label);
        foNew.flush();
    }

    private void removeDateIssued(ModsDefinition mods) {
        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            originInfo.getDateIssued().clear();
        }
    }

    private void removePartNumber(ModsDefinition mods) {
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            titleInfo.getPartNumber().clear();
        }
        if (mods.getTitleInfo().size() > 0) {
            StringPlusLanguage partNumber = new StringPlusLanguage();
            partNumber.setValue("Nove vytvoreny objekt  - nutne zmenit PartNumber a DateIssued");
            mods.getTitleInfo().get(0).getPartNumber().add(partNumber);
        }
    }

    private void repairIdentifiers(ModsDefinition mods) {
        List<IdentifierDefinition> identifiers = new ArrayList<>();
        for (IdentifierDefinition identifier : mods.getIdentifier()) {
            if (!"urnnbn".equals(identifier.getType()) && !"uuid".equals(identifier.getType())) {
                identifiers.add(identifier);
            }
        }
        IdentifierDefinition identifierNew = new IdentifierDefinition();
        identifierNew.setType("uuid");
        identifierNew.setValue(getUuid());
        identifiers.add(identifierNew);

        mods.getIdentifier().clear();
        mods.getIdentifier().addAll(identifiers);
    }

    private String getUuid() {
        return this.pidNew.substring(5);
    }


    private MetsContext buildContext(RemoteStorage.RemoteObject fo, String packageId, File targetFolder, RemoteStorage rstorage) {
        MetsContext mc = new MetsContext();
        mc.setFedoraClient(fo.getClient());
        mc.setRemoteStorage(rstorage);
        mc.setPackageID(packageId);
        mc.setOutputPath(null);
        mc.setAllowNonCompleteStreams(false);
        mc.setAllowMissingURNNBN(false);
        mc.setConfig(null);
        return mc;
    }

    private void checkValues() throws DigitalObjectException {
        checkValue(this.pidOld);
        checkValue(this.modelId);
        checkValue(this.pidNew);

    }

    private void checkValue(String value) throws DigitalObjectException {
        if (value == null || value.isEmpty()) {
            throw new DigitalObjectException(this.pidOld, "Chybejici hodnota v poli pid nebo model");
        }
    }

    public void copyMods() throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        copyMods(dom);
        repairStreams(dom);
    }
}
