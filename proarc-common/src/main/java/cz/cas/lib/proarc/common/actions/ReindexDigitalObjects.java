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
import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.File;
import java.io.IOException;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

/**
 * Reindex all digital objects
 *
 * @author Lukas Sykora
 */
public class ReindexDigitalObjects {


    public static String pid;
    public static String modelId;
    public static AppConfiguration appConfig;
    public static UserProfile user;

    public ReindexDigitalObjects(AppConfiguration appConfig, UserProfile user, String pid, String modelId) {
        this.appConfig = appConfig;
        this.user = user;
        this.pid = pid;
        this.modelId = modelId;
    }

    public IMetsElement getParentElement() throws DigitalObjectException {
        try {
            String parentPid = getParentPid(this.pid);
            return getParentElement(parentPid);

        } catch (Exception ex) {
            throw new DigitalObjectException(this.pid, "nepodarilo se reindexovat strany", ex);
        }
    }

    private IMetsElement getParentElement(String parentPid) throws IOException, MetsExportException {
        RemoteStorage rstorage = RemoteStorage.getInstance(appConfig);
        RemoteStorage.RemoteObject robject = rstorage.find(parentPid);
        MetsContext metsContext = buildContext(robject, null, null, rstorage);
        DigitalObject dobj = MetsUtils.readFoXML(robject.getPid(), robject.getClient());
        if (dobj == null) {
            return null;
        }
        return MetsElement.getElement(dobj, null, metsContext, true);
    }

    private String getParentPid(String pid) throws IOException, MetsExportException {
        RemoteStorage rstorage = RemoteStorage.getInstance(appConfig);
        RemoteStorage.RemoteObject robject = rstorage.find(pid);
        MetsContext metsContext = buildContext(robject, null, null, rstorage);
        return MetsUtils.getParent(pid, metsContext.getRemoteStorage());
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


    public void reindex(IMetsElement parentElement) throws DigitalObjectException {
        int pageIndex = 1;
        int audioPageIndex = 1;

        for (IMetsElement childElement : parentElement.getChildren()) {
            if (Const.PAGE.equals(childElement.getElementType())) {
                String pid = childElement.getOriginalPid();
                reindexMods(pageIndex++, pid, fixModel(childElement.getModel()));
                reindexDc(pid, fixModel(childElement.getModel()));
            }
            if (Const.SOUND_PAGE.equals(childElement.getElementType())) {
                String pid = childElement.getOriginalPid();
                reindexMods(audioPageIndex++, pid, fixModel(childElement.getModel()));
                reindexDc(pid, fixModel(childElement.getModel()));
            }
           /* try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }*/
        }
    }

    private String fixModel(String model) {
        if (model.startsWith("info:fedora/")) {
            return model.substring(12);
        }
        return model;
    }

    private void reindexMods(int index, String pid, String model) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fo = dom.find(pid, null);
        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();
        setIndexToMods(mods, index);
        modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);

        fo.flush();
    }

    private void reindexDc(String pid, String model) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fo = dom.find(pid, null);
        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();

        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
        NdkMapper mapper = NdkMapper.get(model);
        mapper.setModelId(model);

        NdkMapper.Context context = new NdkMapper.Context(handler);
        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = handler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
        dcr.setDc(dc);
        dcEditor.write(handler, dcr, null);

        fo.setLabel(mapper.toLabel(mods));
        fo.flush();
    }

    private void setIndexToMods(ModsDefinition mods, int index) {
        if (mods != null && mods.getPart() != null) {
            for (PartDefinition part : mods.getPart()) {
                DetailDefinition detailDefinition = null;
                for (DetailDefinition detail : part.getDetail()) {
                    if (detail != null && "pageIndex".equals(detail.getType())) {
                        if (detailDefinition == null) {
                            detailDefinition = detail;
                            break;
                        }
                    }
                }
                if (part.getType() == null) {
                    if (detailDefinition == null) {
                        detailDefinition = new DetailDefinition();
                        part.getDetail().add(detailDefinition);
                        detailDefinition.setType("pageIndex");
                    }
                    detailDefinition.getNumber().clear();
                    StringPlusLanguage number = new StringPlusLanguage();
                    number.setValue(String.valueOf(index));
                    detailDefinition.getNumber().add(number);
                }
            }
        }
    }
}
