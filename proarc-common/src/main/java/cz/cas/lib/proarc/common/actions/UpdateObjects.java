/*
 * Copyright (C) 2025 Lukas Sykora
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

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import static cz.cas.lib.proarc.common.object.ndk.ModsRules.ERR_NDK_PHYSICALLOCATION_SIGLA;

/**
 * @author Lukas Sykora
 */
public class UpdateObjects {

    private AppConfiguration appConfig;
    private AkubraConfiguration akubraConfig;
    private Locale locale;

    private List<String> updatedPids;
    private Integer partNumberValue;
    private String signaturaValue;
    private String siglaValue;

    public UpdateObjects(AppConfiguration appConfig, AkubraConfiguration akubraConfig, Locale locale) {
        this.appConfig = appConfig;
        this.akubraConfig = akubraConfig;
        this.locale = locale;
    }

    private static String trim(String value, String start, String end) {
        if (value != null && !value.isEmpty()) {
            if (value.startsWith(start)) {
                value = value.substring(start.length());
            }
            if (value.endsWith(end)) {
                value = value.substring(0, value.length() - end.length());
            }
        }
        return value;
    }

    public static List<String> createListFromArray(String pidsArray) {
        pidsArray = trim(pidsArray, "{", "}");
        pidsArray = trim(pidsArray, "[", "]");
        String[] selectedPids = pidsArray.split(",");
        return Arrays.asList(selectedPids);
    }

    public void createListOfPids(List<String> pids) throws IOException, FedoraClientException, DigitalObjectException {
        SearchView search = null;
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            FedoraStorage storage = FedoraStorage.getInstance(appConfig);
            search = storage.getSearch(locale);
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfig);
            search = akubraStorage.getSearch(locale);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }

        updatedPids = new ArrayList<>();

        for (String pid : pids) {
            List<SearchViewItem> items = search.find(pid);
            if (items.isEmpty()) {
                throw new DigitalObjectException(pid, "Unknown pid \"" + pid + "\".");
            }
            for (SearchViewItem item : items) {
                if (NdkPlugin.MODEL_PERIODICALISSUE.equals(item.getModel()) || NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(item.getModel())
                        || NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(item.getModel()) || NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT.equals(item.getModel())) {
                    updatedPids.add(item.getPid());
                } else {
                    throw new DigitalObjectException(item.getPid(), "Unsupported model \"" + item.getModel() + "\" for multi update objets.");
                }
            }
        }
    }

    public void updateObjects(String signatura, String sigla) throws DigitalObjectException {
        this.signaturaValue = signatura;
        this.siglaValue = sigla;
        if (updatedPids!= null && !updatedPids.isEmpty()) {
            if (sigla != null && !sigla.isEmpty()) {
                List<String> accepted = appConfig.getModsOptions().getAcceptableSiglaId();
                if (!accepted.contains(sigla)) {
                    DigitalObjectValidationException ex = new DigitalObjectValidationException(updatedPids.get(0), null,
                            ModsStreamEditor.DATASTREAM_ID, "MODS validation", null);
                    ex.addValidation("MODS rules", ERR_NDK_PHYSICALLOCATION_SIGLA, false, sigla);
                    throw ex;
                }
            }
        }

        for (String pid : updatedPids) {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            ProArcObject fo = dom.find(pid, null);
            String model = new RelationEditor(fo).getModel();
            DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
            NdkMapper.Context context = new NdkMapper.Context(handler);
            NdkMapper mapper = NdkMapper.get(model);
            mapper.setModelId(model);

            XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                    MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                    MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
            ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
            ModsDefinition mods = modsStreamEditor.read();

            updateMods(mods, model);

            mapper.createMods(mods, context);
            modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);
            OaiDcType dc = mapper.toDc(mods, context);
            DcStreamEditor dcEditor = handler.objectMetadata();
            DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
            dcr.setDc(dc);
            dcEditor.write(handler, dcr, null);

            fo.setLabel(mapper.toLabel(mods));
            fo.flush();
        }
    }


    private void updateMods(ModsDefinition mods, String model) throws DigitalObjectException {
        if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(model)) {
            setSiglaAndSignatura(mods);
            setPartNumber(mods);
        } else if (NdkPlugin.MODEL_PERIODICALISSUE.equals(model)) {
            setSiglaAndSignatura(mods);
            setPartNumber(mods);
        } else if (NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT.equals(model)) {
            setSiglaAndSignatura(mods);
            setPartNumber(mods);
        } else if (NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(model)) {
            setSiglaAndSignatura(mods);
            setPartNumber(mods);
        } else {
            throw new DigitalObjectException("Unsupported model: " + model);
        }
    }

    private void setPartNumber(ModsDefinition mods) {
        if (this.partNumberValue == null) {
            return;
        } else if (this.partNumberValue == -1) {
            if (mods.getTitleInfo().isEmpty()) {
                mods.getTitleInfo().add(new TitleInfoDefinition());
            }
            for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
                if (titleInfo.getPartNumber().isEmpty()) {
                    titleInfo.getPartNumber().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage partNumber : titleInfo.getPartNumber()) {
                    partNumber.setValue(null);
                }
            }
        } else if (this.partNumberValue > -1) {
            if (mods.getTitleInfo().isEmpty()) {
                mods.getTitleInfo().add(new TitleInfoDefinition());
            }
            for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
                if (titleInfo.getPartNumber().isEmpty()) {
                    titleInfo.getPartNumber().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage partNumber : titleInfo.getPartNumber()) {
                    partNumber.setValue(String.valueOf(this.partNumberValue));
                    this.partNumberValue++;
                }
            }
        }
    }

    private void setSiglaAndSignatura(ModsDefinition mods) {
        if (this.siglaValue != null || this.signaturaValue != null) {
            if (mods.getLocation().isEmpty()) {
                mods.getLocation().add(new LocationDefinition());
            }
            for (LocationDefinition location : mods.getLocation()) {
                if (this.siglaValue != null) {
                    if (location.getPhysicalLocation().isEmpty()) {
                        location.getPhysicalLocation().add(new PhysicalLocationDefinition());
                    }
                    for (PhysicalLocationDefinition physicalLocation : location.getPhysicalLocation()) {
                        physicalLocation.setValue(this.siglaValue);
                        physicalLocation.setAuthority("siglaADR");
                    }
                }
                if (this.signaturaValue != null) {
                    if (location.getShelfLocator().isEmpty()) {
                        location.getShelfLocator().add(new StringPlusLanguage());
                    }
                    for (StringPlusLanguage shelfLocator : location.getShelfLocator()) {
                        shelfLocator.setValue(this.signaturaValue);
                    }
                }
            }
            if (mods.getLocation().size() > 0) {
                LocationDefinition location = mods.getLocation().get(0);
                mods.getLocation().clear();
                mods.getLocation().add(location);
            }
        }
    }

    public void createPartNumber(String partNumber) throws DigitalObjectException {
        if (partNumber != null && !partNumber.isEmpty()) {
            try {
                this.partNumberValue = Integer.parseInt(partNumber.replaceAll("[^0-9]", ""));
            } catch (Exception ex) {
                throw new DigitalObjectException(null, "Nepodařilo se ziskat hodnotu partNumber");
            }
        } else if (partNumber != null && partNumber.isEmpty()) {
            this.partNumberValue = null;
        }
    }
}