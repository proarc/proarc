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
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusSupplied;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import static cz.cas.lib.proarc.common.object.ndk.ModsRules.DatumValidator;
import static cz.cas.lib.proarc.common.object.ndk.ModsRules.ERR_NDK_ORIGININFO_DATEISSSUED;
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
    private String dateIssuedValue;
    private String signaturaValue;
    private String siglaValue;
    private String titleValue;
    private String subTitleValue;
    private String partNameValue;
    private String noteValue;
    private String publisherValue;
    private String placeValue;

    public UpdateObjects(AppConfiguration appConfig, AkubraConfiguration akubraConfig, Locale locale) {
        this.appConfig = appConfig;
        this.akubraConfig = akubraConfig;
        this.locale = locale;
    }

    private static boolean isSupportedModel(String model) {
        return NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(model)
                || NdkPlugin.MODEL_PERIODICALISSUE.equals(model)
                || NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT.equals(model)
                || NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(model)
                || NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(model)
                || NdkPlugin.MODEL_MONOGRAPHUNIT.equals(model)
                || NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT.equals(model)
                || NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME.equals(model)
                || NdkEbornPlugin.MODEL_EMONOGRAPHUNIT.equals(model)
                || NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT.equals(model);
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

    public void createListOfPids(List<String> pids) throws IOException, DigitalObjectException {
        SearchView search = null;
        if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
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
                if (isSupportedModel(item.getModel())) {
                    updatedPids.add(item.getPid());
                } else {
                    throw new DigitalObjectException(item.getPid(), "Unsupported model \"" + item.getModel() + "\" for multi update objets.");
                }
            }
        }
    }

    public void updateObjects(String signatura, String sigla, String title, String subTitle, String partName, String note, String publisher, String place) throws DigitalObjectException {
        this.signaturaValue = signatura;
        this.siglaValue = sigla;
        this.titleValue = title;
        this.subTitleValue = subTitle;
        this.partNameValue = partName;
        this.noteValue = note;
        this.publisherValue = publisher;
        this.placeValue = place;

        if (updatedPids != null && !updatedPids.isEmpty()) {
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
        if (!isSupportedModel(model)) {
            throw new DigitalObjectException("Unsupported model: " + model);
        }

        setLocation(mods);
        setTitleInfo(mods);
        setOriginInfo(mods);
        setNote(mods);
    }

    private void setOriginInfo(ModsDefinition mods) {
        if (this.dateIssuedValue == null && this.publisherValue == null && this.placeValue == null) {
            return;
        }
        if (mods.getOriginInfo().isEmpty()) {
            mods.getOriginInfo().add(new OriginInfoDefinition());
        }
        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            if (this.dateIssuedValue != null) {
                if (originInfo.getDateIssued().isEmpty()) {
                    originInfo.getDateIssued().add(new DateDefinition());
                }
                for (DateDefinition dateIssued : originInfo.getDateIssued()) {
                    dateIssued.setValue(this.dateIssuedValue);
                }
            }
            if (this.publisherValue != null) {
                if (originInfo.getPublisher().isEmpty()) {
                    originInfo.getPublisher().add(new StringPlusLanguagePlusSupplied());
                }
                for (StringPlusLanguagePlusSupplied publisher : originInfo.getPublisher()) {
                    publisher.setValue(this.publisherValue);
                }
            }
            if (this.placeValue != null) {
                if (originInfo.getPlace().isEmpty()) {
                    originInfo.getPlace().add(new PlaceDefinition());
                }
                for (PlaceDefinition place : originInfo.getPlace()) {
                    if (place.getPlaceTerm().isEmpty()) {
                        place.getPlaceTerm().add(new PlaceTermDefinition());
                    }
                    for (PlaceTermDefinition placeTerm : place.getPlaceTerm()) {
                        placeTerm.setValue(this.placeValue);
                    }
                }
            }
        }
    }

    private void setNote(ModsDefinition mods) {
        if (this.noteValue == null) {
            return;
        }
        if (mods.getNote().isEmpty()) {
            mods.getNote().add(new NoteDefinition());
        }
        for (NoteDefinition note : mods.getNote()) {
            note.setValue(this.noteValue);
        }
    }

    private void setTitleInfo(ModsDefinition mods) {
        if (this.titleValue == null && this.subTitleValue == null && this.partNameValue == null
                && this.partNumberValue == null) {
            return;
        }
        if (mods.getTitleInfo().isEmpty()) {
            mods.getTitleInfo().add(new TitleInfoDefinition());
        }
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            if (this.titleValue != null) {
                if (titleInfo.getTitle().isEmpty()) {
                    titleInfo.getTitle().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage title : titleInfo.getTitle()) {
                    title.setValue(this.titleValue);
                }
            }
            if (this.subTitleValue != null) {
                if (titleInfo.getSubTitle().isEmpty()) {
                    titleInfo.getSubTitle().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage subTitle : titleInfo.getSubTitle()) {
                    subTitle.setValue(this.subTitleValue);
                }
            }
            if (this.partNameValue != null) {
                if (titleInfo.getPartName().isEmpty()) {
                    titleInfo.getPartName().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage partName : titleInfo.getPartName()) {
                    partName.setValue(this.partNameValue);
                }
            }
            if (this.partNumberValue != null) {
                if (titleInfo.getPartNumber().isEmpty()) {
                    titleInfo.getPartNumber().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage partNumber : titleInfo.getPartNumber()) {
                    if (this.partNumberValue == -1) {
                        partNumber.setValue(null);
                    } else if (this.partNumberValue > -1) {
                        partNumber.setValue(String.valueOf(this.partNumberValue));
                        this.partNumberValue++;
                    }
                }
            }
        }
    }

    private void setLocation(ModsDefinition mods) {
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

    public void createDateIssued(String dateIssued) throws DigitalObjectException {
        if (dateIssued != null && !dateIssued.isEmpty()) {
            if (!DatumValidator.isValid(dateIssued)) {
                String pid = updatedPids == null || updatedPids.isEmpty() ? null : updatedPids.get(0);
                DigitalObjectValidationException ex = new DigitalObjectValidationException(pid, null,
                        ModsStreamEditor.DATASTREAM_ID, "MODS validation", null);
                ex.addValidation("MODS rules", ERR_NDK_ORIGININFO_DATEISSSUED, false, dateIssued);
                throw ex;
            }
            this.dateIssuedValue = dateIssued;
        } else if (dateIssued != null && dateIssued.isEmpty()) {
            this.dateIssuedValue = null;
        }
    }
}
