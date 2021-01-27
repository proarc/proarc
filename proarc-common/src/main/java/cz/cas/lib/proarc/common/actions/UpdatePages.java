/*
 * Copyright (C) 2020 Lukas Sykora
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

import cz.cas.lib.proarc.common.actions.series.SeriesNumber;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapperFactory;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Lukas Sykora
 */
public class UpdatePages {
    private List<String> updatedPids;
    private int index;
    private String pageType;
    private String model;
    private int applyTo;
    private boolean applyToFirstPage;

    public UpdatePages(String applyTo, String applyToFirstPage) throws DigitalObjectException {
        index = -1;
        this.updatedPids = new ArrayList<>();
        setApplyTo(applyTo);
        setApplyToFirstPage(applyToFirstPage);
    }

    private void setApplyToFirstPage(String applyToFirstPage) {
        applyToFirstPage = trim(applyToFirstPage, "{", "}");
        if (applyToFirstPage != null && !applyToFirstPage.isEmpty()) {
            if ("true".equals(applyToFirstPage) || "1".equals(applyToFirstPage.replaceAll("[^0-9]", ""))) {
                this.applyToFirstPage = true;
            } else {
                this.applyToFirstPage = false;
            }
        }
    }

    private String trim(String value, String start, String end) {
        if (value != null && !value.isEmpty()) {
            if (value.startsWith(start)) {
                value = value.substring(start.length());
            }
            if (value.endsWith(end)) {
                value = value.substring(0, value.length()-end.length());
            }
        }
        return value;
    }

    private void setApplyTo(String applyTo) throws DigitalObjectException {
        if (applyTo != null && !applyTo.isEmpty()) {
            try {
                this.applyTo = Integer.parseInt(applyTo.replaceAll("[^0-9]", ""));
            } catch (Exception ex) {
                this.applyTo = 1;
            }
        } else {
            this.applyTo = 1;
        }
    }

    public void createListOfPids(String pids) {
        pids = trim(pids, "{", "}");
        pids = trim(pids, "[", "]");
        String[] selectedPids = pids.split(",");
        if (selectedPids.length > 0) {
            if (applyToFirstPage) {
                for (int i = 0; i < selectedPids.length; i++) {
                    if (i % this.applyTo == 0) {
                        this.updatedPids.add(selectedPids[i]);
                    }
                }
            } else {
                for (int i = 0; i < selectedPids.length; i++) {
                    int value = i - this.applyTo + 1;
                    if (value % this.applyTo == 0) {
                        this.updatedPids.add(selectedPids[i]);
                    }
                }
            }
        }
    }

    public void createIndex(String startIndex) throws DigitalObjectException {
        if (startIndex != null && !startIndex.isEmpty()) {
            try {
                index = Integer.parseInt(startIndex.replaceAll("[^0-9]", ""));
            } catch (Exception ex) {
                throw new DigitalObjectException(null, "Nepodařilo se ziskat hodnotu indexu");
            }
        }
    }

    public void updatePages(String sequenceType, String startNumber, String incrementNumber, String prefix, String suffix, String pageType) throws DigitalObjectException {
        this.pageType = pageType;

        SeriesNumber series = new SeriesNumber(sequenceType, startNumber, incrementNumber, prefix, suffix);

        for (String pid : updatedPids) {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            FedoraObject fo = dom.find(pid, null);
            this.model = new RelationEditor(fo).getModel();
            XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                    MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                    MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
            ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
            ModsDefinition mods = modsStreamEditor.read();
            updateMods(mods, series);
            modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);

            String model = new RelationEditor(fo).getModel();
            DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
            NdkMapperFactory mapperFactory = new NdkMapperFactory();
            NdkMapper mapper = mapperFactory.get(model);
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
    }

    private void updateMods(ModsDefinition mods, SeriesNumber series) throws DigitalObjectException {
        String number = null;
        if (series.isAllowToUpdateNumber()) {
            number = series.getNextValue();
        }
        if (NdkPlugin.MODEL_NDK_PAGE.equals(model)) {
            updateNdkPageMods(mods, number);
        } else if (NdkPlugin.MODEL_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model)) {
            updatePageMods(mods, number);
        } else {
            throw new DigitalObjectException("Unsupported model: " + model);
        }
    }

    private void updatePageMods(ModsDefinition mods, String number) {
        if (mods != null) {
            setPageIndex(mods);
            setPageNumber(mods, number);
            setPageType(mods, false);
        }
    }

    private void updateNdkPageMods(ModsDefinition mods, String number) {
        if (mods != null) {
            setPageIndex(mods);
            setPageNumber(mods, number);
            setPageType(mods, true);
        }
    }

    private void setPageType(ModsDefinition mods, boolean setGenre) {
        if (this.pageType != null && !this.pageType.isEmpty()) {
            for (PartDefinition part : mods.getPart()) {
                part.setType(this.pageType);
            }
            if (setGenre) {
                for (GenreDefinition genre : mods.getGenre()) {
                    genre.setType(this.pageType);
                }
            }
        }
    }

    private void setPageNumber(ModsDefinition mods, String number) {
        boolean updated = false;
        if (number != null && !number.isEmpty()) {
            for (PartDefinition part : mods.getPart()) {
                if (part.getDetail().size() == 0) {
                    DetailDefinition detail = new DetailDefinition();
                    StringPlusLanguage detailNumber = new StringPlusLanguage();
                    detailNumber.setValue(number);
                    detail.getNumber().add(detailNumber);
                    detail.setType("pageNumber");
                    part.getDetail().add(detail);
                } else {
                    for (DetailDefinition detail : part.getDetail()) {
                        if ("pageNumber".equals(detail.getType()) || "page number".equals(detail.getType())) {
                            if (!detail.getNumber().isEmpty()) {
                                detail.getNumber().get(0).setValue(number);
                            }
                        }
                    }
                }
            }
        }
    }

    private void setPageIndex(ModsDefinition mods) {
        if (this.index > 0) {
            for (PartDefinition part : mods.getPart()) {
                for (DetailDefinition detail : part.getDetail()) {
                    if ("pageIndex".equals(detail.getType())) {
                        if (!detail.getNumber().isEmpty()) {
                            detail.getNumber().get(0).setValue(String.valueOf(this.index));
                            part.setType(null);
                            this.index++;
                        }
                    }
                }
            }
        }
    }
}
