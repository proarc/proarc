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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.RelatedItemDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import java.math.BigDecimal;
import java.util.List;
import java.util.Locale;

public class ModsDataHandler {

    private AppConfiguration appConfiguration;

    public ModsDataHandler(AppConfiguration appConfiguration) {
        this.appConfiguration = appConfiguration;
    }

    public ModsDefinition createDefaultMetadata(String pid, String modelId, DigitalObjectHandler objectHandler, Job parentJob) throws DigitalObjectException {
        ModsDefinition defaultMods = ModsStreamEditor.defaultMods(pid);

        if (!(NdkPlugin.MODEL_NDK_PAGE.equals(modelId) || NdkPlugin.MODEL_PAGE.equals(modelId) || OldPrintPlugin.MODEL_PAGE.equals(modelId) || NdkAudioPlugin.MODEL_PAGE.equals(modelId))) {
            setRules(defaultMods);
        }
        if (NdkPlugin.MODEL_ARTICLE.equals(modelId)) {
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_PERIODICALISSUE, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        }
        if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(modelId) || NdkEbornPlugin.MODEL_EPERIODICALVOLUME.equals(modelId)) {
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_PERIODICAL, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(NdkEbornPlugin.MODEL_EPERIODICAL, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        }
        if (NdkPlugin.MODEL_PERIODICALISSUE.equals(modelId) || NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(modelId)) {
            // issue 124
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_PERIODICAL, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritTitleInfo(defaultMods, titleMods.getTitleInfo());

                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
                inheritLocation(defaultMods, titleMods.getLocation());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "issn");
            }
            titleMods = findEnclosingObject(NdkEbornPlugin.MODEL_EPERIODICAL, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritTitleInfo(defaultMods, titleMods.getTitleInfo());

                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
                inheritLocation(defaultMods, titleMods.getLocation());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "issn");
            }
            if (objectHandler != null) {
                String partNumberVal = objectHandler.getParameter(DigitalObjectHandler.PARAM_PART_NUMBER);
                String dateIssuedVal = objectHandler.getParameter(DigitalObjectHandler.PARAM_ISSUE_DATE);
                String dateIssuedEndOfRangeVal = objectHandler.getParameter(DigitalObjectHandler.PARAM_ISSUE_DATE_END_OF_RANGE);
                fillIssueSeries(defaultMods, partNumberVal, dateIssuedVal, dateIssuedEndOfRangeVal);

                String signaturaVal = objectHandler.getParameter(DigitalObjectHandler.PARAM_SIGNATURA);
                replaceShelfLocator(defaultMods, signaturaVal);
            }
        } else if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            // issue 137
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_PERIODICAL, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "issn");
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT.equals(modelId)) {
            // issue 240
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHVOLUME, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
//                inheritOriginInfoDateIssued(defaultMods, titleMods.getOriginInfo());
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHUNIT, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
//                inheritOriginInfoDateIssued(defaultMods, titleMods.getOriginInfo());
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(NdkAudioPlugin.MODEL_MUSICDOCUMENT, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                defaultMods.getTitleInfo().addAll(titleMods.getTitleInfo());
                defaultMods.getName().addAll(titleMods.getName());
                defaultMods.getOriginInfo().addAll(titleMods.getOriginInfo());
                defaultMods.getPhysicalDescription().addAll(titleMods.getPhysicalDescription());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                defaultMods.getTableOfContents().addAll(titleMods.getTableOfContents());
                defaultMods.getNote().addAll(titleMods.getNote());
                defaultMods.getSubject().addAll(titleMods.getSubject());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (NdkPlugin.MODEL_CHAPTER.equals(modelId)) {
            // issue 241
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHVOLUME, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHUNIT, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
//        } else if (NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId)) {
        } else if (NdkPlugin.MODEL_PICTURE.equals(modelId)) {
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHVOLUME, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHUNIT, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (NdkPlugin.MODEL_MONOGRAPHUNIT.equals(modelId)) {
            //issue 540
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHTITLE, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                defaultMods.getTitleInfo().addAll(titleMods.getTitleInfo());
                defaultMods.getName().addAll(titleMods.getName());
                defaultMods.getOriginInfo().addAll(titleMods.getOriginInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                defaultMods.getPhysicalDescription().addAll(titleMods.getPhysicalDescription());
//                defaultMods.getAbstract().addAll(titleMods.getAbstract());
                defaultMods.getNote().addAll(titleMods.getNote());
                defaultMods.getSubject().addAll(titleMods.getSubject());
                defaultMods.getClassification().addAll(titleMods.getClassification());
                inheritIdentifierExclude(defaultMods, titleMods.getIdentifier(), "uuid");
                defaultMods.getLocation().addAll(titleMods.getLocation());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (NdkEbornPlugin.MODEL_EARTICLE.equals(modelId)) {
            // issue 859
            RelatedItemDefinition relatedItem = new RelatedItemDefinition();
            defaultMods.getRelatedItem().add(relatedItem);
            ModsDefinition titleMods = findEnclosingObject(NdkEbornPlugin.MODEL_EPERIODICAL, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                if (titleMods.getTitleInfo().size() != 0) {
                    relatedItem.getTitleInfo().add(titleMods.getTitleInfo().get(0));
                }
                relatedItem.getName().addAll(titleMods.getName());
                copyIdentifier(relatedItem, titleMods, "issn");
            }
            titleMods = findEnclosingObject(NdkEbornPlugin.MODEL_EPERIODICALISSUE, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                if (relatedItem.getTitleInfo().size() != 0
                        && titleMods.getTitleInfo().size() != 0
                        && titleMods.getTitleInfo().get(0).getPartNumber().size() != 0) {
                    relatedItem.getTitleInfo().get(0).getPartNumber().add(titleMods.getTitleInfo().get(0).getPartNumber().get(0));
                }
                copyIdentifier(relatedItem, titleMods, "uuid");
            }
        } else if (NdkAudioPlugin.MODEL_SONG.equals(modelId)) {
            ModsDefinition titleMods = findEnclosingObject(NdkAudioPlugin.MODEL_MUSICDOCUMENT, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                defaultMods.getTitleInfo().addAll(titleMods.getTitleInfo());
                defaultMods.getName().addAll(titleMods.getName());
                defaultMods.getOriginInfo().addAll(titleMods.getOriginInfo());
                defaultMods.getPhysicalDescription().addAll(titleMods.getPhysicalDescription());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                defaultMods.getTableOfContents().addAll(titleMods.getTableOfContents());
                defaultMods.getNote().addAll(titleMods.getNote());
                defaultMods.getSubject().addAll(titleMods.getSubject());
                inheritIdentifier(defaultMods, titleMods.getIdentifier(), "issue number", "matrix number");
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (NdkAudioPlugin.MODEL_TRACK.equals(modelId)) {
            if (parentJob != null) {
                if (NdkAudioPlugin.MODEL_MUSICDOCUMENT.equals(parentJob.getModel())) {
                    ModsDefinition titleMods = findEnclosingObject(NdkAudioPlugin.MODEL_MUSICDOCUMENT, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
                    modsCopyMusicDocument(titleMods, defaultMods);
                    inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
                    inheritIdentifier(defaultMods, titleMods.getIdentifier(), "issue number", "matrix number");
                } else if (NdkAudioPlugin.MODEL_PHONOGRAPH.equals(parentJob.getModel())) {
                    ModsDefinition titleMods = findEnclosingObject(NdkAudioPlugin.MODEL_PHONOGRAPH, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
                    modsCopyMusicDocument(titleMods, defaultMods);
                    inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
                    inheritIdentifier(defaultMods, titleMods.getIdentifier(), "issue number", "matrix number");
                } else if (NdkAudioPlugin.MODEL_SONG.equals(parentJob.getModel())) {
                    ModsDefinition titleMods = findEnclosingObject(NdkAudioPlugin.MODEL_SONG, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
                    modsCopyMusicDocument(titleMods, defaultMods);
                    inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
                }
            }
        } if (OldPrintPlugin.MODEL_SUPPLEMENT.equals(modelId)) {
            // issue 329
            ModsDefinition titleMods = findEnclosingObject(OldPrintPlugin.MODEL_MONOGRAPHVOLUME, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
//                        inheritOriginInfoDateIssued(defaultMods, titleMods.getOriginInfo());
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(OldPrintPlugin.MODEL_MONOGRAPHUNIT, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
//                        inheritOriginInfoDateIssued(defaultMods, titleMods.getOriginInfo());
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (OldPrintPlugin.MODEL_MONOGRAPHUNIT.equals(modelId)) {
            //issue 540
            ModsDefinition titleMods = findEnclosingObject(OldPrintPlugin.MODEL_MONOGRAPHTITLE, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                defaultMods.getTitleInfo().addAll(titleMods.getTitleInfo());
                defaultMods.getOriginInfo().addAll(titleMods.getOriginInfo());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (OldPrintPlugin.MODEL_CHAPTER.equals(modelId)) {
            // issue 241
            ModsDefinition titleMods = findEnclosingObject(OldPrintPlugin.MODEL_MONOGRAPHVOLUME, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(OldPrintPlugin.MODEL_MONOGRAPHUNIT, objectHandler != null ? objectHandler.getParameterParent() : null, parentJob);
            if (titleMods != null) {
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        }

        return defaultMods;
    }

    private void setRules(ModsDefinition mods) {
        StringPlusLanguagePlusAuthority descriptionStandard = new StringPlusLanguagePlusAuthority();
        String rules = appConfiguration.getRules();
        descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR.equalsIgnoreCase(rules)? ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR : ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA);
        RecordInfoDefinition recordInfo = new RecordInfoDefinition();
        recordInfo.getDescriptionStandard().add(0, descriptionStandard);
        mods.getRecordInfo().add(0, recordInfo);
    }

    private ModsDefinition findEnclosingObject(String searchModelId, DigitalObjectHandler parentHandler, Job parentJob) throws DigitalObjectException {
        if (parentHandler != null) {
            return findModsOfEnclosingObjectOfHandler(searchModelId, parentHandler);
        } else if (parentJob != null) {
            return findModsOfEnclosingObjectOfJob(searchModelId, parentJob);
        }
        return null;
    }

    private ModsDefinition findModsOfEnclosingObjectOfJob(String searchModelId, Job parentJob) throws DigitalObjectException {
        if (parentJob == null) {
            return null;
        }
        ProArcObject proArcObject = findEnclosingObject(parentJob, searchModelId);
        if (proArcObject == null) {
            return null;
        }
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        DigitalObjectHandler handler = dom.createHandler(proArcObject);
        MetadataHandler<ModsDefinition> metadataHandler = handler.<ModsDefinition>metadata();
        ModsDefinition mods = metadataHandler.getMetadata().getData();
        return mods;
    }

    private ProArcObject findEnclosingObject(Job job, String searchModelId) throws DigitalObjectNotFoundException {
        if (job == null) {
            return null;
        }
        if (searchModelId == null) {
            return null;
        }
        if (searchModelId.equals(job.getModel())) {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            return dom.find(job.getId(), job.getModel(), new Locale("cs"));
        } else {
            BigDecimal parentJobId = job.getParentId();
            if (parentJobId == null) {
                return null;
            } else {
                WorkflowManager workflowManager = WorkflowManager.getInstance();
                Job parentJob = workflowManager.getJob(parentJobId);
                return findEnclosingObject(parentJob, searchModelId);
            }
        }
    }

    private ModsDefinition findModsOfEnclosingObjectOfHandler(String searchModelId, DigitalObjectHandler parentHandler) throws DigitalObjectException {
        if (parentHandler != null) {
            DigitalObjectHandler handler = findEnclosingObject(parentHandler, searchModelId);
            if (handler != null) {
                return handler.<ModsDefinition>metadata().getMetadata().getData();
            }
        }
        return null;
    }

    protected final DigitalObjectHandler findEnclosingObject(
            DigitalObjectHandler parentHandler, String searchModelId) throws DigitalObjectException {

        if (parentHandler != null) {
            if (searchModelId.equals(parentHandler.relations().getModel())) {
                return parentHandler;
            } else {
                DigitalObjectElement parent = getCrawler().getParent(parentHandler.getFedoraObject().getPid());
                return findEnclosingObject(parent, searchModelId);
            }
        }
        return null;
    }

    private DigitalObjectHandler findEnclosingObject(
            DigitalObjectElement objectElement, String searchModelId) throws DigitalObjectException {

        if (objectElement == DigitalObjectElement.NULL) {
            return null;
        } else if (searchModelId.equals(objectElement.getModelId())) {
            return objectElement.getHandler();
        } else {
            DigitalObjectElement parent = getCrawler().getParent(objectElement.getPid());
            return findEnclosingObject(parent, searchModelId);
        }
    }

    public DigitalObjectCrawler getCrawler() {
        try {
            if (Storage.FEDORA.equals(appConfiguration.getTypeOfStorage())) {
                return new DigitalObjectCrawler(DigitalObjectManager.getDefault(), FedoraStorage.getInstance().getSearch());
            } else if (Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
                AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfiguration.getConfigHome());
                return new DigitalObjectCrawler(DigitalObjectManager.getDefault(), AkubraStorage.getInstance(akubraConfiguration).getSearch());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfiguration.getTypeOfStorage());
            }
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
    }

    private void replaceShelfLocator(ModsDefinition defaultMods, String signatura) {
        if (signatura != null && !signatura.isEmpty()) {
            if (defaultMods.getLocation().isEmpty()) {
                defaultMods.getLocation().add(new LocationDefinition());
            }
            for (LocationDefinition loc : defaultMods.getLocation()) {
                if (loc.getShelfLocator().isEmpty()) {
                    loc.getShelfLocator().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage shelfLocator : loc.getShelfLocator()) {
                    shelfLocator.setValue(signatura);
                }
            }
        }
    }

    public static void copyIdentifier(RelatedItemDefinition relatedItem, ModsDefinition mods, String key) {
        List<IdentifierDefinition> identifiers = mods.getIdentifier();
        if (key == null) {
            return;
        }
        for (IdentifierDefinition identifier : identifiers) {
            if (key.equals(identifier.getType())) {
                relatedItem.getIdentifier().add(identifier);
            }
        }

    }

    public static void modsCopyMusicDocument(ModsDefinition titleMods, ModsDefinition defaultMods) {
        if (titleMods != null) {
            defaultMods.getTitleInfo().addAll(titleMods.getTitleInfo());
            defaultMods.getName().addAll(titleMods.getName());
            defaultMods.getTypeOfResource().addAll(titleMods.getTypeOfResource());
            defaultMods.getPhysicalDescription().addAll(titleMods.getPhysicalDescription());
        }
    }

    private void fillIssueSeries(ModsDefinition mods, String partNumberVal, String dateIssuedVal, String dateIssuedEndOfRangeVal) {
        if (partNumberVal != null) {
            TitleInfoDefinition titleInfo = mods.getTitleInfo().stream()
                    .filter(ti -> ti.getType() == null).findFirst().orElse(null);
            if (titleInfo == null) {
                mods.getTitleInfo().add(titleInfo = new TitleInfoDefinition());
            }
            StringPlusLanguage partNumber = titleInfo.getPartNumber().stream().findFirst().orElse(null);
            if (partNumber == null) {
                titleInfo.getPartNumber().add(partNumber = new StringPlusLanguage());
            }
            partNumber.setValue(partNumberVal);
        }
        if (dateIssuedVal != null) {
            OriginInfoDefinition originInfo = mods.getOriginInfo().stream().findFirst().orElse(null);
            if (originInfo == null) {
                mods.getOriginInfo().add(originInfo = new OriginInfoDefinition());
            }
            DateDefinition dateIssued = new DateDefinition();
            dateIssued.setValue(dateIssuedVal);
            originInfo.getDateIssued().add(dateIssued);

            if (dateIssuedEndOfRangeVal != null && !dateIssuedEndOfRangeVal.isEmpty()) {
                dateIssued.setPoint("start");
                DateDefinition dateIssuedEndOfRange = new DateDefinition();
                dateIssuedEndOfRange.setValue(dateIssuedEndOfRangeVal);
                dateIssuedEndOfRange.setPoint("end");
                originInfo.getDateIssued().add(dateIssuedEndOfRange);
            }
        }
    }

    public static final void inheritIdentifier(ModsDefinition mods, List<IdentifierDefinition> ids, String... includeIdTypes) {
        for (IdentifierDefinition id : ids) {
            String type = id.getType();
            if (includeIdTypes == null) {
                mods.getIdentifier().add(id);
            } else {
                for (String includeIdType : includeIdTypes) {
                    if (includeIdType.equals(type)) {
                        mods.getIdentifier().add(id);
                    }
                }
            }
        }
    }

    public static final void inheritIdentifierExclude(ModsDefinition mods, List<IdentifierDefinition> ids, String... excludeIdTypes) {
        for (IdentifierDefinition id : ids) {
            String type = id.getType();
            if (excludeIdTypes == null) {
                mods.getIdentifier().add(id);
            } else {
                boolean exclude = false;
                for (String excludeIdType : excludeIdTypes) {
                    if (excludeIdType.equals(type)) {
                        exclude = true;
                    }
                }
                if (!exclude) {
                    mods.getIdentifier().add(id);
                }
            }
        }
    }

    public static void inheritLocation(ModsDefinition mods, List<LocationDefinition> locs) {
        for (LocationDefinition loc : locs) {
            List<PhysicalLocationDefinition> pls = loc.getPhysicalLocation();
            List<StringPlusLanguage> sls = loc.getShelfLocator();
            if (!pls.isEmpty() || !sls.isEmpty()) {
                loc.getUrl().clear();
                mods.getLocation().add(loc);
            }
        }
    }

    public static final void inheritOriginInfoDateIssued(ModsDefinition mods, List<OriginInfoDefinition> ois) {
        for (OriginInfoDefinition oi : ois) {
            OriginInfoDefinition newOi = null;
            for (DateDefinition dateIssued : oi.getDateIssued()) {
                if (newOi == null) {
                    newOi = new OriginInfoDefinition();
                    mods.getOriginInfo().add(newOi);
                }
                newOi.getDateIssued().add(dateIssued);
            }
        }
    }

    public static final void inheritPhysicalDescriptionForm(ModsDefinition mods, List<PhysicalDescriptionDefinition> pds) {
        for (PhysicalDescriptionDefinition pd : pds) {
            PhysicalDescriptionDefinition newPd = null;
            for (FormDefinition form : pd.getForm()) {
                if (newPd == null) {
                    newPd = new PhysicalDescriptionDefinition();
                    mods.getPhysicalDescription().add(newPd);
                }
                newPd.getForm().add(form);
            }
        }
    }

    public static void inheritTitleInfo(ModsDefinition mods, List<TitleInfoDefinition> tis) {
        for (TitleInfoDefinition ti : tis) {
            if (ti.getType() == null) {
                ti.getPartNumber().clear();
                ti.getPartName().clear();
                ti.getNonSort().clear();
                mods.getTitleInfo().add(ti);
            }
        }
    }

    public static void inheritRecordInfo(ModsDefinition mods, List<RecordInfoDefinition> recordInfos) {
        for (int i = 0; i < recordInfos.size(); i++) {
            RecordInfoDefinition recordInfo = recordInfos.get(i);
            if (recordInfo.getDescriptionStandard().size() > 0 && recordInfo.getDescriptionStandard().get(0).getValue() != null) {
                RecordInfoDefinition ri;
                if (mods.getRecordInfo().size() != 0 && mods.getRecordInfo().size() >= i) {
                    ri = mods.getRecordInfo().get(i);
                    if (ri.getDescriptionStandard().size() != 0) {
                        ri.getDescriptionStandard().get(0).setValue(recordInfo.getDescriptionStandard().get(0).getValue());
                    } else {
                        StringPlusLanguagePlusAuthority description = new StringPlusLanguagePlusAuthority();
                        description.setValue(recordInfo.getDescriptionStandard().get(0).getValue());
                        ri.getDescriptionStandard().add(description);
                    }
                } else {
                    ri =new RecordInfoDefinition();
                    mods.getRecordInfo().add(ri);
                    StringPlusLanguagePlusAuthority description = new StringPlusLanguagePlusAuthority();
                    description.setValue(recordInfo.getDescriptionStandard().get(0).getValue());
                    ri.getDescriptionStandard().add(description);
                }
            }
        }
    }

    public static final void inheritSupplementTitleInfo(ModsDefinition mods, List<TitleInfoDefinition> tis) {
        for (TitleInfoDefinition ti : tis) {
            if (ti.getType() == null) {
                ti.getPartNumber().clear();
                ti.getPartName().clear();
                ti.getNonSort().clear();
                ti.getSubTitle().clear();
                mods.getTitleInfo().add(ti);
            }
        }
    }
}
