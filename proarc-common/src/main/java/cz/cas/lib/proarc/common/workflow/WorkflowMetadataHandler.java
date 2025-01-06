/*
 * Copyright (C) 2025 Jan Pokorsky
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
package cz.cas.lib.proarc.common.workflow;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.RelatedItemDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import java.math.BigDecimal;
import java.util.Locale;

import static cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.copyIdentifier;
import static cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.inheritIdentifier;
import static cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.inheritLocation;
import static cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.inheritPhysicalDescriptionForm;
import static cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.inheritRecordInfo;
import static cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.inheritSupplementTitleInfo;
import static cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.inheritTitleInfo;
import static cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.modsCopyMusicDocument;

public class WorkflowMetadataHandler {

    private final String modelId;
    private final String pid;
    private final Job parentJob;

    public WorkflowMetadataHandler(String modelId, String pid, Job parentJob) {
        this.modelId = modelId;
        this.pid = pid;
        this.parentJob = parentJob;
    }

    protected static ModsDefinition setRules(ModsDefinition mods) {
        try {
            AppConfiguration config = AppConfigurationFactory.getInstance().defaultInstance();
            StringPlusLanguagePlusAuthority descriptionStandard = new StringPlusLanguagePlusAuthority();
            String rules = config.getRules();
            descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR.equalsIgnoreCase(rules)? ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR : ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA);
            RecordInfoDefinition recordInfo = new RecordInfoDefinition();
            recordInfo.getDescriptionStandard().add(0, descriptionStandard);
            mods.getRecordInfo().add(0, recordInfo);
        } catch (AppConfigurationException ex) {
            ex.printStackTrace();
        }
        return mods;
    }

    public ModsDefinition createDefaultMods() throws DigitalObjectException {
        ModsDefinition defaultMods = ModsStreamEditor.defaultMods(pid);

        if (!(NdkPlugin.MODEL_NDK_PAGE.equals(modelId) || NdkPlugin.MODEL_PAGE.equals(modelId) || OldPrintPlugin.MODEL_PAGE.equals(modelId) || NdkAudioPlugin.MODEL_PAGE.equals(modelId))) {
            setRules(defaultMods);
        }
        if (NdkPlugin.MODEL_ARTICLE.equals(modelId)) {
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_PERIODICALISSUE);
            if (titleMods != null) {
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        }
        if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(modelId)) {
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_PERIODICAL);
            if (titleMods != null) {
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        }
        if (NdkPlugin.MODEL_PERIODICALISSUE.equals(modelId)) {
            // issue 124
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_PERIODICAL);
            if (titleMods != null) {
                inheritTitleInfo(defaultMods, titleMods.getTitleInfo());

                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
                inheritLocation(defaultMods, titleMods.getLocation());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "issn");
            }
        } else if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            // issue 137
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_PERIODICAL);
            if (titleMods != null) {
                inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "issn");
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT.equals(modelId)) {
            // issue 240
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHVOLUME);
            if (titleMods != null) {
                inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
//                inheritOriginInfoDateIssued(defaultMods, titleMods.getOriginInfo());
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHUNIT);
            if (titleMods != null) {
                inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
//                inheritOriginInfoDateIssued(defaultMods, titleMods.getOriginInfo());
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(NdkAudioPlugin.MODEL_MUSICDOCUMENT);
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
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHVOLUME);
            if (titleMods != null) {
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHUNIT);
            if (titleMods != null) {
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
//        } else if (NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId)) {
        } else if (NdkPlugin.MODEL_PICTURE.equals(modelId)) {
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHVOLUME);
            if (titleMods != null) {
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHUNIT);
            if (titleMods != null) {
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (NdkPlugin.MODEL_MONOGRAPHUNIT.equals(modelId)) {
            //issue 540
            ModsDefinition titleMods = findEnclosingObject(NdkPlugin.MODEL_MONOGRAPHTITLE);
            if (titleMods != null) {
                defaultMods.getTitleInfo().addAll(titleMods.getTitleInfo());
                defaultMods.getOriginInfo().addAll(titleMods.getOriginInfo());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(modelId)) {
            // issue 124
            ModsDefinition titleMods = findEnclosingObject(NdkEbornPlugin.MODEL_EPERIODICAL);
            if (titleMods != null) {
                inheritTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritLocation(defaultMods, titleMods.getLocation());
                //inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "issn");
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (NdkEbornPlugin.MODEL_EARTICLE.equals(modelId)) {
            // issue 859
            RelatedItemDefinition relatedItem = new RelatedItemDefinition();
            defaultMods.getRelatedItem().add(relatedItem);
            ModsDefinition titleMods = findEnclosingObject(NdkEbornPlugin.MODEL_EPERIODICAL);
            if (titleMods != null) {
                if (titleMods.getTitleInfo().size() != 0) {
                    relatedItem.getTitleInfo().add(titleMods.getTitleInfo().get(0));
                }
                relatedItem.getName().addAll(titleMods.getName());
                copyIdentifier(relatedItem, titleMods, "issn");
            }
            titleMods = findEnclosingObject(NdkEbornPlugin.MODEL_EPERIODICALISSUE);
            if (titleMods != null) {
                if (relatedItem.getTitleInfo().size() != 0
                        && titleMods.getTitleInfo().size() != 0
                        && titleMods.getTitleInfo().get(0).getPartNumber().size() != 0) {
                    relatedItem.getTitleInfo().get(0).getPartNumber().add(titleMods.getTitleInfo().get(0).getPartNumber().get(0));
                }
                copyIdentifier(relatedItem, titleMods, "uuid");
            }
        } else if (NdkAudioPlugin.MODEL_SONG.equals(modelId)) {
            ModsDefinition titleMods = findEnclosingObject(NdkAudioPlugin.MODEL_MUSICDOCUMENT);
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
                    ModsDefinition titleMods = findEnclosingObject(NdkAudioPlugin.MODEL_MUSICDOCUMENT);
                    modsCopyMusicDocument(titleMods, defaultMods);
                    inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
                    inheritIdentifier(defaultMods, titleMods.getIdentifier(), "issue number", "matrix number");
                } else if (NdkAudioPlugin.MODEL_PHONOGRAPH.equals(parentJob.getModel())) {
                    ModsDefinition titleMods = findEnclosingObject(NdkAudioPlugin.MODEL_PHONOGRAPH);
                    modsCopyMusicDocument(titleMods, defaultMods);
                    inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
                    inheritIdentifier(defaultMods, titleMods.getIdentifier(), "issue number", "matrix number");
                } else if (NdkAudioPlugin.MODEL_SONG.equals(parentJob.getModel())) {
                    ModsDefinition titleMods = findEnclosingObject(NdkAudioPlugin.MODEL_SONG);
                    modsCopyMusicDocument(titleMods, defaultMods);
                    inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
                }
            }
        } else if (OldPrintPlugin.MODEL_SUPPLEMENT.equals(modelId)) {
            // issue 329
            ModsDefinition titleMods = findEnclosingObject(OldPrintPlugin.MODEL_MONOGRAPHVOLUME);
            if (titleMods != null) {
                inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
//                        inheritOriginInfoDateIssued(defaultMods, titleMods.getOriginInfo());
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(OldPrintPlugin.MODEL_MONOGRAPHUNIT);
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
            ModsDefinition titleMods = findEnclosingObject(OldPrintPlugin.MODEL_MONOGRAPHTITLE);
            if (titleMods != null) {
                defaultMods.getTitleInfo().addAll(titleMods.getTitleInfo());
                defaultMods.getOriginInfo().addAll(titleMods.getOriginInfo());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        } else if (OldPrintPlugin.MODEL_CHAPTER.equals(modelId)) {
            // issue 241
            ModsDefinition titleMods = findEnclosingObject(OldPrintPlugin.MODEL_MONOGRAPHVOLUME);
            if (titleMods != null) {
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
            titleMods = findEnclosingObject(OldPrintPlugin.MODEL_MONOGRAPHUNIT);
            if (titleMods != null) {
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
                inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                inheritRecordInfo(defaultMods, titleMods.getRecordInfo());
            }
        }

        return defaultMods;
    }

    private ModsDefinition findEnclosingObject(String searchModelId) throws DigitalObjectException {
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
}
