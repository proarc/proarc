/*
 * Copyright (C) 2017 Lukas Sykora
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
package cz.cas.lib.proarc.common.object.ndk;

import cz.cas.lib.proarc.common.fedora.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;

import org.apache.commons.configuration.Configuration;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Checks RDA rules.
 *
 * @author Lukas Sykora
 */
public class RdaRules {

    String modelId;
    ModsDefinition mods;
    DigitalObjectValidationException exception;
    static final String PROP_MODS_RULES = "metadata.mods.rules";
    private String rdaRules;


    public static final Set<String> HAS_MEMBER_RDA_VALIDATION_MODELS = Collections.unmodifiableSet(new HashSet<>(
            Arrays.asList(NdkPlugin.MODEL_CARTOGRAPHIC, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, NdkPlugin.MODEL_MONOGRAPHVOLUME,
                    NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALSUPPLEMENT, NdkPlugin.MODEL_SHEETMUSIC,
                    OldPrintPlugin.MODEL_VOLUME)));

    public static final String ERR_NDK_RDA_EMPTYVALUE = "Err_Ndk_Rda_EmptyValue";
    public static final String ERR_NDK_RDA_FILLVALUE = "Err_Ndk_Rda_FillValue";
    public static final String ERR_NDK_DESCRIPTIONSTANDARD = "Err_Ndk_DescriptionStandard";
    public static final String ERR_NDK_AACR_EMPTYVALUE = "Err_Ndk_Aacr_EmptyValue";
    public static final String ERR_NDK_AACR_INVALIDVALUE = "Err_Ndk_Aacr_InvalidValue";
    public static final String ERR_NDK_ORIGININFO_EVENTTYPE_WRONGVALUE ="Err_Ndk_OriginInfo_EventType_WrongValue";

    public RdaRules(String modelId, ModsDefinition mods, DigitalObjectValidationException ex) {
        this.modelId = modelId;
        this.mods = mods;
        this.exception = ex;
    }

    public RdaRules() {}

    public void check() throws DigitalObjectValidationException{
        if (HAS_MEMBER_RDA_VALIDATION_MODELS.contains(modelId)) {
            checkAndRepairRules(mods);
            for (OriginInfoDefinition oi : mods.getOriginInfo()) {
                checkOriginInfoRdaRules(oi);
            }
        }
        if (!exception.getValidations().isEmpty()){
            throw exception;
        }
    }

    /** Checks if the correct fields are filled depending on eventType */
    protected void checkOriginInfoRdaRules(OriginInfoDefinition oi) {
        String eventType = oi.getEventType();
        if (eventType == null || ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION.equals(eventType)) {
            checkDateNull(oi.getCopyrightDate(), eventType, ModsConstants.FIELD_ORIGININFO_DATE_COPYRIGHT, false);
            checkDateNull(oi.getDateOther(), eventType, ModsConstants.FIELD_ORIGININFO_DATE_OTHER, false);
            checkDateEmpty(oi.getDateIssued(), eventType, ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, true);
            checkDateNull(oi.getDateIssued(), eventType, ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, true);
        } else if (ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION.equals(eventType)
                || ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION.equals(eventType)
                || ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE.equals(eventType)) {
            checkDateNull(oi.getCopyrightDate(), eventType, ModsConstants.FIELD_ORIGININFO_DATE_COPYRIGHT, false);
            checkDateEmpty(oi.getDateIssued(), eventType, ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, false);
            checkDateNull(oi.getDateIssued(), eventType, ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, false);
        } else if (ModsConstants.VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT.equals(eventType)) {
            checkDateEmpty(oi.getDateIssued(), eventType, ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, false);
            checkDateNull(oi.getDateIssued(), eventType, ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, false);
            checkDateNull(oi.getDateOther(), eventType, ModsConstants.FIELD_ORIGININFO_DATE_OTHER, false);
        } else {
            exception.addValidation("RDA rules", ERR_NDK_ORIGININFO_EVENTTYPE_WRONGVALUE, eventType);
        }
    }

    /** Checks if elements in List is null */
    private void checkDateNull(List dates, String event, String element, boolean mustBeFill) {
        for (Object date : dates) {
            Object dateValue = ((DateDefinition) date).getValue();
            if (mustBeFill && dateValue == null) {
                exception.addValidation("RDA rules", ERR_NDK_RDA_FILLVALUE);
            } else if (!mustBeFill && dateValue != null) {
                exception.addValidation("RDA rules", ERR_NDK_RDA_EMPTYVALUE, element, event);
            }
        }
    }

    /** Checks if the list is empty */
    private void checkDateEmpty(List dates, String event, String element, boolean mustBeFill) {
        if (mustBeFill && dates.isEmpty()) {
            exception.addValidation("RDA rules", ERR_NDK_RDA_FILLVALUE, element, event);
        } else if (!mustBeFill && !dates.isEmpty()) {
            exception.addValidation("RDA rules", ERR_NDK_RDA_EMPTYVALUE, element, event);
        }
    }

    /** Checks if the correct fields are filled depending on eventType */
    protected void checkAndRepairRules(ModsDefinition mods) {
        if (mods.getRecordInfo().isEmpty()) {
            return;
        }
        List<StringPlusLanguagePlusAuthority> descriptionStandards = mods.getRecordInfo().get(0).getDescriptionStandard();
        String descriptionStandard = descriptionStandards.isEmpty() ? null : descriptionStandards.get(0).getValue();
        if (descriptionStandard == null) {
            StringPlusLanguagePlusAuthority description = new StringPlusLanguagePlusAuthority();
            description.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR);
            if (mods.getRecordInfo().get(0) == null) {
                RecordInfoDefinition recordInfoDefinition = new RecordInfoDefinition();
                recordInfoDefinition.getDescriptionStandard().add(description);
                mods.getRecordInfo().add(recordInfoDefinition);
            } else {
                mods.getRecordInfo().get(0).getDescriptionStandard().add(description);
            }
        } else if (!ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA.equalsIgnoreCase(descriptionStandard)
                && !ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR.equalsIgnoreCase(descriptionStandard)) {
            exception.addValidation("RDA rules", ERR_NDK_DESCRIPTIONSTANDARD);
        }
        List<OriginInfoDefinition> originInfoDefinitions = mods.getOriginInfo();
        List<PhysicalDescriptionDefinition> physicalDescriptions = mods.getPhysicalDescription();
        if (ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR.equalsIgnoreCase(descriptionStandard)) {
            for (OriginInfoDefinition oi : originInfoDefinitions) {
                if (oi.getEventType() != null) {
                    exception.addValidation("RDA rules", ERR_NDK_AACR_EMPTYVALUE);
                }
            }
            for (PhysicalDescriptionDefinition pd : physicalDescriptions) {
                String authority = pd.getForm().isEmpty() ? null : pd.getForm().get(0).getAuthority();
                if ("rdamedia".equals(authority) || "rdacarrier".equals(authority)) {
                    exception.addValidation("RDA rules", ERR_NDK_AACR_INVALIDVALUE);
                }
            }
        }
    }

    public static RdaRules getOptions(Configuration config) {
        RdaRules options = new RdaRules();

        String modsRules = config.getString(PROP_MODS_RULES);
        if (modsRules != null && !modsRules.isEmpty()) {
            options.setModsRules(modsRules);
        }
        return options;
    }

    public void setModsRules(String modsRules) {
        this.rdaRules = modsRules;
    }

    public String getRules() {
        return rdaRules;
    }
}
