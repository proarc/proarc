/*
 * Copyright (C) 2014 Lukas Sykora
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
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
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

    public static final Set<String> HAS_MEMBER_RDA_VALIDATION_MODELS = Collections.unmodifiableSet(new HashSet<String>(
            Arrays.asList(NdkPlugin.MODEL_CARTOGRAPHIC, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, NdkPlugin.MODEL_MONOGRAPHVOLUME,
                    NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALSUPPLEMENT, NdkPlugin.MODEL_SHEETMUSIC)));

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

    public void check() throws DigitalObjectValidationException{
        if (HAS_MEMBER_RDA_VALIDATION_MODELS.contains(modelId)) {
            checkRules(mods);
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
        if (oi.getEventType() == null || oi.getEventType().equals(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION)) {
            checkDateNull(oi.getCopyrightDate(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_COPYRIGHT, false);
            checkDateNull(oi.getDateOther(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_OTHER, false);
            checkDateEmpty(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, true);
            checkDateNull(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, true);
        } else if (oi.getEventType().equals(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION)
                || oi.getEventType().equals(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION)
                || oi.getEventType().equals(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE)) {
            checkDateNull(oi.getCopyrightDate(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_COPYRIGHT, false);
            checkDateEmpty(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, false);
            checkDateNull(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, false);
        } else if (oi.getEventType().equals(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT)) {
            checkDateEmpty(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, false);
            checkDateNull(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, false);
            checkDateNull(oi.getDateOther(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_OTHER, false);
        } else {
            exception.addValidation("RDA rules", ERR_NDK_ORIGININFO_EVENTTYPE_WRONGVALUE, oi.getEventType());
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
    protected void checkRules(ModsDefinition mods) {
        if (mods.getRecordInfo().isEmpty()) {
            return;
        }
        String descriptionStandard = mods.getRecordInfo().get(0).getDescriptionStandard().get(0).getValue();
        if (descriptionStandard == null) {
            exception.addValidation("RDA rules", ERR_NDK_DESCRIPTIONSTANDARD);
        } else if (!descriptionStandard.equalsIgnoreCase(ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA)
                && !descriptionStandard.equalsIgnoreCase(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR)) {
            exception.addValidation("RDA rules", ERR_NDK_DESCRIPTIONSTANDARD);
        }
        List<OriginInfoDefinition> originInfoDefinitions = mods.getOriginInfo();
        List<PhysicalDescriptionDefinition> physicalDescriptions = mods.getPhysicalDescription();
        if (descriptionStandard.equalsIgnoreCase("aacr")) {
            for (OriginInfoDefinition oi : originInfoDefinitions) {
                if (oi.getEventType() != null) {
                    exception.addValidation("RDA rules", ERR_NDK_AACR_EMPTYVALUE);
                }
            }
            for (PhysicalDescriptionDefinition pd : physicalDescriptions) {
                if (!pd.getForm().isEmpty() && (pd.getForm().get(0).getAuthority().equals("rdamedia") || pd.getForm().get(0).getAuthority().equals("rdacarrier"))) {
                    exception.addValidation("RDA rules", ERR_NDK_AACR_INVALIDVALUE);
                }
            }
        }
    }
}
