/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.mods.ndk;

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.*;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.ModsWrapper;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.List;

/**
 * Subclass to implement transformations of MODS data in NDK flavor
 * to various formats for given digital object model.
 *
 * @author Jan Pokorsky
 */
public abstract class NdkMapper {

    /**
     * Gets a NDK mapper for the given model ID.
     * @param modelId model ID
     * @return the mapper
     * @deprecated Replaced with {@link NdkMapperFactory#get}.
     */
    @Deprecated
    public static NdkMapper get(String modelId) {
        NdkMapper mapper;
        if (NdkPlugin.MODEL_PAGE.equals(modelId)
                || ModsCutomEditorType.EDITOR_PAGE.equals(modelId)) {
            mapper = new NdkPageMapper();
        } else if (NdkPlugin.MODEL_PERIODICAL.equals(modelId)) {
            mapper = new NdkPeriodicalMapper();
        } else if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(modelId)) {
            mapper = new NdkPeriodicalVolumeMapper();
        } else if (NdkPlugin.MODEL_PERIODICALISSUE.equals(modelId)) {
            mapper = new NdkPeriodicalIssueMapper();
        } else if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            mapper = new NdkPeriodicalSupplementMapper();
        } else if (NdkPlugin.MODEL_ARTICLE.equals(modelId)) {
            mapper = new NdkArticleMapper();
        } else if (NdkPlugin.MODEL_PICTURE.equals(modelId)) {
            mapper = new NdkPictureMapper();
        } else if (NdkPlugin.MODEL_MONOGRAPHTITLE.equals(modelId)) {
            mapper = new NdkMonographTitleMapper();
        } else if (NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId)) {
            mapper = new NdkMonographVolumeMapper();
        } else if (NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT.equals(modelId)) {
            mapper = new NdkMonographSupplementMapper();
        } else if (NdkPlugin.MODEL_CHAPTER.equals(modelId)) {
            mapper = new NdkChapterMapper();
        } else if (NdkPlugin.MODEL_CARTOGRAPHIC.equals(modelId)) {
            mapper = new NdkCartographicMapper();
        } else if (NdkPlugin.MODEL_SHEETMUSIC.equals(modelId)) {
            mapper = new NdkSheetMusicMapper();
        } else {
            throw new IllegalStateException("Unsupported model: " + modelId);
        }
        return mapper;
    }

    /**
     * Updates missing required attribute and elements to comply with the NDK specification.
     */
    public void createMods(ModsDefinition mods, Context ctx) {
        mods.setVersion(ModsUtils.VERSION);
        addPid(mods, ctx.getPid());
    }

    /**
     * Transforms MODS to DC.
     */
    public final OaiDcType toDc(ModsDefinition mods, Context ctx) {
        return createDc(mods, ctx);
    }

    /**
     * Override to provide own view of MODS for a JSON editor.
     * @param mods persisted MODS
     * @param ctx context
     * @return the serializable object
     */
    public ModsWrapper toJsonObject(ModsDefinition mods, Context ctx) {
        return new ModsWrapper(mods);
    }

    /**
     * Reads MODS from JSON. Use subclass of {@link ModsWrapper} to read a customized MODS.
     * @param jsMapper
     * @param json
     * @param ctx
     * @return MODS
     * @throws IOException failure
     */
    public ModsDefinition fromJsonObject(ObjectMapper jsMapper, String json, Context ctx) throws IOException {
        return jsMapper.readValue(json, ModsWrapper.class).getMods();
    }

    /**
     * Adds identifiers.
     */
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = new OaiDcType();
        List<ElementType> identifiers = dc.getIdentifiers();
        for (IdentifierDefinition identifier : mods.getIdentifier()) {
            String idVal = toValue(identifier.getValue());
            if (idVal == null) {
                continue;
            }
            String idType = toValue(identifier.getType());
            if (idType != null) {
                idVal = idType + ':' + idVal;
            }
            identifiers.add(new ElementType(idVal, null));
        }
        return dc;
    }

    /**
     * Transforms MODS to Fedora digital object label.
     */
    public final String toLabel(ModsDefinition mods) {
        String label = createObjectLabel(mods);
        if (label == null) {
            label = "?";
        } else {
            label = label.trim();
            label = label.isEmpty() ? "?" : label;
            int threshold = 2000;
            if (label.length() > threshold) {
                label = label.substring(0, threshold);
            }
        }
        return label;
    }

    /**
     * The default implementation creates label from titleInfo subelements.
     * @return label or {@code null}
     */
    protected String createObjectLabel(ModsDefinition mods) {
        for (TitleInfoDefinition ti : mods.getTitleInfo()) {
            if (toValue(ti.getType()) != null) {
                continue;
            }
            return createTitleString(ti);
        }
        return null;
    }

    /** Checks if the correct fields are filled depending on eventType */
    protected void checkOriginInfo(OriginInfoDefinition oi) {
        if (oi.getEventType() == null || oi.getEventType().equals(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION)) {
            isDateNull(oi.getCopyrightDate(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_COPYRIGHT, 0);
            isDateNull(oi.getDateOther(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_OTHER, 0);
            isDateEmpty(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, 1);
            isDateNull(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, 1);
        } else if (oi.getEventType().equals(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION) ||
                oi.getEventType().equals(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION) ||
                oi.getEventType().equals(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE)){
            isDateNull(oi.getCopyrightDate(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_COPYRIGHT, 0);
            isDateEmpty(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, 0);
            isDateNull(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, 0);
        } else if (oi.getEventType().equals(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT)){
            isDateEmpty(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, 0);
            isDateNull(oi.getDateIssued(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_ISSUED, 0);
            isDateNull(oi.getDateOther(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_OTHER, 0);
            isDateEmpty(oi.getCopyrightDate(), oi.getEventType(), ModsConstants.FIELD_ORIGININFO_DATE_COPYRIGHT, 1);
        } else {
            throw new IllegalArgumentException("Invalid value in element eventType");
        }
    }

    /** Checks if elements in List is null */
    private void isDateNull(List dates, String event, String element, int isEmpty) {
        for (Object date : dates) {
            if (isEmpty == 0) {
                if (!(((DateDefinition) date).getValue() == null)) {
                    throw new IllegalArgumentException("Pole " + element + " musí být prázdné, pokud je vyplněno eventType = " + event);
                }
            } else if (isEmpty == 1) {
                if (((DateDefinition) date).getValue() == null) {
                    throw new IllegalArgumentException("Pole " + element + " nesmí být prázdné, pokud je vyplněno eventType = " + event);
                }
            }
        }
    }

    /** Checks if the list is empty */
    private void isDateEmpty (List dates, String event, String element, int isEmpty) {
        if (isEmpty == 1 && dates.isEmpty()) {
            throw new IllegalArgumentException("Pole " + element + " nesmí být prázdné, pokud je vyplněno eventType = " + event);
        } else if (isEmpty == 0 && !dates.isEmpty()) {
            throw new IllegalArgumentException("Pole " + element + " musí být prázdné, pokud je vyplněno eventType = " + event);
        }
    }

    /** Checks if the correct fields are filled depending on eventType */
    protected void checkRules(ModsDefinition mods) {
        if (mods.getRecordInfo().isEmpty()) {
            return;
        }
        String descriptionStandard = mods.getRecordInfo().get(0).getDescriptionStandard().get(0).getValue();
        if (descriptionStandard == null) {
            throw new IllegalArgumentException("Missing value in descriptionStandard");
        } else if (!descriptionStandard.equalsIgnoreCase("rda")
                && !descriptionStandard.equalsIgnoreCase("aacr")) {
            throw new IllegalArgumentException("Wrong value in descriptionStandard");
        }
        List<OriginInfoDefinition> originInfoDefinitions = mods.getOriginInfo();
        List<PhysicalDescriptionDefinition> physicalDescriptions = mods.getPhysicalDescription();
        if (descriptionStandard.equalsIgnoreCase("rda")) {
            for (PhysicalDescriptionDefinition pd : physicalDescriptions) {
                if (!pd.getForm().isEmpty() && !pd.getForm().get(0).getAuthority().equals("rdamedia") && !pd.getForm().get(0).getAuthority().equals("rdacarrier")) {
                    throw new IllegalArgumentException("Pokud se zpracovává podle pravidel \"RDA\" potom v elementu physicalDescription musí být pole hodnota \"rdamedia\" nebo \"rdacarrier\".");
                }
            }
        } else if (descriptionStandard.equalsIgnoreCase("aacr")) {
            for (OriginInfoDefinition oi : originInfoDefinitions) {
                if (oi.getEventType() != null) {
                    throw new IllegalArgumentException("Pokud se zpracovává podle pravidel \"AACR\" potom musí být pole eventType prázdné.");
                }
            }
            for (PhysicalDescriptionDefinition pd : physicalDescriptions) {
                if (!pd.getForm().isEmpty() && !pd.getForm().get(0).getAuthority().equals("marcform") && !pd.getForm().get(0).getAuthority().equals("gmd")) {
                    throw new IllegalArgumentException("Pokud se zpracovává podle pravidel \"AACR\" potom v elementu physicalDescription musí být pole hodnota \"marcform\" nebo \"gmd\".");
                }
            }
        }
    }

    public static class Context {

        private DigitalObjectHandler handler;
        private String pid;

        public Context(DigitalObjectHandler handler) {
            this.handler = handler;
        }

        /**
         * Use this just in case there is no handler to provide.
         */
        public Context(String pid) {
            this.pid = pid;
        }

        public String getPid() {
            return handler == null ? pid: handler.getFedoraObject().getPid();
        }

        public DigitalObjectHandler getHandler() {
            return handler;
        }

    }

}
