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
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.*;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.ModsWrapper;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.ndk.RdaRules;
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.List;
import org.apache.empire.commons.StringUtils;

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
    public ModsWrapper toJsonObject(ModsDefinition mods, Context ctx) throws DigitalObjectException {
        RdaModsWrapper wrapper = new RdaModsWrapper();
        wrapper.setMods(mods);
        if (RdaRules.HAS_MEMBER_RDA_VALIDATION_MODELS.contains(ctx.getHandler().relations().getModel())) {
            if (mods.getRecordInfo().isEmpty() || mods.getRecordInfo().get(0).getDescriptionStandard().isEmpty()) {
                return wrapper;
            }
            String descriptionStandard = mods.getRecordInfo().get(0).getDescriptionStandard().get(0).getValue();
            if (descriptionStandard.equalsIgnoreCase(ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA)) {
                mods.getRecordInfo().get(0).getDescriptionStandard().clear();
                wrapper.setRdaRules(true);
            } else {
                mods.getRecordInfo().get(0).getDescriptionStandard().clear();
                wrapper.setRdaRules(false);
            }
            return wrapper;
        }
        return wrapper;
    }

    /**
     * Reads MODS from JSON. Use subclass of {@link ModsWrapper} to read a customized MODS.
     * @param jsMapper
     * @param json
     * @param ctx
     * @return MODS
     * @throws IOException failure
     */
    public ModsDefinition fromJsonObject(ObjectMapper jsMapper, String json, Context ctx) throws IOException, DigitalObjectException {
        RdaModsWrapper wrapper = jsMapper.readValue(json, RdaModsWrapper.class);
        ModsDefinition mods = wrapper.getMods();

        if (RdaRules.HAS_MEMBER_RDA_VALIDATION_MODELS.contains(ctx.getHandler().relations().getModel())) {
            StringPlusLanguagePlusAuthority descriptionStandard = new StringPlusLanguagePlusAuthority();
            if (wrapper.getRdaRules() != null && wrapper.getRdaRules()) {
                descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA);
            } else {
                descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR);
            }
            mods.getRecordInfo().get(0).getDescriptionStandard().add(0, descriptionStandard);
            return mods;
        }
        return mods;
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

    /** Set default Authority value or repair it if needed. */
    protected void repairAuthorityInClassification(ClassificationDefinition classification){
        if (classification.getAuthority() == null) {
            classification.setAuthority("udc");
        }
        if (StringUtils.isNotEmpty(classification.getEdition()) && classification.getEdition().equals("Konspekt")){
            classification.setAuthority("udc"); // edition = "Konspekt" only if authority = "udc"
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

    public static class RdaModsWrapper extends ModsWrapper {

        private Boolean rdaRules;

        public Boolean getRdaRules() {
            return rdaRules;
        }

        public void setRdaRules(Boolean rdaRules) {
            this.rdaRules = rdaRules;
        }
    }
}
