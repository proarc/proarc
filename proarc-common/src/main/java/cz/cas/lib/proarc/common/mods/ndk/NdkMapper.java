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

import cz.cas.lib.proarc.common.mods.ModsUtils;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.*;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.ModsWrapper;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.List;
import org.codehaus.jackson.map.ObjectMapper;

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
        if (NdkPlugin.MODEL_PERIODICAL.equals(modelId)) {
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

    protected String createObjectLabel(ModsDefinition mods) {
        return null;
    }

    public static class Context {

        private DigitalObjectHandler handler;

        public Context(DigitalObjectHandler handler) {
            this.handler = handler;
        }

        public String getPid() {
            return handler.getFedoraObject().getPid();
        }

        public DigitalObjectHandler getHandler() {
            return handler;
        }

    }

}
