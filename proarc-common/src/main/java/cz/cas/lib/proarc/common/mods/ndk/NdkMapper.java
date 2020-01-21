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
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.chronicle.ChronicleMapperFactory;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsMapperFactory;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.ModsWrapper;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintMapperFactory;
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.empire.commons.StringUtils;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addPid;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.createTitleString;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.toValue;

/**
 * Subclass to implement transformations of MODS data in NDK flavor
 * to various formats for given digital object model.
 *
 * @author Jan Pokorsky
 */
public abstract class NdkMapper {

    /**
     * model ID
     */
    private String modelId;

    private static final NdkMapperFactory ndkMapperFactory = new NdkMapperFactory();
    private static final OldPrintMapperFactory oldprintMapperFacotry = new OldPrintMapperFactory();
    private static final ChronicleMapperFactory chronicleMapperFactory = new ChronicleMapperFactory();
    private static final CollectionOfClippingsMapperFactory clippingMapperFactory = new CollectionOfClippingsMapperFactory();

    /**
     * Gets a NDK mapper for the given model ID.
     * @param modelId model ID
     * @return the mapper
     */
    @Deprecated
    public static NdkMapper get(String modelId) {
        NdkMapper mapper;
        if (isNdkModel(modelId)) {
            mapper = ndkMapperFactory.get(modelId);
        } else if (isChronicleModel(modelId)) {
            mapper = chronicleMapperFactory.get(modelId);
        } else if (isClippingsModel(modelId)) {
            mapper = clippingMapperFactory.get(modelId);
        } else {
            mapper = oldprintMapperFacotry.get(modelId);
        }
        mapper.modelId = modelId;
        return mapper;
    }

    private static boolean isClippingsModel(String modelId) {
        return modelId != null && modelId.contains("clipping");
    }

    private static boolean isChronicleModel(String modelId) {
        return modelId != null && modelId.contains("chronicle");
    }

    private static boolean isNdkModel(String modelId) {
         return modelId != null && modelId.contains("ndk");
    }

    public String getModelId() {
        return modelId;
    }

    public void setModelId(String modelId) {
        this.modelId = modelId;
    }

    /**
     * Updates missing required attribute and elements to comply with the NDK specification.
     */
    public void createMods(ModsDefinition mods, Context ctx) {
        mods.setVersion(ModsUtils.VERSION);
        if (ctx.getPid() != null) {
            addPid(mods, ctx.getPid());
            checkUuidIdentifier(mods, ctx.getPid());
        }
    }

    private void checkUuidIdentifier(ModsDefinition mods, String pid) {
        String uuid = FoxmlUtils.pidAsUuid(pid);
        for (IdentifierDefinition id : mods.getIdentifier()) {
            if ("uuid".equals(id.getType()) && !uuid.equals(id.getValue())) {
                id.setInvalid("yes");
            }
        }
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
     *  @see <a href="http://www.ndk.cz/standardy-digitalizace/elementy-modsgenre_dctype">modsgenre elements</a>
     */
    protected final String getDcType() {
        Map<String, String> modelMap = new HashMap<String, String>() {
            {
                put(NdkPlugin.MODEL_ARTICLE, "model:internalpart");
                put(NdkPlugin.MODEL_CARTOGRAPHIC, "model:map");
                put(NdkPlugin.MODEL_CHAPTER, "model:internalpart");
                put(NdkPlugin.MODEL_MONOGRAPHTITLE, "model:monograph");
                put(NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, "model:supplement");
                put(NdkPlugin.MODEL_MONOGRAPHVOLUME, "model:monographunit");
                put(NdkPlugin.MODEL_PERIODICAL, "model:periodical");
                put(NdkPlugin.MODEL_PERIODICALISSUE, "model:periodicalitem");
                put(NdkPlugin.MODEL_PERIODICALSUPPLEMENT, "model:supplement");
                put(NdkPlugin.MODEL_PERIODICALVOLUME, "model:periodicalvolume");
                put(NdkPlugin.MODEL_PICTURE, "model:internalpart");
                put(NdkPlugin.MODEL_SHEETMUSIC, "model:sheetmusic");
                put(NdkAudioPlugin.MODEL_MUSICDOCUMENT, "model:soundrecording");
                put(NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, "model:electronicmonograph");
                put(NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, "model:electronicmonographunit");
                put(NdkEbornPlugin.MODEL_ECHAPTER, "model:internalpart");
            }
        };

        return modelMap.get(modelId);
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
        private String parentModel = null;

        public Context(DigitalObjectHandler handler) {
            this.handler = handler;
        }

        /**
         * Use this just in case there is no handler to provide.
         */
        public Context(String pid) {
            this.pid = pid;
        }

        /**
         * Use this just in case there is no handler to provide.
         */
        public Context(String pid, String parentModel) {
            this.pid = pid;
            this.parentModel = parentModel;
        }

        public String getPid() {
            return handler == null ? pid: handler.getFedoraObject().getPid();
        }

        public String getParentModel() {
            try {
                return handler != null && handler.getParameterParent() != null && handler.getParameterParent().getModel() != null ? handler.getParameterParent().getModel().getPid() : parentModel;
            } catch (DigitalObjectException e) {
                return null;
            }
        }

        public DigitalObjectHandler getHandler() {
            return handler;
        }

    }

    public static class RdaModsWrapper extends ModsWrapper {

        private Boolean rdaRules;

        public Boolean getRdaRules() {
            return rdaRules == null ? true : rdaRules;
        }

        public void setRdaRules(Boolean rdaRules) {
            this.rdaRules = rdaRules;
        }
    }

    public static class PageModsWrapper extends RdaModsWrapper {

        private String type;
        private String pageIndex;
        private String pageNumber;

        public String getPageNumber() {
            return pageNumber;
        }

        public void setPageNumber(String pageNumber) {
            this.pageNumber = pageNumber;
        }

        public String getPageIndex() {
            return pageIndex;
        }

        public void setPageIndex(String pageIndex) {
            this.pageIndex = pageIndex;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }
    }
}
