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
import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.chronicle.ChronicleMapperFactory;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsMapperFactory;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsMapperFactory;
import cz.cas.lib.proarc.common.object.graphic.GraphicMapperFactory;
import cz.cas.lib.proarc.common.object.graphic.GraphicPlugin;
import cz.cas.lib.proarc.common.object.k4.K4MapperFactory;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.ModsWrapper;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintMapperFactory;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.ExtentDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionNote;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
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
    private static final GraphicMapperFactory graphicMapperFactory = new GraphicMapperFactory();
    private static final BornDigitalModsMapperFactory bornDigitalMapperFactory = new BornDigitalModsMapperFactory();
    private static final K4MapperFactory k4MapperFactory = new K4MapperFactory();

    /**
     * Gets a NDK mapper for the given model ID.
     * @param modelId model ID
     * @return the mapper
     */
    @Deprecated
    public static NdkMapper get(String modelId) {
        NdkMapper mapper;
        if (isNdkEModel(modelId) || isNdkModel(modelId)) {
            mapper = ndkMapperFactory.get(modelId);
        } else if (isChronicleModel(modelId)) {
            mapper = chronicleMapperFactory.get(modelId);
        } else if (isClippingsModel(modelId)) {
            mapper = clippingMapperFactory.get(modelId);
        } else if (isGraphicModel(modelId)) {
            mapper = graphicMapperFactory.get(modelId);
        } else if (isBornDigitalModel(modelId)) {
            mapper = bornDigitalMapperFactory.get(modelId);
        } else if(isOldprintModel(modelId)) {
            mapper = oldprintMapperFacotry.get(modelId);
        } else {
            mapper = k4MapperFactory.get(modelId);
        }
        mapper.modelId = modelId;
        return mapper;
    }

    private static boolean isBornDigitalModel(String modelId) {
        return modelId != null && modelId.contains("bdmarticle");
    }

    private static boolean isClippingsModel(String modelId) {
        return modelId != null && modelId.contains("clipping");
    }

    private static boolean isGraphicModel(String modelId) {
        return modelId != null && GraphicPlugin.MODEL_GRAPHIC.equals(modelId);
    }

    public static boolean isChronicleModel(String modelId) {
        return modelId != null && modelId.contains("chronicle");
    }

    public static boolean isNdkModel(String modelId) {
         return modelId != null && (modelId.contains("ndk") || NdkPlugin.MODEL_PAGE.equals(modelId));
    }

    public static boolean isNdkEModel(String modelId) {
        return modelId != null && modelId.contains("ndke");
    }

    public static boolean isOldprintModel(String modelId) {
        return modelId != null && (modelId.contains(OldPrintPlugin.ID));
    }

    public static boolean isK4Model(String modelId) {
        return modelId != null &&
                (modelId.equals(K4Plugin.MODEL_MONOGRAPH) ||
                        modelId.equals(K4Plugin.MODEL_MONOGRAPHUNIT) ||
                        modelId.equals(K4Plugin.MODEL_PERIODICAL) ||
                        modelId.equals(K4Plugin.MODEL_PERIODICALVOLUME) ||
                        modelId.equals(K4Plugin.MODEL_PERIODICALITEM));
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
                put(NdkPlugin.MODEL_MONOGRAPHVOLUME, "model:monograph");
                put(NdkPlugin.MODEL_MONOGRAPHUNIT, "model:monographunit");
                put(NdkPlugin.MODEL_PERIODICAL, "model:periodical");
                put(NdkPlugin.MODEL_PERIODICALISSUE, "model:periodicalitem");
                put(NdkPlugin.MODEL_PERIODICALSUPPLEMENT, "model:supplement");
                put(NdkPlugin.MODEL_PERIODICALVOLUME, "model:periodicalvolume");
                put(NdkPlugin.MODEL_PICTURE, "model:internalpart");
                put(NdkPlugin.MODEL_SHEETMUSIC, "model:sheetmusic");
                put(NdkAudioPlugin.MODEL_MUSICDOCUMENT, "model:soundrecording");
                put(NdkAudioPlugin.MODEL_PHONOGRAPH, "model:soundrecording");
                put(NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, "model:electronicmonograph");
                put(NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, "model:electronicmonographunit");
                put(NdkEbornPlugin.MODEL_EPERIODICAL, "model:electronic_periodical");
                put(NdkEbornPlugin.MODEL_EPERIODICALVOLUME, "model:periodicvolume");
                put(NdkEbornPlugin.MODEL_EPERIODICALISSUE, "model:periodicitem");
                put(NdkEbornPlugin.MODEL_ECHAPTER, "model:internalpart");
                put(NdkEbornPlugin.MODEL_EARTICLE, "model:internalpart");
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

    protected void addDetailNumber(String number, String type, PartDefinition part) {
        if (number != null) {
            DetailDefinition detail = new DetailDefinition();
            detail.setType(type);
            StringPlusLanguage splNumber = new StringPlusLanguage();
            splNumber.setValue(number);
            detail.getNumber().add(splNumber);
            part.getDetail().add(detail);
        }
    }

    protected void setNdkPageMods(NdkPageMapper.Page page, ModsDefinition mods) {
        if (page.getTitle() != null || page.getSubtitle() != null) {
            TitleInfoDefinition titleInfo = new TitleInfoDefinition();
            mods.getTitleInfo().add(titleInfo);

            setTitle(page, titleInfo);
            setSubtitle(page, titleInfo);
        }

        setPhysicalDescription(page, mods);
        setGenre(page, mods);
        setNote(page, mods);
        setTypeOfResource(page, mods);
        setExtent(page, mods);
    }

    private void setExtent(NdkPageMapper.Page page, ModsDefinition mods) {
        PartDefinition part = mods.getPart().get(0);
        if (page.getExtent() != null && part != null) {
            ExtentDefinition extentDefinition = new ExtentDefinition();
            extentDefinition.setUnit("pages");
            StringPlusLanguage extent = new StringPlusLanguage();
            extent.setValue(page.getExtent());
            extentDefinition.setStart(extent);
            part.getExtent().add(extentDefinition);
        }
    }

    private void setTypeOfResource(NdkPageMapper.Page page, ModsDefinition mods) {
        if (page.getTypeOfResource() != null) {
            TypeOfResourceDefinition typeOfResource = new TypeOfResourceDefinition();
            typeOfResource.setValue(page.getTypeOfResource());
            mods.getTypeOfResource().add(typeOfResource);
        }
    }

    private void setNote(NdkPageMapper.Page page, ModsDefinition mods) {
        if (page.getNote() != null) {
            NoteDefinition noteDefinition = new NoteDefinition();
            noteDefinition.setValue(page.getNote());
            mods.getNote().add(noteDefinition);
        }
    }

    private void setGenre(NdkPageMapper.Page page, ModsDefinition mods) {
        if (page.getGenre() != null) {
            GenreDefinition genreDefinition = new GenreDefinition();
            mods.getGenre().add(genreDefinition);
            genreDefinition.setValue(page.getGenre());
        }
    }

    private void setPhysicalDescription(NdkPageMapper.Page page, ModsDefinition mods) {
        if (page.getPhysicalDescription() != null) {
            PhysicalDescriptionDefinition physicalDescription = new PhysicalDescriptionDefinition();
            mods.getPhysicalDescription().add(physicalDescription);
            PhysicalDescriptionNote phNote = new PhysicalDescriptionNote();
            phNote.setValue(page.getPhysicalDescription());
            physicalDescription.getNote().add(phNote);
        }
    }

    private void setSubtitle(NdkPageMapper.Page page, TitleInfoDefinition titleInfo) {
        if (page.getSubtitle() != null) {
            StringPlusLanguage subtitle = new StringPlusLanguage();
            subtitle.setValue(page.getSubtitle());
            titleInfo.getSubTitle().add(subtitle);
        }
    }

    private void setTitle(NdkPageMapper.Page page, TitleInfoDefinition titleInfo) {
        if (page.getTitle() != null) {
            StringPlusLanguage title = new StringPlusLanguage();
            title.setValue(page.getTitle());
            titleInfo.getTitle().add(title);
        }
    }

    protected List<IdentifierDefinition> getIdentifierDefinition(List<IdentifierMapper.IdentifierItem> iis) {
        if (iis == null) {
            return Collections.emptyList();
        }
        ArrayList<IdentifierDefinition> ids = new ArrayList<>(iis.size());
        for (IdentifierMapper.IdentifierItem ii : iis) {
            String iiValue = MapperUtils.toValue(ii.getValue());
            if (iiValue != null) {
                IdentifierDefinition id = new IdentifierDefinition();
                id.setType(ii.getType());
                id.setValue(iiValue);
                ids.add(id);
            }
        }

        return ids;
    }

    public static class Context {

        private DigitalObjectHandler handler;
        private String pid;
        private String parentModel = null;

        public Context(DigitalObjectHandler handler) throws DigitalObjectException {
            this.handler = handler;
            if (handler.getParameterParent() != null && handler.getParameterParent().getModel() != null) {
                this.parentModel = handler.getParameterParent().getModel().getPid();
            }
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

    public static class PageModsWrapper extends ModsWrapper {

        private String pageType;
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

        public String getPageType() {
            return pageType;
        }

        public void setPageType(String pageType) {
            this.pageType = pageType;
        }
    }
}
