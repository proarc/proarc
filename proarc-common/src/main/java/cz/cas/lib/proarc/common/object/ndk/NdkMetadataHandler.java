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
package cz.cas.lib.proarc.common.object.ndk;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapperFactory;
import cz.cas.lib.proarc.common.mods.ndk.NdkNewPageMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper.Page;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ModsDataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.process.export.mets.ValidationErrorHandler;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.PageView.PageViewHandler;
import cz.cas.lib.proarc.common.storage.PageView.PageViewItem;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.SearchViewQuery;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.xml.bind.DataBindingException;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Validator;
import org.xml.sax.SAXException;

/**
 * Handles description metadata in the NDK format.
 *
 * @author Jan Pokorsky
 */
public class NdkMetadataHandler implements MetadataHandler<ModsDefinition>, PageViewHandler {

    public static final String ERR_NDK_CHANGE_MODS_WITH_URNNBN = "Err_Ndk_Change_Mods_With_UrnNbn";
    public static final String ERR_NDK_CHANGE_MODS_WITH_MEMBERS = "Err_Ndk_Change_Mods_With_Members";
    public static final String ERR_NDK_DOI_DUPLICITY = "Err_Ndk_Doi_Duplicity";
    public static final String ERR_NDK_REMOVE_URNNBN = "Err_Ndk_Remove_UrnNbn";
    public static final String DEFAULT_PAGE_TYPE = "normalPage";

    public static final String OPERATION_NEW = "new";
    public static final String OPERATION_VALIDATE = "validate";
    public static final String OPERATION_UPDATE = "update";
    public static final String OPERATION_URNNBN = "addUrnNbn";

    /**
     * The set of model IDs that should be checked for connected members.
     */
    private static final Set<String> HAS_MEMBER_VALIDATION_MODELS = Collections.unmodifiableSet(new HashSet<String>(Arrays.asList(
            NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALVOLUME
    )));
    private static final Logger LOG = Logger.getLogger(NdkMetadataHandler.class.getName());

    protected final DigitalObjectHandler handler;
    protected final ModsStreamEditor editor;
    protected final ProArcObject fobject;
    protected DigitalObjectCrawler crawler;
    private final NdkMapperFactory mapperFactory;
    private static AppConfiguration appConfiguration;
    private static AkubraConfiguration akubraConfiguration;

    public NdkMetadataHandler(DigitalObjectHandler handler) {
        this(handler, new NdkMapperFactory());
    }

    public NdkMetadataHandler(DigitalObjectHandler handler, NdkMapperFactory mapperFactory) {
        this.handler = handler;
        this.fobject = handler.getFedoraObject();
        XmlStreamEditor streamEditor = fobject.getEditor(FoxmlUtils.inlineProfile(
                DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, DESCRIPTION_DATASTREAM_LABEL));
        this.editor = new ModsStreamEditor(streamEditor, fobject);
        this.mapperFactory = mapperFactory;
        try {
            this.appConfiguration = AppConfigurationFactory.getInstance().defaultInstance();
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfiguration.getConfigHome());
        } catch (AppConfigurationException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void setMetadata(DescriptionMetadata<ModsDefinition> data, String message, String typeRecord) throws DigitalObjectException {
        ModsDefinition mods = data.getData();
        String modelId = handler.relations().getModel();
        if (mods == null) {
            mods = createDefault(modelId);
        }
        write(modelId, mods, data, message, typeRecord);
    }

    /**
     * Creates a new MODS with required default values according to model ID.
     * Override to support custom models.
     */
    protected ModsDefinition createDefault(String modelId) throws DigitalObjectException {
        ModsDataHandler modsDataHandler = new ModsDataHandler(appConfiguration);
        ModsDefinition defaultMods = modsDataHandler.createDefaultMetadata(fobject.getPid(), modelId, handler, null);
        return defaultMods;
    }

    @Override
    public void setMetadataAsJson(DescriptionMetadata<String> jsonData, String message, String typeRecord) throws DigitalObjectException {
        String json = jsonData.getData();
        String modelId = handler.getModel().getPid();
        ModsDefinition mods;
        if (json == null) {
            mods = createDefault(modelId);
        } else {
            NdkMapper mapper = mapperFactory.get(modelId);
            mapper.setModelId(modelId);
            Context context = new Context(handler);
            ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
            try {
                mods = mapper.fromJsonObject(jsMapper, json, context);
            } catch (Exception ex) {
                throw new DigitalObjectException(fobject.getPid(), null, ModsStreamEditor.DATASTREAM_ID, null, ex);
            }
        }

        String standard = jsonData.getStandard();
        if ("aacr".equals(standard) || "rda".equals(standard)) {
            mods = ModsUtils.overrideDescriptionStandard(mods, standard);
        }

        write(modelId, mods, jsonData, message, typeRecord);
    }

    @Override
    public void setMetadataAsXml(DescriptionMetadata<String> xmlData, String message, String typeRecord) throws DigitalObjectException {
        ModsDefinition mods;
        String modelId = handler.getModel().getPid();
        if (xmlData.getData() != null) {
            ValidationErrorHandler errHandler = new ValidationErrorHandler();
            try {
                String data = xmlData.getData();
                xmlData.setData(data);

                Validator validator = ModsUtils.getSchema().newValidator();
                validator.setErrorHandler(errHandler);
                validator.validate(new StreamSource(new StringReader(xmlData.getData())));
                checkValidation(errHandler, xmlData);
                mods = ModsUtils.unmarshalModsType(new StreamSource(new StringReader(xmlData.getData())));
            } catch (DataBindingException | SAXException | IOException ex) {
                checkValidation(errHandler, xmlData);
                throw new DigitalObjectValidationException(xmlData.getPid(),
                            xmlData.getBatchId(), ModsStreamEditor.DATASTREAM_ID, null, ex)
                        .addValidation("mods", ex.getMessage(), true);
            }
        } else {
            mods = createDefault(modelId);
        }
        String standard = xmlData.getStandard();
        if ("aacr".equals(standard) || "rda".equals(standard)) {
            mods = ModsUtils.overrideDescriptionStandard(mods, standard);
        }

        write(modelId, mods, xmlData, message, typeRecord);
    }

    @Override
    public void validateMetadataAsJson(DescriptionMetadata<Object> jsonData) throws DigitalObjectException {
        Object json = jsonData.getData();
        String modelId = handler.getModel().getPid();
        ModsDefinition mods = null;
        if (json == null) {
            throw new DigitalObjectException(handler.getFedoraObject().getPid(), "No data - nothing to validate.");
        } else {
            if (json instanceof NdkMapper.RdaModsWrapper) {
                NdkMapper.RdaModsWrapper wrapper = (NdkMapper.RdaModsWrapper) json;
                mods = wrapper.getMods();
            } else {
                throw new DigitalObjectException(handler.getFedoraObject().getPid(), "Unsupported type of data.");
            }
        }
        validate(modelId, mods, jsonData);
    }

    @Override
    public void validateMetadataAsXml(DescriptionMetadata<String> xmlData) throws DigitalObjectException {
        String data = xmlData.getData();
        String modelId = handler.getModel().getPid();
        ModsDefinition mods;
        if (data == null) {
            throw new DigitalObjectValidationException(xmlData.getPid(), xmlData.getBatchId(), ModsStreamEditor.DATASTREAM_ID, "No data - nothing to validate.", null);
        } else {
            ValidationErrorHandler errHandler = new ValidationErrorHandler();
            try {
                xmlData.setData(data);

                Validator validator = ModsUtils.getSchema().newValidator();
                validator.setErrorHandler(errHandler);
                validator.validate(new StreamSource(new StringReader(xmlData.getData())));
                checkValidation(errHandler, xmlData);
                mods = ModsUtils.unmarshalModsType(new StreamSource(new StringReader(xmlData.getData())));
            } catch (DataBindingException | SAXException | IOException ex) {
                checkValidation(errHandler, xmlData);
                throw new DigitalObjectValidationException(xmlData.getPid(),
                            xmlData.getBatchId(), ModsStreamEditor.DATASTREAM_ID, null, ex)
                        .addValidation("mods", ex.getMessage(), true);
            }
        }
        validate(modelId, mods, xmlData);
    }

    private void checkValidation(ValidationErrorHandler errHandler, DescriptionMetadata<String> xmlData)
            throws DigitalObjectValidationException {
        if (!errHandler.getValidationErrors().isEmpty()) {
            String msg = errHandler.getValidationErrors().stream().collect(Collectors.joining("\n"));
            throw new DigitalObjectValidationException(xmlData.getPid(), xmlData.getBatchId(), ModsStreamEditor.DATASTREAM_ID, msg, null)
                    .addValidation("mods", msg, true);
        }
    }

    @Override
    public DescriptionMetadata<ModsDefinition> getMetadata() throws DigitalObjectException {
        ModsDefinition mods = editor.read();
        DescriptionMetadata<ModsDefinition> dm = new DescriptionMetadata<ModsDefinition>();
        dm.setPid(fobject.getPid());
        dm.setTimestamp(editor.getLastModified());
//            dm.setEditor(editorId);
        dm.setData(mods);
        return dm;
    }

    @Override
    @SuppressWarnings("unchecked")
    public <O> DescriptionMetadata<O> getMetadataAsJsonObject(String mappingId) throws DigitalObjectException {
        DescriptionMetadata<ModsDefinition> dm = getMetadata();
        DescriptionMetadata json = dm;
        String modelId = null;
        if (mappingId == null || ModsCutomEditorType.EDITOR_PAGE.equals(mappingId)) {
            modelId = handler.relations().getModel();
            MetaModel model = modelId == null ? null : MetaModelRepository.getInstance().find(modelId);
            if (model == null) {
                throw new DigitalObjectException(fobject.getPid(), null, "ds", "Missing mappingId!", null);
            }
            if (mappingId == null) {
                mappingId = model.getModsCustomEditor();
            }
        }
        NdkMapper mapper = mapperFactory.get(mappingId);
        mapper.setModelId(ModsCutomEditorType.EDITOR_PAGE.equals(mappingId) ? modelId : mappingId);
        Context context = new Context(handler);
        json.setData(mapper.toJsonObject(dm.getData(), context));
        json.setEditor(mappingId);
        return json;
    }

    @Override
    public DescriptionMetadata<String> getMetadataAsXml() throws DigitalObjectException {
        String xml = editor.readAsString();
        DescriptionMetadata<String> dm = new DescriptionMetadata<String>();
        dm.setPid(fobject.getPid());
        dm.setTimestamp(editor.getLastModified());
//            dm.setEditor(editorId);
        dm.setData(xml);
        return dm;
    }

    @Override
    public PageViewItem createPageViewItem(Locale locale) throws DigitalObjectException {
        String modelId = handler.relations().getModel();
        if (modelId.equals(NdkPlugin.MODEL_PAGE)) {
            ModsDefinition mods = editor.read();
            NdkPageMapper mapper = new NdkPageMapper();
            Page page = mapper.toJsonObject(mods, new Context(handler));
            PageViewItem item = new PageViewItem();
            item.setPageIndex(page.getIndex());
            item.setPageNumber(page.getNumber());
            item.setPageType(page.getType());
            item.setPageTypeLabel(NdkPageMapper.getPageTypeLabel(item.getPageType(), locale));
            return item;
        } else if (NdkPlugin.MODEL_NDK_PAGE.equals(modelId)) {
            ModsDefinition mods = editor.read();
            NdkNewPageMapper mapper = new NdkNewPageMapper();
            PageViewItem item = new PageViewItem();
            item.setPageIndex(mapper.getIndex(mods));
            item.setPageNumber(mapper.getNumber(mods));
            item.setPageType(mapper.getType(mods));
            item.setPagePosition(mapper.getPosition(mods));
            item.setPageRepre(mapper.getPageRepre(mods));
            item.setPageTypeLabel(NdkPageMapper.getPageTypeLabel(item.getPageType(), locale));
            return item;
        } else {
            throw new DigitalObjectException(fobject.getPid(), "Unexpected model for NDK page: " + modelId);
        }
    }

    @Override
    public void setPage(PageViewItem page, String message) throws DigitalObjectException {
        String modelId = handler.relations().getModel();
        if (modelId.equals(NdkPlugin.MODEL_PAGE)) {
            DescriptionMetadata<ModsDefinition> metadata = new DescriptionMetadata<ModsDefinition>();
            metadata.setTimestamp(editor.getLastModified());
            NdkPageMapper mapper = new NdkPageMapper();
            ModsDefinition mods = mapper.createPage(
                    page.getPageIndex(), page.getPageNumber(), page.getPageType(), new Context(handler));
            metadata.setIgnoreValidation(true);
            write(modelId, mods, metadata, message, NdkMetadataHandler.OPERATION_UPDATE);
        } else if (NdkPlugin.MODEL_NDK_PAGE.equals(modelId)) {
            DescriptionMetadata<ModsDefinition> metadata = new DescriptionMetadata<ModsDefinition>();
            metadata.setTimestamp(editor.getLastModified());
            NdkNewPageMapper mapper = new NdkNewPageMapper();
            ModsDefinition mods = mapper.createPage(
                    page.getPageIndex(), page.getPageNumber(), page.getPageType(), new Context(handler));
            metadata.setIgnoreValidation(true);
            write(modelId, mods, metadata, message, NdkMetadataHandler.OPERATION_UPDATE);
        } else {
            throw new DigitalObjectException(fobject.getPid(), "Unexpected model for NDK page: " + modelId);
        }
    }

    private void checkBeforeWrite(boolean skipUrnNbnValidation, ModsDefinition mods, ModsDefinition oldMods, boolean ignoreValidations, boolean ignoreConnectedModels, String modelId, Context context) throws DigitalObjectException {
        if (ignoreValidations) {
            checkIdentifiers(skipUrnNbnValidation, mods, oldMods, null);

            DigitalObjectValidationException ex = new DigitalObjectValidationException(fobject.getPid(), null,
                    DESCRIPTION_DATASTREAM_ID, "MODS validation", null);
            ModsRules modsRules = new ModsRules(modelId, mods, ex, context, appConfiguration);
            modsRules.checkPhysicalLocation(mods.getLocation());
            modsRules.checkRelatedItemPhysicalLocation(mods.getRelatedItem());
            modsRules.checkDateIssuedFormat(mods, modelId);
            modsRules.checkGenreType(mods, modelId);
            if (!ex.getValidations().isEmpty()) {
                throw ex;
            }
            return ;
        }
        DigitalObjectValidationException ex = new DigitalObjectValidationException(fobject.getPid(), null,
                DESCRIPTION_DATASTREAM_ID, "MODS validation", null);
        checkIdentifiers(skipUrnNbnValidation, mods, oldMods, ex);
        RelationEditor relations = handler.relations();
        List<String> members = relations.getMembers();
        if (!ignoreConnectedModels) {
            if (HAS_MEMBER_VALIDATION_MODELS.contains(relations.getModel()) && !members.isEmpty()) {
                ex.addValidation("mods", ERR_NDK_CHANGE_MODS_WITH_MEMBERS, true);
            }
        }

        RdaRules rdaRules = new RdaRules(modelId, mods, ex);
        rdaRules.check();

        ModsRules modsRules = new ModsRules(modelId, mods, ex, context, appConfiguration);
        modsRules.check();

        if (!ex.getValidations().isEmpty()) {
            throw ex;
        }
    }

    private void checkIdentifiers(boolean skipUrnNbnValidation, ModsDefinition mods, ModsDefinition oldMods, DigitalObjectValidationException ex) throws DigitalObjectException {
        if (fobject != null && fobject.getPid() != null) {
            ModsStreamEditor.addPid(mods, fobject.getPid());
        }
        List<IdentifierDefinition> oldIds = oldMods != null ? oldMods.getIdentifier()
                : Collections.<IdentifierDefinition>emptyList();
        // check URN:NBN
        for (IdentifierDefinition oldId : oldIds) {
            if (!skipUrnNbnValidation) {
                if ("urnnbn".equals(oldId.getType()) && oldId.getValue() != null && !oldId.getValue().trim().isEmpty() && (oldId.getInvalid() == null || "no".equals(oldId.getInvalid()))) {
                    boolean missingId = true;
                    for (IdentifierDefinition id : mods.getIdentifier()) {
                        if (oldId.getType().equals(id.getType()) && oldId.getValue().equals(id.getValue())) {
                            missingId = false;
                            break;
                        }
                    }
                    if (missingId) {
                        if (ex != null) {
                            ex.addValidation("mods.identifier", ERR_NDK_REMOVE_URNNBN, true, oldId.getValue());
                        } else {
                            mods.getIdentifier().add(oldId);
                        }
                    } else if (ex != null) {
                        ex.addValidation("mods.identifier", ERR_NDK_CHANGE_MODS_WITH_URNNBN, true, oldId.getValue());
                    }
                }
            }
        }
        checkDoiDuplicity(mods, ex);
    }

    /** issue 443. */
    private void checkDoiDuplicity(ModsDefinition mods, DigitalObjectValidationException ex) throws DigitalObjectException {
        if (ex == null) {
            return ;
        }
        SearchView search = null;
        try {
            if (Storage.FEDORA.equals(appConfiguration.getTypeOfStorage())) {
                search = FedoraStorage.getInstance().getSearch();
            } else if (Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
                search = AkubraStorage.getInstance(akubraConfiguration).getSearch();
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfiguration.getTypeOfStorage());
            }
        } catch (IOException ioException) {
            throw new IllegalStateException(ioException);
        }
        for (IdentifierDefinition idDef : mods.getIdentifier()) {
            if ("doi".equals(idDef.getType()) && idDef.getValue() != null) {
                String doi = idDef.getValue();
                if (doi != null && !doi.isEmpty()) {
                    try {
                        List<SearchViewItem> results = search.findQuery(new SearchViewQuery().setIdentifier(doi), "active");
                        if (!results.isEmpty()) {
                            if (results.size() == 1 && results.get(0).getPid().equals(fobject.getPid())) {
                                // ignore the self-reference
                                continue;
                            }
                            ex.addValidation("mods.identifier", ERR_NDK_DOI_DUPLICITY, true, doi);
                        }
                    } catch (FedoraClientException ex1) {
                        throw new DigitalObjectException(fobject.getPid(), ex1);
                    } catch (IOException ex1) {
                        throw new DigitalObjectException(fobject.getPid(), ex1);
                    }
                }
            }
        }
    }

    protected void validate(String modelId, ModsDefinition mods, DescriptionMetadata<?> options) throws DigitalObjectException {
        ModsDefinition oldMods = editor.read();
        Context context = new Context(handler);
        checkBeforeWrite(true, mods, oldMods, false, true, modelId, context);
    }

    protected void write(String modelId, ModsDefinition mods,
            DescriptionMetadata<?> options, String message, String typeRecord) throws DigitalObjectException {
        ModsDefinition oldMods = null;
        long timestamp = options.getTimestamp();
        if (timestamp < 0) {
            // rewrite with brand new MODS
            timestamp = editor.getLastModified();
        }
        if (timestamp > 0) {
            oldMods = editor.read();
        }
        Context context = new Context(handler);
        context.setOperation(typeRecord);

        if (OPERATION_VALIDATE.equals(typeRecord)) {
            checkBeforeWrite(false, mods, oldMods, options.isIgnoreValidation(), false, modelId, context);
        } else if (!(OPERATION_NEW.equals(typeRecord) || OPERATION_URNNBN.equals(typeRecord))) {
            checkBeforeWrite(false, mods, oldMods, options.isIgnoreValidation(), false, modelId, context);
        }
        NdkMapper mapper = mapperFactory.get(modelId);
        mapper.setModelId(modelId);

        mapper.createMods(mods, context);
        if (LOG.isLoggable(Level.FINE)) {
            String toXml = ModsUtils.toXml(mods, true);
            LOG.fine(toXml);
        }
        editor.write(mods, timestamp, message);

        // DC
        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = handler.objectMetadata();
        DublinCoreRecord dcr = dcEditor.read();
        dcr.setDc(dc);
        dcEditor.write(handler, dcr, message);

        // Label
        String label = mapper.toLabel(mods);
        fobject.setLabel(label);
    }

    protected final DigitalObjectHandler findEnclosingObject(
            DigitalObjectHandler obj, String searcModelId) throws DigitalObjectException {

        if (obj != null) {
            if (searcModelId.equals(obj.relations().getModel())) {
                return obj;
            } else {
                DigitalObjectElement parent = getCrawler().getParent(obj.getFedoraObject().getPid());
                return findEnclosingObject(parent, searcModelId);
            }
        }
        return null;
    }

    private DigitalObjectHandler findEnclosingObject(
            DigitalObjectElement obj, String searcModelId) throws DigitalObjectException {

        if (obj == DigitalObjectElement.NULL) {
            return null;
        } else if (searcModelId.equals(obj.getModelId())) {
            return obj.getHandler();
        } else {
            DigitalObjectElement parent = getCrawler().getParent(obj.getPid());
            return findEnclosingObject(parent, searcModelId);
        }
    }

    public DigitalObjectCrawler getCrawler() {
        if (crawler == null) {
            try {
                if (Storage.FEDORA.equals(appConfiguration.getTypeOfStorage())) {
                    crawler = new DigitalObjectCrawler(DigitalObjectManager.getDefault(), FedoraStorage.getInstance().getSearch());
                } else if (Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
                    crawler = new DigitalObjectCrawler(DigitalObjectManager.getDefault(), AkubraStorage.getInstance(akubraConfiguration).getSearch());
                } else {
                    throw new IllegalStateException("Unsupported type of storage: " + appConfiguration.getTypeOfStorage());
                }
            } catch (Exception ex) {
              throw new IllegalStateException(ex);
            }
        }
        return crawler;
    }

    /**
     * Wraps MODS for JSON serialization. Subclasses can add own properties.
     */
    public static class ModsWrapper {

        private ModsDefinition mods;

        public ModsWrapper() {
        }

        public ModsWrapper(ModsDefinition mods) {
            this.mods = mods;
        }

        public ModsDefinition getMods() {
            return mods;
        }

        public void setMods(ModsDefinition mods) {
            this.mods = mods;
        }
    }
}
