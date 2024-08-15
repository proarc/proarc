package cz.cas.lib.proarc.common.actions;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.chronicle.ChroniclePlugin;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsPlugin;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.graphic.GraphicPlugin;
import cz.cas.lib.proarc.common.object.ndk.ModsRules;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.ndk.RdaRules;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.process.export.archive.ArchiveObjectProcessor;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraUtils;
import cz.cas.lib.proarc.common.storage.akubra.SolrSearchView;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXBException;

public class Validation {

    private static final Logger LOG = Logger.getLogger(Validation.class.getName());

    private static AppConfiguration appConfig;
    private static AkubraConfiguration akubraConfiguration;
    private static List<String> pids;
    private static Locale locale;
    private static AkubraStorage storage;
    private static SolrSearchView search;

    private static final Set<String> PAGE_MODELS = new HashSet<String>(Arrays.asList(NdkPlugin.MODEL_PAGE, NdkPlugin.MODEL_NDK_PAGE, OldPrintPlugin.MODEL_PAGE));
    private int indexPageValue;
    private boolean reprePageValue;
    private String positionPageValue;

    private static final Set<String> CONTAINS_PDF = new HashSet<String>(Arrays.asList(NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT, NdkEbornPlugin.MODEL_ECHAPTER, NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT, NdkEbornPlugin.MODEL_EPERIODICALISSUE, NdkEbornPlugin.MODEL_EARTICLE, BornDigitalModsPlugin.MODEL_ARTICLE));
    private static final Set<String> CONTAINS_AUDIO_PAGE = new HashSet<String>(Arrays.asList(NdkAudioPlugin.MODEL_SONG, NdkAudioPlugin.MODEL_TRACK));
    private static final Set<String> CONTAINS_PAGE = new HashSet<String>(Arrays.asList(
            NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT,
            NdkPlugin.MODEL_PERIODICALISSUE, NdkPlugin.MODEL_PERIODICALSUPPLEMENT,
            NdkPlugin.MODEL_CARTOGRAPHIC, NdkPlugin.MODEL_GRAPHIC, NdkPlugin.MODEL_SHEETMUSIC,
            OldPrintPlugin.MODEL_CONVOLUTTE, OldPrintPlugin.MODEL_VOLUME, OldPrintPlugin.MODEL_SUPPLEMENT,
            OldPrintPlugin.MODEL_CARTOGRAPHIC, OldPrintPlugin.MODEL_GRAPHICS, OldPrintPlugin.MODEL_SHEETMUSIC,
            NdkAudioPlugin.MODEL_MUSICDOCUMENT, NdkAudioPlugin.MODEL_PHONOGRAPH, NdkAudioPlugin.MODEL_SONG, NdkAudioPlugin.MODEL_TRACK,
            CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME, GraphicPlugin.MODEL_GRAPHIC,
            ChroniclePlugin.MODEL_CHRONICLEVOLUME, ChroniclePlugin.MODEL_CHRONICLESUPPLEMENT
    ));

    private static final Set<String> REQUIRED_URNNBN_MODELS = new HashSet<String>(Arrays.asList(
            NdkPlugin.MODEL_PERIODICALISSUE, NdkPlugin.MODEL_PERIODICALSUPPLEMENT,
            NdkPlugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT,
            NdkPlugin.MODEL_CARTOGRAPHIC, NdkPlugin.MODEL_GRAPHIC, NdkPlugin.MODEL_SHEETMUSIC,
            NdkAudioPlugin.MODEL_MUSICDOCUMENT, NdkAudioPlugin.MODEL_PHONOGRAPH,
            NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT,
            NdkEbornPlugin.MODEL_EPERIODICALISSUE, NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT,
            OldPrintPlugin.MODEL_VOLUME, OldPrintPlugin.MODEL_SUPPLEMENT,
            OldPrintPlugin.MODEL_GRAPHICS, OldPrintPlugin.MODEL_CARTOGRAPHIC, OldPrintPlugin.MODEL_SHEETMUSIC
    ));

    public Validation(AppConfiguration appConfig, AkubraConfiguration akubraConfiguration, List<String> pids, Locale locale) {
        this.appConfig = appConfig;
        this.akubraConfiguration = akubraConfiguration;
        this.pids = pids;
        this.locale = locale;

        this.indexPageValue = 1;
        this.reprePageValue = false;
        this.positionPageValue = null;
    }

    public enum Type {
        EXPORT_NDK, EXPORT_ARCHIVE, VALIDATION
    }

    public Result validate(Type type) throws IOException {
        Result result = new Result();

        if (!Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            result.getValidationResults().add(new ValidationResult("Úložiště", "Validace je podporována jen s AKUBROU!", Level.SEVERE));
            return result;
        } else {
            storage = AkubraStorage.getInstance(akubraConfiguration);
            search = storage.getSearch(locale);
        }

        List<SearchViewItem> items = search.find(pids);
        for (SearchViewItem item : items) {
            List<SearchViewItem> parentsList = search.findReferrers(item.getPid());
            if (parentsList.size() > 1) {
                result.getValidationResults().add(new ValidationResult(item.getPid(), "Objekt má více nadřazených objektů!", Level.SEVERE));
            }
            validatePid(item, result, parentsList.isEmpty() ? null : parentsList.get(0).getModel(), type);
        }

        return result;
    }

    private void validatePid(SearchViewItem item, Result result, String parentModel, Type type) {
        if (item == null) {
            result.getValidationResults().add(new ValidationResult("PID", "Objekt nenalezen v SOLRu", Level.SEVERE));
            return;
        }

        String model = item.getModel();

        AkubraStorage.AkubraObject akubraObject = storage.find(item.getPid());
        List<SearchViewItem> children = new ArrayList<>();
        try {
            children = search.findSortedChildren(item.getPid());
        } catch (Throwable t) {
            result.getValidationResults().add(new ValidationResult(item.getPid(), "Nepodařilo se získat potomky a tím pádem je validovat.", Level.SEVERE));
        }

        // seznam validaci
        if (PAGE_MODELS.contains(model)) {
            validatePage(item, result);
        }
        if (CONTAINS_PDF.contains(model)) {
            validateContainsPdf(akubraObject, !children.isEmpty(), result);
        }
        if (CONTAINS_AUDIO_PAGE.contains(model)) {
            validateMusic(item, children, result);
        }
        if (CONTAINS_PAGE.contains(model)) {
            validatePages(item, children, result, type);
        }

        ModsDefinition mods = null;
        try {
            mods = getMods(item.getPid());
        } catch (DigitalObjectException e) {
            result.getValidationResults().add(new ValidationResult(item.getPid(), "Nepodařilo se načíst MODS.", Level.SEVERE, e));
        }
        DigitalObjectValidationException ex = new DigitalObjectValidationException(akubraObject.getPid(), null,
                ModsStreamEditor.DATASTREAM_ID, "MODS validation", null);
        if (mods != null) {
            ModsRules modsRules = new ModsRules(item.getModel(), mods, ex, parentModel, appConfig);
            try {
                modsRules.check();
            } catch (DigitalObjectValidationException e) {
                processValidationException(item.getPid(), e, result);
            }
            RdaRules rdaRules = new RdaRules(item.getModel(), mods, ex);
            try {
                rdaRules.check();
            } catch (DigitalObjectValidationException e) {
                processValidationException(item.getPid(), e, result);
            }
        }

        // validace potomku
        for (SearchViewItem child : children) {
            validatePid(child, result, item.getModel(), type);
        }

        // urnnbn validace jako posledni
        if (REQUIRED_URNNBN_MODELS.contains(model) && !ArchiveObjectProcessor.containUrnNbn(mods.getIdentifier())) {
            result.getValidationResults().add(new ValidationResult(item.getPid(), "Objekt nemá validní identifikátor URN:NBN.", Level.SEVERE));
        }
    }

    private void processValidationException(String pid, DigitalObjectValidationException exeption, Result result) {
        String message = null;
        Level level = Level.SEVERE;
        if (exeption != null && exeption.getValidations() != null) {
            for (DigitalObjectValidationException.ValidationResult validationResult : exeption.getValidations()) {
                if (validationResult.getBundleKey().equals(ModsRules.ERR_NDK_SUPPLEMENT_GENRE_TYPE)) {
                    message = String.format("Špatná hodnota v atributu elementu \"Genre[@type]\". Očekávána hodnota \"type={0}\", ale nalezeno \"type={1}\".", validationResult.getValues());
                } else if (validationResult.getBundleKey().equals(ModsRules.ERR_NDK_PHYSICALLOCATION_SIGLA)) {
                    message = String.format("Špatná hodnota v elementu \"Location/PhysicalLocation\". V konfiguraci není hodnota \"{0}\" povolena.", validationResult.getValues());
                } else if (validationResult.getBundleKey().equals(ModsRules.ERR_NDK_RELATEDITEM_PHYSICALLOCATION_SIGLA)) {
                    message = String.format("Špatná hodnota v elementu \"RelatedItem/Location/PhysicalLocation\". V konfiguraci není hodnota \"{0}\" povolena.", validationResult.getValues());
                } else if (validationResult.getBundleKey().equals(RdaRules.ERR_NDK_RDA_EMPTYEVENTTYPE)) {
                    message = String.format("Pro pravidla \"RDA\" musí atribut \"OriginInfo[@EventType]\" obsahovat alespoň jednou jednu z hodnot (PUBLICATION, PRODUCTION).");
                } else if (validationResult.getBundleKey().equals(RdaRules.ERR_NDK_RDA_EMPTYVALUE)) {
                    message = String.format("Element \"{0}\" musí být prázdný, pokud \"OriginInfo[@EventType]\": \"{1}\"!", validationResult.getValues());
                } else if (validationResult.getBundleKey().equals(RdaRules.ERR_NDK_RDA_FILLVALUE)) {
                    message = String.format("Element \"{0}\" nesmí být prázdný, pokud \"OriginInfo[@EventType]\": \"{1}\"!", validationResult.getValues());
                } else if (validationResult.getBundleKey().equals(RdaRules.ERR_NDK_DESCRIPTIONSTANDARD)) {
                    message = String.format("Špatná hodnota v elmentu \"RecordInfo/DescriptionStandard\".");
                } else if (validationResult.getBundleKey().equals(RdaRules.ERR_NDK_AACR_EMPTYVALUE)) {
                    message = String.format("Atribut \"OriginInfo[@EventType] musí být prázdný, pokud element \"RecordInfo/descriptionStandard\" = \"AACR\".", validationResult.getValues());
                } else if (validationResult.getBundleKey().equals(RdaRules.ERR_NDK_AACR_INVALIDVALUE)) {
                    message = String.format("Špatná hodnota v elementu \"Physical Description\".");
                } else if (validationResult.getBundleKey().equals(RdaRules.ERR_NDK_ORIGININFO_EVENTTYPE_WRONGVALUE)) {
                    message = String.format("Špatná hodnota v atributu \"OriginInfo[@EventType]\": \"{0}\"!", validationResult.getValues());
                } else if (validationResult.getBundleKey().equals(ModsRules.ERR_NDK_PHYSICALLOCATION_MULTIPLE)) {
                    message = String.format("Vícenásobný výskyt elementu \"Location\".");
                    level = Level.WARNING;
                }
                if (message != null) {
                    result.getValidationResults().add(new ValidationResult(pid, message, level));
                    message = null;
                }
            }
        }
        exeption.getValidations().clear();
    }

    private ModsDefinition getMods(String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fo = dom.find(pid, null);
        XmlStreamEditor streamEditorOld = fo.getEditor(FoxmlUtils.inlineProfile(
                    MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditorOld = new ModsStreamEditor(streamEditorOld, fo);
        return modsStreamEditorOld.read();
    }

    private void validatePages(SearchViewItem item, List<SearchViewItem> children, Result result, Type type) {
        int pageCount = 0;
        int bdmArticleCount = 0;
        for (SearchViewItem child : children) {
            if (PAGE_MODELS.contains(child.getModel())) {
                pageCount++;
            }
            if (BornDigitalModsPlugin.MODEL_ARTICLE.equals(child.getModel())) {
                bdmArticleCount++;
            }
        }
        if (pageCount < 1) {
            if (Type.EXPORT_ARCHIVE.equals(type) || Type.EXPORT_NDK.equals(type)) {
                result.getValidationResults().add(new ValidationResult(item.getPid(), "Objekt neobsahuje žádnou stranu.", Level.SEVERE));
            } else if (Type.VALIDATION.equals(type) && (!(bdmArticleCount > 0 && NdkPlugin.MODEL_PERIODICALISSUE.equals(item.getModel())))) {
                result.getValidationResults().add(new ValidationResult(item.getPid(), "Objekt neobsahuje žádnou stranu.", Level.SEVERE));
            }
        } else if (pageCount % 2 == 1) {
            result.getValidationResults().add(new ValidationResult(item.getPid(), "Objekt obsahuje lichý počet stran.", Level.WARNING));
        }
    }

    private void validateMusic(SearchViewItem item, List<SearchViewItem> children, Result result) {
        int audioPageCount = 0;
        if (NdkAudioPlugin.MODEL_TRACK.equals(item.getModel())) {
            for (SearchViewItem child : children) {
                if (NdkAudioPlugin.MODEL_PAGE.equals(child.getModel())) {
                    audioPageCount++;
                }
            }
            if (audioPageCount > 1) {
                result.getValidationResults().add(new ValidationResult(item.getPid(), "Objekt obsahuje více zvukových nahrávek.", Level.SEVERE));
            }
        } else if (NdkAudioPlugin.MODEL_SONG.equals(item.getModel())) {
            for (SearchViewItem child : children) {
                if (NdkAudioPlugin.MODEL_PAGE.equals(child.getModel())) {
                    audioPageCount++;
                }
            }
            if (audioPageCount > 1) {
                result.getValidationResults().add(new ValidationResult(item.getPid(), "Objekt obsahuje více zvukových nahrávek.", Level.SEVERE));
            }
        }
    }

    private void validateContainsPdf(AkubraStorage.AkubraObject object, boolean hasChilren, Result result) {
        try {
            if (hasChilren) {
                return;
            }
            if (!AkubraUtils.containsDatastream(object, BinaryEditor.RAW_ID)) {
                result.getValidationResults().add(new ValidationResult(object.getPid(), "Objekt neobsahuje RAW stream.", Level.SEVERE));
            }
        } catch (JAXBException e) {
            result.getValidationResults().add(new ValidationResult(object.getPid(), "Nepodařilo se načíst RAW stream.", Level.SEVERE));
        }
    }

    private void validatePage(SearchViewItem item, Result result) {
        if (item.getPageIndex() == null || item.getPageIndex().isEmpty()) {
            result.getValidationResults().add(new ValidationResult(item.getPid(), "Není vyplněný index strany.", Level.SEVERE));
        } else {
            String pageIndex = item.getPageIndex();
            if (String.valueOf(indexPageValue).equals(pageIndex)) {
                indexPageValue++;
            } else {
                result.getValidationResults().add(new ValidationResult(item.getPid(), "Neočekávaný index=" + pageIndex + ", očekávaná hodnota=" + String.valueOf(indexPageValue) + ".", Level.SEVERE));
            }
        }
        if (item.getPageNumber() == null || item.getPageNumber().isEmpty()) {
            result.getValidationResults().add(new ValidationResult(item.getPid(), "Není vyplněný číslo strany.", Level.SEVERE));
        }
        if (item.getPageType() == null || item.getPageType().isEmpty()) {
            result.getValidationResults().add(new ValidationResult(item.getPid(), "Není vyplněný typ strany.", Level.SEVERE));
        }
        if (item.getPageRepre() != null && !item.getPageRepre().isEmpty()) {
            if ("reprePage".equals(item.getPageRepre())) {
                if (reprePageValue) {
                    result.getValidationResults().add(new ValidationResult(item.getPid(), "Vícenásobný výskyt reprezentativní strany.", Level.SEVERE));
                } else {
                    reprePageValue = true;
                }
            }
        }
        if (item.getPagePosition() != null && !item.getPagePosition().isEmpty()) {
            if (positionPageValue == null) {
                positionPageValue = item.getPagePosition();
            } else if (positionPageValue.equals(item.getPagePosition())) {
                result.getValidationResults().add(new ValidationResult(item.getPid(), "Opakující se pozice strany.", Level.SEVERE));
            } else {
                positionPageValue = item.getPagePosition();
            }
        }
    }

    public static class Result {
        private List<ValidationResult> validationResults;

        public List<ValidationResult> getValidationResults() {
            if (validationResults == null) {
                validationResults = new ArrayList<>();
            }
            return validationResults;
        }

        public void setValidationResults(List<ValidationResult> validationResults) {
            this.validationResults = validationResults;
        }

        public Boolean isStatusOk(boolean ignoreWarning) {
            if (getValidationResults().isEmpty()) {
                return true;
            }
            for (ValidationResult validationResult : getValidationResults()) {
                if (Level.SEVERE.equals(validationResult.getLevel())) {
                    return false;
                }
                if (Level.WARNING.equals(validationResult.getLevel()) && !ignoreWarning) {
                    return false;
                }
            }
            return true;
        }

        public String getMessages() {
            StringBuilder sb = new StringBuilder();
            for (ValidationResult result : validationResults) {
                LOG.log(result.getLevel(), result.getMessage(), result.getMessage());
                sb.append(result.toString()).append(System.lineSeparator());
            }
            return sb.toString();
        }
    }

    private static class ValidationResult {

        private String pid;
        private String message;
        private Level level;
        private Exception ex;

        public ValidationResult(String pid, String message, Level level) {
            this(pid, message, level, null);
        }
        public ValidationResult(String pid, String message, Level level, Exception ex) {
            this.pid = pid;
            this.ex = ex;
            this.level = level;
            this.message = message;

        }

        public String getPid() {
            return pid;
        }

        public String getMessage() {
            return message;
        }

        public Level getLevel() {
            return level;
        }

        public Exception getEx() {
            return ex;
        }

        @Override
        public String toString() {
            String level = "Info";
            if (Level.SEVERE.equals(getLevel())) {
                level = "Chyba";
            } else if (Level.WARNING.equals(getLevel())) {
                level = "Upozornění";
            }
            return level + " (" + getPid() + ") " + getMessage();
        }
    }
}
