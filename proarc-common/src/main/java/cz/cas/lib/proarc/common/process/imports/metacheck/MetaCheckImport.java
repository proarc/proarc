package cz.cas.lib.proarc.common.process.imports.metacheck;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.image.ImageMimeType;
import cz.cas.lib.proarc.common.metacheck.MetaCheckBatch;
import cz.cas.lib.proarc.common.metacheck.MetaCheckClient;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.external.ExternalProcess;
import cz.cas.lib.proarc.common.process.external.PeroOcrProcessor;
import cz.cas.lib.proarc.common.process.external.TiffToJpgConvert;
import cz.cas.lib.proarc.common.process.export.Kramerius4ExportOptions;
import cz.cas.lib.proarc.common.process.imports.FileSet;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner;
import cz.cas.lib.proarc.common.process.imports.ImportHandler;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import cz.cas.lib.proarc.common.process.imports.ImportProfile;
import cz.cas.lib.proarc.common.process.imports.InputUtils;
import cz.cas.lib.proarc.common.process.imports.TiffImporter;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import static cz.cas.lib.proarc.common.image.ImageUtility.readImage;

public class MetaCheckImport implements ImportHandler {

    private static final Logger LOG = Logger.getLogger(MetaCheckImport.class.getName());

    @Override
    public int estimateItemNumber(ImportProcess.ImportOptions importConfig) throws IOException {
        ImportFileScanner scanner = new ImportFileScanner();
        List<File> files = scanner.findDigitalContent(importConfig.getImportFolder());
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        TiffImporter tiffImporter = new TiffImporter(BatchManager.getInstance());
        int count = 0;
        for (FileSet fileSet : fileSets) {
            if (tiffImporter.accept(fileSet)) {
                count++;
            }
        }
        return count;
    }

    @Override
    public boolean isImportable(File folder) {
        String[] fileNames = folder.list();
        TiffImporter tiffImporter = new TiffImporter(BatchManager.getInstance());
        for (String fileName : fileNames) {
            if (ImportFileScanner.IMPORT_STATE_FILENAME.equals(fileName)) {
                continue;
            }
            File file = new File(folder, fileName);
            if (file.isFile() && file.canRead()) {
                List<FileSet> fileSets = ImportFileScanner.getFileSets(Arrays.asList(file));
                if (tiffImporter.accept(fileSets.get(0))) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {
        Batch batch = importConfig.getBatch();
        batch.setState(Batch.State.LOADING);
        batch.setUpdated(new Timestamp(System.currentTimeMillis()));
        batch = batchManager.update(batch);

        ImportFileScanner scanner = new ImportFileScanner();
        List<File> files = scanner.findDigitalContent(importConfig.getImportFolder());
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        TiffImporter tiffImporter = new TiffImporter(batchManager);

        for (FileSet fileSet : fileSets) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            if (!tiffImporter.accept(fileSet)) {
                continue;
            }

            File tiff = findTiff(fileSet);
            if (tiff == null) {
                continue;
            }
            if (!InputUtils.isTiff(tiff)) {
                throw new IllegalStateException("Not a TIFF content: " + tiff);
            }

            File fullJpg = generateFullJpg(fileSet, tiff, importConfig);
            generateOcrAndAlto(tiff, fullJpg, importConfig);

            batchManager.addFileItem(batch.getId(), null, BatchItem.FileState.OK, fileSet.getFiles());
            importConfig.setConsumedFileCounter(importConfig.getConsumedFileCounter() + 1);
        }

        createPackageInfo(importConfig, config);
        callMetaCheckApi(importConfig, config);

        if (batch.getState() == Batch.State.LOADING) {
            batch.setState(Batch.State.EXTERNAL_EDITING);
            batch.setUpdated(new Timestamp(System.currentTimeMillis()));
        }
        batchManager.update(batch);
    }

    private File findTiff(FileSet fileSet) {
        for (FileSet.FileEntry entry : fileSet.getFiles()) {
            if (ImageMimeType.TIFF.getMimeType().equals(entry.getMimetype())) {
                return entry.getFile();
            }
        }
        return null;
    }

    private File generateFullJpg(FileSet fileSet, File tiff, ImportProcess.ImportOptions importConfig) throws Exception {
        ImportProfile profile = importConfig.getConfig();
        File fullJpg = new File(importConfig.getTargetFolder(), fileSet.getName() + profile.getNdkFullFileSuffix());
        if (fullJpg.exists() && InputUtils.isJpeg(fullJpg)) {
            LOG.log(Level.FINE, "Skipping FULL JPG generation, file exists: {0}", fullJpg);
            return fullJpg;
        }

        if (profile.isTiffToJpgDefined()) {
            ExternalProcess process = new TiffToJpgConvert(profile.getConvertorTiffToJpgProcessor(), tiff, fullJpg);
            process.run();
            if (!process.isOk()) {
                throw new IllegalStateException("Converting tiff to FULL jpg failed: " + process.getFullOutput());
            }
        } else {
            TiffImporter.writeImage(
                    TiffImporter.removeAlphaChannel(readImage(tiff.toURI().toURL(), ImageMimeType.TIFF)),
                    importConfig.getTargetFolder(),
                    fullJpg.getName(),
                    ImageMimeType.JPEG);
        }

        if (!InputUtils.isJpeg(fullJpg)) {
            throw new IllegalStateException("Not a JPEG content: " + fullJpg);
        }
        return fullJpg;
    }

    private File[] generateOcrAndAlto(File tiff, File fullJpg, ImportProcess.ImportOptions importConfig) throws IOException {
        ImportProfile profile = importConfig.getConfig();
        File[] ocrAltoFiles = getOcrAltoFiles(tiff, profile);
        if (ocrAltoFiles[0].exists() && ocrAltoFiles[1].exists()) {
            LOG.log(Level.FINE, "Skipping OCR/ALTO generation, files exist for: {0}", tiff);
            return ocrAltoFiles;
        }

        Integer peroOcrEngine = getPeroOcrEngine(importConfig);
        PeroOcrProcessor ocrProcessor = new PeroOcrProcessor(profile.getOcrGenProcessor(), peroOcrEngine);
        try {
            boolean processed = ocrProcessor.process(
                    fullJpg.getAbsolutePath(),
                    ocrAltoFiles[0].getAbsolutePath(),
                    ocrAltoFiles[1].getAbsolutePath());
            if (processed) {
                LOG.info("OCR GENERATED SUCCESSFULLY for " + fullJpg.getAbsolutePath());
            }
        } catch (JSONException ex) {
            LOG.log(Level.SEVERE, "Generating OCR for " + fullJpg.getName() + " failed.", ex);
            throw new IOException(ex);
        }
        if (!ocrAltoFiles[0].exists() || !ocrAltoFiles[1].exists()) {
            throw new IOException("Generating OCR/ALTO failed for " + fullJpg.getAbsolutePath());
        }
        return ocrAltoFiles;
    }

    private File[] getOcrAltoFiles(File tiff, ImportProfile profile) {
        String tiffPath = tiff.getAbsolutePath();
        String basePath = tiffPath.substring(0, tiffPath.lastIndexOf('.'));
        File ocr = new File(basePath + profile.getPlainOcrFileSuffix());
        File alto = new File(basePath + profile.getAltoFileSuffix());
        return new File[]{ocr, alto};
    }

    private Integer getPeroOcrEngine(ImportProcess.ImportOptions importConfig) {
        try {
            Integer peroOcrEngine = importConfig.getBatch().getParamsAsObject().getPeroOcrEngine();
            if (peroOcrEngine == null || peroOcrEngine < 0) {
                return 1;
            }
            return peroOcrEngine;
        } catch (NullPointerException ex) {
            return 1;
        }
    }

    void createPackageInfo(ImportProcess.ImportOptions importConfig, AppConfiguration config) throws Exception {
        List<String> selectedPids = getSelectedPids(importConfig);
        if (selectedPids == null || selectedPids.isEmpty()) {
            LOG.warning("No selected PIDs found. Skipping packageInfo.json generation.");
            return;
        }

        DigitalObjectManager dom = getDigitalObjectManager();
        if (dom == null) {
            throw new IllegalStateException("DigitalObjectManager is not initialized.");
        }

        Set<String> pids = new LinkedHashSet<>();
        Set<String> visitedPids = new HashSet<>();
        for (String pid : selectedPids) {
            addPackageInfoPids(dom, pid, pids, visitedPids);
        }

        JSONArray objects = new JSONArray();
        String packageType = "other";

        for (String pid : pids) {
            ProArcObject object = dom.find(pid, null);
            DigitalObjectHandler handler = dom.createHandler(object);
            String model = handler.relations().getModel();
            packageType = resolvePackageType(packageType, model);
            DescriptionMetadata<String> metadata = handler.metadata().getMetadataAsXml();

            JSONObject packageObject = new JSONObject();
            packageObject.put("pid", pid);
            packageObject.put("model", toPackageInfoModel(model, config.getKramerius4Export()));
            packageObject.put("metadata", metadata.getData());
            objects.put(packageObject);
        }

        JSONObject packageInfo = new JSONObject();
        packageInfo.put("type", packageType);
        packageInfo.put("objects", objects);

        File packageInfoFile = new File(importConfig.getImportFolder(), "packageInfo.json");
        Files.write(packageInfoFile.toPath(), packageInfo.toString(2).getBytes(StandardCharsets.UTF_8));
    }

    DigitalObjectManager getDigitalObjectManager() {
        return DigitalObjectManager.getDefault();
    }

    private List<String> getSelectedPids(ImportProcess.ImportOptions importConfig) {
        BatchParams params = importConfig.getBatch().getParamsAsObject();
        return params == null ? null : params.getPids();
    }

    private void addPackageInfoPids(DigitalObjectManager dom, String pid, Set<String> pids, Set<String> visitedPids) throws Exception {
        if (pid == null || !visitedPids.add(pid)) {
            return;
        }

        ProArcObject object = dom.find(pid, null);
        DigitalObjectHandler handler = dom.createHandler(object);
        String model = handler.relations().getModel();
        if (!isPageModel(model)) {
            pids.add(pid);
        }
        for (String childPid : handler.relations().getMembers()) {
            addPackageInfoPids(dom, childPid, pids, visitedPids);
        }
    }

    private boolean isPageModel(String model) {
        return model != null && model.toLowerCase(Locale.ROOT).contains("page");
    }

    private String resolvePackageType(String currentPackageType, String model) {
        if (model == null) {
            return currentPackageType;
        }
        String lowerModel = model.toLowerCase(Locale.ROOT);
        if (lowerModel.contains("periodical")) {
            return "periodical";
        }
        if ("other".equals(currentPackageType) && lowerModel.contains("monograph")) {
            return "monograph";
        }
        return currentPackageType;
    }

    private String toPackageInfoModel(String model, Kramerius4ExportOptions kramerius4ExportOptions) {
        Map<String, String> modelMap = kramerius4ExportOptions.getModelMap();
        String mappedModel = modelMap.get(model);
        if (mappedModel == null) {
            mappedModel = model;
        }
        if (mappedModel != null && mappedModel.startsWith("model:")) {
            return mappedModel.substring("model:".length());
        }
        return mappedModel;
    }

    private void callMetaCheckApi(ImportProcess.ImportOptions importConfig, AppConfiguration config) throws IOException, JSONException {
        ImportProfile profile = importConfig.getConfig();
        String apiUrl = profile.getMetaCheckApiUrl();
        if (apiUrl == null || apiUrl.trim().isEmpty()) {
            LOG.warning("MetaCheck API URL is not configured. Skipping MetaCheck API call.");
            return;
        }

        try (MetaCheckClient client = new MetaCheckClient(apiUrl)) {
            Integer engine = getMetaKatEngine(importConfig);
            MetaCheckBatch metaCheckBatch = client.addBatch(
                    getMetaCheckFolder(importConfig.getImportFolder(), config.getConfigHome()),
                    engine,
                    importConfig.getBatch().getId());
            LOG.log(Level.INFO, "MetaCheck batch {0} created for ProArc batch {1}.",
                    new Object[]{metaCheckBatch.getBatchId(), importConfig.getBatch().getId()});
        }
    }

    private Integer getMetaKatEngine(ImportProcess.ImportOptions importConfig) {
        try {
            Integer metakatEngine = importConfig.getBatch().getParamsAsObject().getMetakatEngine();
            if (metakatEngine == null || metakatEngine < 0) {
                return 1;
            }
            return metakatEngine;
        } catch (NullPointerException ex) {
            return 1;
        }
    }

    private String getMetaCheckFolder(File importFolder, File configHome) {
        String path = importFolder.getPath().replace('\\', '/');
        String proarcHome = configHome.getPath().replace('\\', '/');
        if (path.startsWith(proarcHome + "/")) {
            return path.substring(proarcHome.length() + 1);
        }
        return path;
    }
}
