/*
 * Copyright (C) 2016 Jan Pokorsky
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
package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchItem.FileState;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.process.BatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.chronicle.ChroniclePlugin;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.cas.lib.proarc.mods.SubjectDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.File;
import java.io.IOException;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

/**
 * Imports files grouped to {@link FileSet file sets}.
 *
 * @author Jan Pokorsky
 */
public class FileSetImport implements ImportHandler {

    private static final Logger LOG = Logger.getLogger(FileSetImport.class.getName());

    @Override
    public boolean isImportable(File folder) {
        String[] fileNames = folder.list();
        for (String fileName : fileNames) {
            if (ImportFileScanner.IMPORT_STATE_FILENAME.equals(fileName)) {
                continue;
            }
            File file = new File(folder, fileName);
            if (file.isFile() && file.canRead()) {
                List<FileSet> fileSets = ImportFileScanner.getFileSets(Arrays.asList(file));
                if (canImport(fileSets.get(0))) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public int estimateItemNumber(ImportProcess.ImportOptions importConfig) throws IOException {
        File importFolder = importConfig.getImportFolder();
        ImportFileScanner scanner = new ImportFileScanner();
        List<File> files = scanner.findDigitalContent(importFolder);
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        if (!canImport(fileSets)) {
            // nothing to import
            return 0;
        } else {
            return fileSets.size();
        }
    }

    @Override
    public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration configuration) throws Exception {
        File importFolder = importConfig.getImportFolder();
        Batch batch = importConfig.getBatch();
        batch.setState(Batch.State.LOADING);
        batch.setUpdated(new Timestamp(System.currentTimeMillis()));
        batch = batchManager.update(batch);
        ImportFileScanner scanner = new ImportFileScanner();
        List<File> files = scanner.findDigitalContent(importFolder);
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        importConfig.setJhoveContext(JhoveUtility.createContext());
        try {
            consumeFileSets(batch, fileSets, importConfig);
            if(batch.getState().equals(Batch.State.LOADING)
                    && "profile.chronicle".equals(importConfig.getConfig().getProfileId())
                    && importConfig.getConfig().getCreateModelsHierarchy()) {
                String pid = createObject(fileSets, ChroniclePlugin.MODEL_CHRONICLEVOLUME, batchManager, importConfig);
                batch.setParentPid(pid);
                batchManager.update(batch);
            }
        } finally {
            importConfig.getJhoveContext().destroy();
        }
    }

    private String createObject(List<FileSet> fileSets, String model, BatchManager batchManager, ImportProcess.ImportOptions importConfig) throws DigitalObjectException, JAXBException {
        String pid = FoxmlUtils.createPid();
        DigitalObjectManager dom  = DigitalObjectManager.getDefault();
        DigitalObjectManager.CreateHandler handler = dom.create(model, pid, null, importConfig.getUser(), null, "create new object with pid: " + pid);
        handler.create();

        ImportCatalog catalog = createCatalog(batchManager, importConfig);
        ImportArchiveCatalog archiveCatalog = createArchiveCatalog(batchManager, importConfig);
        String fileName = getFileName(fileSets);
        String[] fileNameSplit = fileName.split(" ");
        String[] name = fileNameSplit[0].split("_");
        String id = "";
        String id2 = "";
        if (name.length > 3) {
            id = name[0] + "_" + name[1] + "_" + name[2];
            id2 = name[0] + "_" + name[1] + "_ic" + name[2];
        } else {
            id = fileName;
            id2 = fileName;
        }
        ImportArchiveCatalog.Archiv archiv = getArchiveName(archiveCatalog, name[0], name[1]);
        for (ImportCatalog.Kronika chronicle : catalog.getKronika()) {
            if (chronicle.getLocalId() != null && id != null && id2 != null) {
                if (chronicle.getLocalId().contains(id) || chronicle.getLocalId().contains(id2)) {
                    fillMetadata(chronicle, archiv, pid);
                    break;
                }
            }
        }
        return pid;
    }

    private ImportArchiveCatalog.Archiv getArchiveName(ImportArchiveCatalog archiveCatalog, String organization, String name) {
        String id = organization + "_" + name;
        for (ImportArchiveCatalog.Archiv archiv : archiveCatalog.getArchivy()) {
            if (id.equalsIgnoreCase(archiv.getId())) {
                return archiv;
            }
        }
        return null;
    }

    private void fillMetadata(ImportCatalog.Kronika chronicle, ImportArchiveCatalog.Archiv archiv, String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fo = dom.find(pid, null);
        XmlStreamEditor streamEditor = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStream = new ModsStreamEditor(streamEditor, fo);
        ModsDefinition mods = modsStream.read();
        //repair Mods
        repairMods(chronicle, archiv, mods);
        modsStream.write(mods, modsStream.getLastModified(), null);

        //repair DcDatastream
        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
        NdkMapper mapper = NdkMapper.get(ChroniclePlugin.MODEL_CHRONICLEVOLUME);
        mapper.setModelId(ChroniclePlugin.MODEL_CHRONICLEVOLUME);
        NdkMapper.Context context = new NdkMapper.Context(handler);
        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = handler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
        dcr.setDc(dc);
        dcEditor.write(handler, dcr, null);

        //repair Label
        String label = mapper.toLabel(mods);
        fo.setLabel(label);
        fo.flush();
    }

    private void repairMods(ImportCatalog.Kronika chronicle, ImportArchiveCatalog.Archiv archiv, ModsDefinition mods) {
        StringPlusLanguage title = new StringPlusLanguage();
        title.setValue(chronicle.getNazev());
        TitleInfoDefinition titleInfo = new TitleInfoDefinition();
        titleInfo.getTitle().add(title);
        mods.getTitleInfo().add(titleInfo);

        IdentifierDefinition identifierLocalId = new IdentifierDefinition();
        identifierLocalId.setType("localId");
        identifierLocalId.setValue(chronicle.getLocalId());
        mods.getIdentifier().add(identifierLocalId);

        IdentifierDefinition identifierId = new IdentifierDefinition();
        identifierId.setType("id");
        identifierId.setValue(chronicle.getId());
        mods.getIdentifier().add(identifierId);

        DateDefinition dateCreated = new DateDefinition();
        //dateCreated.setQualifier("approximate");
        dateCreated.setValue(chronicle.getObdobi());
        OriginInfoDefinition originInfo = new OriginInfoDefinition();
        originInfo.getDateIssued().add(dateCreated);
        mods.getOriginInfo().add(originInfo);

        PhysicalLocationDefinition physicalLocation = new PhysicalLocationDefinition();
        physicalLocation.setType(chronicle.getMistoUlozeni());
        if (archiv != null) {
            physicalLocation.setValue(archiv.getId());
            physicalLocation.setDisplayLabel(archiv.getValue());
        }
        LocationDefinition location = new LocationDefinition();
        location.getPhysicalLocation().add(physicalLocation);
        mods.getLocation().add(location);

        setGeoStorage(mods, chronicle.getMistoUlozeni());

        FormDefinition form = new FormDefinition();
        form.setType("PocetSnimku");
        form.setValue(chronicle.getPocetSnimku());
        PhysicalDescriptionDefinition description = new PhysicalDescriptionDefinition();
        description.getForm().add(form);

        NoteDefinition note = new NoteDefinition();
        note.setValue(chronicle.getPoznamka());
        mods.getNote().add(note);
    }

    private void setGeoStorage(ModsDefinition mods, String mistoUlozeni) {
        switch (mistoUlozeni) {
            case "226102010":       // SOkA Jihlava
                setGeoStorageValue(mods, "Česká republika","Jihovýchod",
                        "Kraj Vysočina","Jihlava",
                        "Jihomoravský","Jihlava",
                        "Jihlava","Jihlava",
                        "Jihlava","Fritzova",
                        "Fritzova 4800/19, 58601 Jihlava","1",
                        "60","108",
                        "3707","37",
                        "1503","3018",
                        "586846","412317",
                        "170534","25038184");
                break;
            case "226103010":       // SOkA Pelhrimov
                setGeoStorageValue(mods, "Česká republika","Jihovýchod",
                        "Kraj Vysočina","Pelhřimov",
                        "Jihočeský","Pelhřimov",
                        "Pelhřimov","Pelhřimov",
                        "Pelhřimov","Pražská",
                        "Pražská 1883, 39301 Pelhřimov","1",
                        "60","108",
                        "3304","33",
                        "388","809",
                        "547492","404292",
                        "381462","20147929");
                break;
            case "226101010":       // SOkA Havlickuv Brod
                setGeoStorageValue(mods, "Česká republika","Jihovýchod",
                        "Kraj Vysočina","Havlíčkův Brod",
                        "Východočeský","Havlíčkův Brod",
                        "Havlíčkův Brod","Havlíčkův Brod",
                        "Havlíčkův Brod","Kyjovská",
                        "Kyjovská 1125, 58001 Havlíčkův Brod","1",
                        "60","108",
                        "3601","36",
                        "949","1996",
                        "568414","409472",
                        "111783","9599100");
                break;
            case "226104010":       //SOkA Trebic
                setGeoStorageValue(mods, "Česká republika", "Jihovýchod",
                        "Kraj Vysočina", "Třebíč",
                        "Jihomoravský", "Třebíč",
                        "Třebíč", "Třebíč",
                        "Horka-Domky", "Na Potoce",
                        "Na Potoce 60/23, Horka-Domky, 67401 Třebíč", "1",
                        "60", "108",
                        "3710", "37",
                        "1597", "3204",
                        "590266", "412538",
                        "603554", "18772595");
                break;
            case "226105010":       // SOkA Zdar nad Sazavou
                setGeoStorageValue(mods, "Česká republika", "Jihovýchod",
                        "Kraj Vysočina", "Žďár nad Sázavou",
                        "Jihomoravský", "Žďár nad Sázavou",
                        "Žďár nad Sázavou", "Žďár nad Sázavou",
                        "Žďár nad Sázavou 1", "U Malého lesa",
                        "U Malého lesa 1445/4, Žďár nad Sázavou 1, 59101 Žďár nad Sázavou", "1",
                        "60", "108",
                        "3714", "37",
                        "1708", "3417",
                        "595209", "412805",
                        "656097", "3401880");
                break;
        }
    }

    private void setGeoStorageValue(ModsDefinition mods, String name_stat, String name_kraj_1960,
                                    String name_region_soudrznosti, String name_okres, String name_vusc,
                                    String name_orp, String name_pou, String name_obec, String name_cast_obce,
                                    String name_ulice, String name_adresni_misto, String code_stat,
                                    String code_kraj_1960, String code_region_soudrznosti, String code_okres,
                                    String code_vusc, String code_orp, String code_pou, String code_obec,
                                    String code_cast_obce, String code_ulice, String code_adresni_misto) {
        SubjectDefinition subject = new SubjectDefinition();
        subject.setAuthority("geo:storage");
        mods.getSubject().add(subject);

        subject.getGeographic().add(setGeographic("RUIAN_NAME:STAT", name_stat));
        subject.getGeographic().add(setGeographic("RUIAN_NAME:KRAJ_1960", name_kraj_1960));
        subject.getGeographic().add(setGeographic("RUIAN_NAME:REGION_SOUDRZNOSTI", name_region_soudrznosti));
        subject.getGeographic().add(setGeographic("RUIAN_NAME:OKRES", name_okres));
        subject.getGeographic().add(setGeographic("RUIAN_NAME:VUSC", name_vusc));
        subject.getGeographic().add(setGeographic("RUIAN_NAME:ORP", name_orp));
        subject.getGeographic().add(setGeographic("RUIAN_NAME:POU", name_pou));
        subject.getGeographic().add(setGeographic("RUIAN_NAME:OBEC", name_obec));
        subject.getGeographic().add(setGeographic("RUIAN_NAME:CAST_OBCE", name_cast_obce));
        subject.getGeographic().add(setGeographic("RUIAN_NAME:ULICE", name_ulice));
        subject.getGeographic().add(setGeographic("RUIAN_NAME:ADRESNI_MISTO", name_adresni_misto));

        subject.getGeographic().add(setGeographic("RUIAN_CODE:STAT", code_stat));
        subject.getGeographic().add(setGeographic("RUIAN_CODE:KRAJ_1960", code_kraj_1960));
        subject.getGeographic().add(setGeographic("RUIAN_CODE:REGION_SOUDRZNOSTI", code_region_soudrznosti));
        subject.getGeographic().add(setGeographic("RUIAN_CODE:OKRES", code_okres));
        subject.getGeographic().add(setGeographic("RUIAN_CODE:VUSC", code_vusc));
        subject.getGeographic().add(setGeographic("RUIAN_CODE:ORP", code_orp));
        subject.getGeographic().add(setGeographic("RUIAN_CODE:POU", code_pou));
        subject.getGeographic().add(setGeographic("RUIAN_CODE:OBEC", code_obec));
        subject.getGeographic().add(setGeographic("RUIAN_CODE:CAST_OBCE", code_cast_obce));
        subject.getGeographic().add(setGeographic("RUIAN_CODE:ULICE", code_ulice));
        subject.getGeographic().add(setGeographic("RUIAN_CODE:ADRESNI_MISTO", code_adresni_misto));
    }

    private StringPlusLanguagePlusAuthority setGeographic(String authority, String value) {
        StringPlusLanguagePlusAuthority geographic = new StringPlusLanguagePlusAuthority();
        geographic.setAuthority(authority);
        geographic.setValue(value);
        return geographic;
    }

    private void setGeoStorageValue(ModsDefinition mods) {
    }

    private String getFileName(List<FileSet> fileSets) {
        return fileSets.get(0).getName();
    }

    private ImportCatalog createCatalog(BatchManager batchManager, ImportProcess.ImportOptions importConfig) throws JAXBException {
        File catalogFile = new File(batchManager.getAppConfig().getConfigHome().toURI().resolve(importConfig.getConfig().getDefaultCatalog()));
        JAXBContext jaxbContext = JAXBContext.newInstance(ImportCatalog.class);
        Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
        return (ImportCatalog) unmarshaller.unmarshal(catalogFile);
    }

    private ImportArchiveCatalog createArchiveCatalog(BatchManager batchManager, ImportProcess.ImportOptions importConfig) throws JAXBException {
        File catalogFile = new File(batchManager.getAppConfig().getConfigHome().toURI().resolve(importConfig.getConfig().getArchiveCatalog()));
        JAXBContext jaxbContext = JAXBContext.newInstance(ImportArchiveCatalog.class);
        Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
        return (ImportArchiveCatalog) unmarshaller.unmarshal(catalogFile);
    }

    protected void consumeFileSets(Batch batch, List<FileSet> fileSets, ImportProcess.ImportOptions ctx) throws InterruptedException {
        BatchManager batchManager = BatchManager.getInstance();
        long start = System.currentTimeMillis();
        for (FileSet fileSet : fileSets) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            List<BatchItem> existingBatchItems = batchManager.findBatchItemObject(ctx, fileSet.getName());
            if (!exists(existingBatchItems)) {
                BatchItemObject item = consumeFileSet(fileSet, ctx);
                String pid = item == null ? null : item.getPid();
                FileState state = item == null ? FileState.SKIPPED : FileState.OK;
                batchManager.addFileItem(batch.getId(), pid, state, fileSet.getFiles());
                if (item != null) {
                    if (ObjectState.LOADING_FAILED == item.getState()) {
                        batch.setState(Batch.State.LOADING_FAILED);
                        batch.setUpdated(new Timestamp(System.currentTimeMillis()));
                        batch.setLog(item.getFile() + "\n" + item.getLog());
                        return;
                    }
                }
            }
        }
        LOG.log(Level.FINE, "Total time: {0} ms", System.currentTimeMillis() - start);
    }

    private boolean exists(List<BatchItem> batchItemObject) {
        return batchItemObject != null && batchItemObject.size() > 0;
    }

    private BatchItemObject consumeFileSet(FileSet fileSet, ImportProcess.ImportOptions ctx) {
        long start = System.currentTimeMillis();
        List<ImageImporter> consumers = ImportProcess.getConsumers();
        for (ImageImporter consumer : consumers) {
            BatchItemObject item = consumer.consume(fileSet, ctx);
            if (item != null) {
                LOG.log(Level.FINE, "time: {0} ms, {1}", new Object[] {System.currentTimeMillis() - start, fileSet});
                ctx.setConsumedFileCounter(ctx.getConsumedFileCounter() + 1);
                return item;
            }
        }

        return null;
    }

    static boolean canImport(FileSet fileSet) {
        for (ImageImporter consumer : ImportProcess.getConsumers()) {
            if (consumer.accept(fileSet)) {
                return true;
            }
        }
        return false;
    }

    static boolean canImport(List<FileSet> fileSets) {
        for (FileSet fileSet : fileSets) {
            if (canImport(fileSet)) {
                return true;
            }
        }
        return false;
    }

}
