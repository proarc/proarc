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
package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem.FileState;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.chronicle.ChronicleMapperFactory;
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
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import static cz.cas.lib.proarc.common.imports.ImportProcess.getConsumers;

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
    public int estimateItemNumber(ImportOptions importConfig) throws IOException {
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
    public void start(ImportOptions importConfig, ImportBatchManager batchManager) throws Exception {
        File importFolder = importConfig.getImportFolder();
        Batch batch = importConfig.getBatch();
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

    private String createObject(List<FileSet> fileSets, String model, ImportBatchManager batchManager, ImportOptions importConfig) throws DigitalObjectException, JAXBException {
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
        FedoraObject fo = dom.find(pid, null);
        XmlStreamEditor streamEditor = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStream = new ModsStreamEditor(streamEditor, fo);
        ModsDefinition mods = modsStream.read();
        //repair Mods
        repairMods(chronicle, archiv, mods);
        modsStream.write(mods, modsStream.getLastModified(), null);

        //repair DcDatastream
        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
        ChronicleMapperFactory mapperFactory = new ChronicleMapperFactory();
        NdkMapper mapper = mapperFactory.get(ChroniclePlugin.MODEL_CHRONICLEVOLUME);
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

        FormDefinition form = new FormDefinition();
        form.setType("PocetSnimku");
        form.setValue(chronicle.getPocetSnimku());
        PhysicalDescriptionDefinition description = new PhysicalDescriptionDefinition();
        description.getForm().add(form);

        NoteDefinition note = new NoteDefinition();
        note.setValue(chronicle.getPoznamka());
        mods.getNote().add(note);
    }

    private String getFileName(List<FileSet> fileSets) {
        return fileSets.get(0).getName();
    }

    private ImportCatalog createCatalog(ImportBatchManager batchManager, ImportOptions importConfig) throws JAXBException {
        File catalogFile = new File(batchManager.getAppConfig().getConfigHome().toURI().resolve(importConfig.getConfig().getDefaultCatalog()));
        JAXBContext jaxbContext = JAXBContext.newInstance(ImportCatalog.class);
        Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
        return (ImportCatalog) unmarshaller.unmarshal(catalogFile);
    }

    private ImportArchiveCatalog createArchiveCatalog(ImportBatchManager batchManager, ImportOptions importConfig) throws JAXBException {
        File catalogFile = new File(batchManager.getAppConfig().getConfigHome().toURI().resolve(importConfig.getConfig().getArchiveCatalog()));
        JAXBContext jaxbContext = JAXBContext.newInstance(ImportArchiveCatalog.class);
        Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
        return (ImportArchiveCatalog) unmarshaller.unmarshal(catalogFile);
    }

    private void consumeFileSets(Batch batch, List<FileSet> fileSets, ImportOptions ctx) throws InterruptedException {
        ImportBatchManager batchManager = ImportBatchManager.getInstance();
        long start = System.currentTimeMillis();
        for (FileSet fileSet : fileSets) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            BatchItemObject item = consumeFileSet(fileSet, ctx);
            String pid = item == null ? null : item.getPid();
            FileState state = item == null ? FileState.SKIPPED : FileState.OK;
            batchManager.addFileItem(batch.getId(), pid, state, fileSet.getFiles());
            if (item != null) {
                if (ObjectState.LOADING_FAILED == item.getState()) {
                    batch.setState(Batch.State.LOADING_FAILED);
                    batch.setLog(item.getFile() + "\n" + item.getLog());
                    return ;
                }
            }
        }
        LOG.log(Level.FINE, "Total time: {0} ms", System.currentTimeMillis() - start);
    }

    private BatchItemObject consumeFileSet(FileSet fileSet, ImportOptions ctx) {
        long start = System.currentTimeMillis();
        List<ImageImporter> consumers = getConsumers();
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
