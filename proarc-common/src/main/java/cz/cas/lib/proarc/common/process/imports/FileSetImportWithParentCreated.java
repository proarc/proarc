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
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraImport;
import cz.cas.lib.proarc.common.process.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.Extent;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;
import javax.xml.bind.JAXBException;

/**
 * Imports files grouped to {@link FileSet file sets}.
 *
 * @author Jan Pokorsky
 */
public class FileSetImportWithParentCreated extends FileSetImport {

    private static final Logger LOG = Logger.getLogger(FileSetImportWithParentCreated.class.getName());

    @Override
    public boolean isImportable(File folder) {
        String[] fileNames = folder.list();
        for (String fileName : fileNames) {
            if (ImportFileScanner.IMPORT_STATE_FILENAME.equals(fileName)) {
                continue;
            }
            File file = new File(folder, fileName);
            if (file.isFile() && file.canRead() && isAcceptableFile(file)) {
                return true;
            }
        }
        return false;
    }

    private boolean isAcceptableFile(File file) {
        return file.getName().endsWith(".csv");
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
    public void start(ImportOptions importConfig, BatchManager batchManager, AppConfiguration configuration) throws Exception {
        File importFolder = importConfig.getImportFolder();
        Batch batch = importConfig.getBatch();
        ImportFileScanner scanner = new ImportFileScanner();
        if (importFolder.exists() && importFolder.isDirectory()) {
            List<File> files = scanner.findDigitalContent(importFolder);
            List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
            importConfig.setJhoveContext(JhoveUtility.createContext());
            try {
                consumeFileSets(batch, fileSets, importConfig);
                if(batch.getState().equals(Batch.State.LOADING)
                        && ConfigurationProfile.IMPORT_WITH_CREATION_PARENT.equals(importConfig.getConfig().getProfileId())) {
                    String pid = createObject(fileSets, batch, importConfig);

                    if (pid != null) {
                        batch.setParentPid(pid);
                        batch.setState(Batch.State.LOADED);
                        batchManager.update(batch);
                        if (Storage.FEDORA.equals(configuration.getTypeOfStorage())) {
                            batch = new FedoraImport(configuration, RemoteStorage.getInstance(configuration), batchManager, importConfig.getUser(), importConfig).importBatch(batch, importConfig.getUser().getUserName(), "Importing new object from import");
                        } else if (Storage.AKUBRA.equals(configuration.getTypeOfStorage())) {
                            AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(configuration.getConfigHome());
                            batch = new AkubraImport(configuration, akubraConfiguration, batchManager, importConfig.getUser(), importConfig).importBatch(batch, importConfig.getUser().getUserName(), "Importing new object from import");
                        } else {
                            throw new IllegalStateException("Unsupported type of storage: " + configuration.getTypeOfStorage());
                        }
                    }
                }
            } finally {
                importConfig.getJhoveContext().destroy();
            }
        }
    }

    private String createObject(List<FileSet> fileSets, Batch batch, ImportOptions importConfig) throws DigitalObjectException, JAXBException {
        MetadataCatalog catalog = getMetadataCatalog(importConfig);
        if (catalog.getModel() == null || !isValidModel(catalog.getModel())) {
            return null;
        }

        String pid = FoxmlUtils.createPid();
        DigitalObjectManager dom  = DigitalObjectManager.getDefault();
        DigitalObjectManager.CreateHandler handler = dom.create(catalog.getModel(), pid, catalog.getParent(), importConfig.getUser(), null, "create new object with pid: " + pid);
        handler.create();

        fillMetadata(catalog, pid);

        return pid;
    }

    private boolean isValidModel(String model) {
        try {
            NdkMapper mapper = NdkMapper.get(model);
            if (mapper != null) {
                return true;
            }
            return false;
        } catch (IllegalStateException ex) {
            return false;
        }
    }

    private void fillMetadata(MetadataCatalog metadataCatalog, String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fo = dom.find(pid, null);
        XmlStreamEditor streamEditor = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());

        NdkMapper mapper = NdkMapper.get(metadataCatalog.getModel());
        mapper.setModelId(metadataCatalog.getModel());
        NdkMapper.Context context = new NdkMapper.Context(handler);

        ModsStreamEditor modsStream = new ModsStreamEditor(streamEditor, fo);
        ModsDefinition mods = modsStream.read();
        //repair Mods
        repairMods(metadataCatalog, mods);
        mapper.createMods(mods, context);
        modsStream.write(mods, modsStream.getLastModified(), null);

        //repair DcDatastream


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

    private void repairMods(MetadataCatalog metadataCatalog, ModsDefinition mods) {
        setTitleInfo(metadataCatalog, mods);
        setIdentifier(metadataCatalog, mods);
        setOriginInfo(metadataCatalog, mods);
        setNote(metadataCatalog, mods);
        setPhysicalDescription(metadataCatalog, mods);
    }

    private void setPhysicalDescription(MetadataCatalog metadataCatalog, ModsDefinition mods) {
        if (metadataCatalog.getPages() != null) {
            Extent extent = new Extent();
            extent.setValue(metadataCatalog.getPages());
            PhysicalDescriptionDefinition physicalDescription = new PhysicalDescriptionDefinition();
            physicalDescription.getExtent().add(extent);
            mods.getPhysicalDescription().add(physicalDescription);
        }
    }

    private void setNote(MetadataCatalog metadataCatalog, ModsDefinition mods) {
        if (metadataCatalog.getNote() != null) {
            NoteDefinition note = new NoteDefinition();
            note.setValue(metadataCatalog.getNote());
            mods.getNote().add(note);
        }
    }

    private void setOriginInfo(MetadataCatalog metadataCatalog, ModsDefinition mods) {
        if (metadataCatalog.getDateIssued() != null && mods.getOriginInfo().isEmpty()) {
            mods.getOriginInfo().add(new OriginInfoDefinition());
        }

        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            if (originInfo.getDateIssued().isEmpty()) {
                originInfo.getDateIssued().add(new DateDefinition());
            }
            for (DateDefinition date : originInfo.getDateIssued()) {
                if (date.getValue() == null || date.getValue().isEmpty()) {
                    date.setValue(metadataCatalog.getDateIssued());
                    break;
                }
            }
        }
    }

    private void setIdentifier(MetadataCatalog metadataCatalog, ModsDefinition mods) {
        IdentifierDefinition identifierLocalId = new IdentifierDefinition();
        identifierLocalId.setType("localId");
        identifierLocalId.setValue(metadataCatalog.getLocalId());
        mods.getIdentifier().add(identifierLocalId);
    }

    private void setTitleInfo(MetadataCatalog metadataCatalog, ModsDefinition mods) {
        if (metadataCatalog.getTitle() != null || metadataCatalog.getSubtitle() != null || metadataCatalog.getPartNumber() != null || metadataCatalog.getPartName() != null) {
            if (mods.getTitleInfo().isEmpty()) {
                mods.getTitleInfo().add(new TitleInfoDefinition());
            }
        }

        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            if (metadataCatalog.getTitle() != null) {
                if (titleInfo.getTitle().isEmpty()) {
                    titleInfo.getTitle().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage title : titleInfo.getTitle()) {
                    if (title.getValue() == null || title.getValue().isEmpty()) {
                        title.setValue(metadataCatalog.getTitle());
                        break;
                    }
                }
            }
            if (metadataCatalog.getSubtitle() != null) {
                if (titleInfo.getSubTitle().isEmpty()) {
                    titleInfo.getSubTitle().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage subtitle : titleInfo.getSubTitle()) {
                    if (subtitle.getValue() == null || subtitle.getValue().isEmpty()) {
                        subtitle.setValue(metadataCatalog.getSubtitle());
                        break;
                    }
                }
            }
            if (metadataCatalog.getPartName() != null) {
                if (titleInfo.getPartName().isEmpty()) {
                    titleInfo.getPartName().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage partName : titleInfo.getPartName()) {
                    if (partName.getValue() == null || partName.getValue().isEmpty()) {
                        partName.setValue(metadataCatalog.getPartName());
                        break;
                    }
                }
            }
            if (metadataCatalog.getPartNumber() != null) {
                if (titleInfo.getPartNumber().isEmpty()) {
                    titleInfo.getPartNumber().add(new StringPlusLanguage());
                }
                for (StringPlusLanguage partNumber : titleInfo.getPartNumber()) {
                    if (partNumber.getValue() == null || partNumber.getValue().isEmpty()) {
                        partNumber.setValue(metadataCatalog.getPartNumber());
                        break;
                    }
                }
            }
        }
    }

    private MetadataCatalog getMetadataCatalog(ImportOptions importOptions) {
        HashMap<String, MetadataCatalog> catalogs = createMetadataCatalog(importOptions);
        if (catalogs == null || catalogs.isEmpty()) {
            return null;
        }
        return catalogs.get(importOptions.getImportFolder().getName());
    }

    private HashMap<String, MetadataCatalog> createMetadataCatalog(ImportOptions importOptions) {
        File importFolder = importOptions.getImportFolder().getParentFile();
        boolean firstLine = true;
        File catalog = null;
        for (File file : importFolder.listFiles()) {
            if (file.exists() && file.isFile() && isAcceptableFile(file)) {
                catalog = file;
                break;
            }
        }
        HashMap<String, MetadataCatalog> catalogs = new HashMap<>();
        if (catalog != null) {
            Path pathToFile = Paths.get(catalog.getAbsolutePath());
            try (BufferedReader br = Files.newBufferedReader(pathToFile, StandardCharsets.UTF_8)) {
                String line = br.readLine();
                while (line != null) {
                    if (firstLine) {
                        firstLine = false;
                        line = br.readLine();
                    } else {
                        String[] attributes = line.split(";");
                        MetadataCatalog metadataCatalog = createCatalog(attributes);
                        if (metadataCatalog != null && metadataCatalog.getFolderPath() != null) {
                            catalogs.put(metadataCatalog.getFolderPath(), metadataCatalog);
                        }
                        line = br.readLine();
                    }
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        }
        return catalogs;
    }

    private MetadataCatalog createCatalog(String[] attributes) {
        MetadataCatalog metadataCatalog = new MetadataCatalog();
        metadataCatalog.setModel(attributes[0]);
        metadataCatalog.setParent(attributes[1]);
        metadataCatalog.setFolderPath(attributes[2]);
        metadataCatalog.setLocalId(attributes[3]);
        metadataCatalog.setTitle(attributes[4]);
        metadataCatalog.setSubtitle(attributes[5]);
        metadataCatalog.setPartNumber(attributes[6]);
        metadataCatalog.setPartName(attributes[7]);
        metadataCatalog.setNote(attributes[8]);
        metadataCatalog.setDateIssued(attributes[9]);
        metadataCatalog.setPages(attributes[10]);
        return metadataCatalog;
    }

}
