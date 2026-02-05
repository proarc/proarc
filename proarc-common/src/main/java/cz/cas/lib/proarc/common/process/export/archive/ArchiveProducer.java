/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.process.export.archive;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.process.export.ExportResultLog;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType;
import jakarta.xml.bind.JAXB;
import java.io.File;
import java.util.List;
import java.util.logging.Logger;

/**
 * It produces archive packages.
 *
 * @author Jan Pokorsky
 */
public class ArchiveProducer {

    private static final Logger LOG = Logger.getLogger(ArchiveProducer.class.getName());
    protected final DigitalObjectCrawler crawler;
    protected ExportResultLog reslog;
    protected AppConfiguration appConfig;
    protected AkubraConfiguration akubraConfiguration;

    public ArchiveProducer(AppConfiguration appConfiguration, AkubraConfiguration akubraConfiguration) {
        this.appConfig = appConfiguration;
        this.akubraConfiguration = akubraConfiguration;
        SearchView searchView = null;
        try {
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                searchView = FedoraStorage.getInstance().getSearch();
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                searchView = AkubraStorage.getInstance(akubraConfiguration).getSearch();
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
        this.crawler = new DigitalObjectCrawler(DigitalObjectManager.getDefault(), searchView);
    }

    public static void fixPdfFile(File targetFolder) {
        File metsFile = getFile(targetFolder, PackageBuilder.METS_FILENAME);
        File originFolder = getFile(targetFolder, "original");

        Mets mets = JAXB.unmarshal(metsFile, Mets.class);
        MetsType.FileSec fileSec = mets.getFileSec();
        for (MetsType.FileSec.FileGrp fileGrp : fileSec.getFileGrp()) {
            if ("RAW".equals(fileGrp.getID())) {
                for (FileType fileType : fileGrp.getFile()) {
                    if (fileType.getFLocat().get(0) != null) {
                        FileType.FLocat flocat = fileType.getFLocat().get(0);
                        String href = flocat.getHref();
                        href = repairHref(href, originFolder);
                        flocat.setHref(href);
                        break;
                    }
                }
            }
        }
        File metsParent = metsFile.getParentFile();
        MetsUtils.deleteFolder(metsFile);

        JAXB.marshal(mets, new File(metsParent, PackageBuilder.METS_FILENAME));
    }

    private static String repairHref(String href, File originFolder) {
        String id = href.split("_")[2];
        String fileExtension = href.split("\\.")[2];
        for (File file : originFolder.listFiles()) {
            if (file.getName().endsWith(id + "." + fileExtension)) {
                return getPath(file);
            }
        }
        return href;
    }

    private static String getPath(File pdfFile) {
        File originFile = pdfFile.getParentFile();
        File urnNbnFile = originFile.getParentFile();

        return "./NDK/" + urnNbnFile.getName() + "/" + originFile.getName() + "/" + pdfFile.getName();
    }

    private static File getFile(File file, String filename) {
        if (filename.equals(file.getName())) {
            return file;
        }
        if (file.isDirectory()) {
            for (File children : file.listFiles()) {
                File metsFile = getFile(children, filename);
                if (metsFile != null) {
                    return metsFile;
                }
            }
        }
        return null;
    }

    /**
     * Gets details about last archiving.
     */
    public ExportResultLog getResultLog() {
        return reslog;
    }

    /**
     * It selects object hierarchies to build archive packages of them.
     *
     * @param pids a list of PIDS to archive
     * @return the result folder that contains folders with archive packages.
     * @throws IllegalStateException failure. See {@link #getResultLog() } for details.
     */
    public File archive(List<String> pids, File archiveRootFolder, boolean ignoreMissingUrnNbn) throws IllegalStateException {
        reslog = new ExportResultLog();
        //File archiveRootFolder = ExportUtils.createFolder(targetFolder, "archive_" + FoxmlUtils.pidAsUuid(pids.get(0)));
        archiveImpl(pids, archiveRootFolder, ignoreMissingUrnNbn);
        return archiveRootFolder;
    }

    private void archiveImpl(List<String> pids, File archiveRootFolder, boolean ignoreMissingUrnNbn) {
        List<List<DigitalObjectElement>> objectPaths = null;
        try {
            objectPaths = selectObjects(pids);
        } catch (MetsExportException ex) {
            ExportResultLog.ExportResult result = new ExportResultLog.ExportResult();
            result.setInputPid(pids.get(0));
            result.setStatus(ExportResultLog.ResultStatus.FAILED);
            reslog.getExports().add(result);
            result.getError().add(new ExportResultLog.ResultError(pids.get(0), ex.getMessage(), ex.getMessage(), null));
            throw new IllegalStateException("Archivation failed!", ex);
        }

        ArchiveObjectProcessor processor = new ArchiveObjectProcessor(crawler, archiveRootFolder, appConfig, akubraConfiguration, ignoreMissingUrnNbn);
        for (List<DigitalObjectElement> path : objectPaths) {
            ExportResultLog.ExportResult result = new ExportResultLog.ExportResult();
            DigitalObjectElement dobj = path.get(0);
            result.setInputPid(dobj.getPid());
            reslog.getExports().add(result);
            try {
                processor.getDevicePids().clear();
                processor.process(path);
                result.setStatus(ExportResultLog.ResultStatus.OK);
            } catch (MetsExportException ex) {
                result.setStatus(ExportResultLog.ResultStatus.FAILED);
                result.getError().add(new ExportResultLog.ResultError(dobj.getPid(), ex.getMessage(), dobj.toString(), null));
                throw new IllegalStateException("Archivation failed!", ex);
            } catch (Exception ex) {
                result.setStatus(ExportResultLog.ResultStatus.FAILED);
                result.getError().add(new ExportResultLog.ResultError(dobj.getPid(), null, dobj.toString(), ex));
                throw new IllegalStateException("Archivation failed!", ex);
            } finally {
                result.setEnd();
            }
        }
    }

    protected List<List<DigitalObjectElement>> selectObjects(List<String> pids) throws MetsExportException {
        ArchiveObjectSelector selector = new ArchiveObjectSelector(crawler);
        try {
            selector.select(pids);
            return selector.getSelectedObjects();
        } catch (DigitalObjectException ex) {
            ExportResultLog.ExportResult archiveResult = new ExportResultLog.ExportResult();
            archiveResult.setInputPid(pids.get(0));
            reslog.getExports().add(archiveResult);
            archiveResult.setStatus(ExportResultLog.ResultStatus.FAILED);
            archiveResult.getError().add(new ExportResultLog.ResultError(ex.getPid(), null, ex));
            archiveResult.setEnd();
            throw new IllegalStateException("Archivation failed!", ex);
        }
    }

}
