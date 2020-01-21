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
package cz.cas.lib.proarc.common.export.archive;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.ExportResultLog;
import cz.cas.lib.proarc.common.export.ExportResultLog.ExportResult;
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultError;
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultStatus;
import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
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
    private final DigitalObjectCrawler crawler;
    private ExportResultLog reslog;
    private AppConfiguration appConfig;

    public ArchiveProducer(AppConfiguration appConfiguration) {
        this.crawler = new DigitalObjectCrawler(
                DigitalObjectManager.getDefault(), RemoteStorage.getInstance().getSearch());
        this.appConfig = appConfiguration;
    }

    /**
     * Gets details about last archiving.
     */
    public ExportResultLog getResultLog() {
        return reslog;
    }

    /**
     * It selects object hierarchies to build archive packages of them.
     * @param pids a list of PIDS to archive
     * @param targetFolder where to place the result folder
     * @return the result folder that contains folders with archive packages.
     * @throws IllegalStateException failure. See {@link #getResultLog() } for details.
     */
    public File archive(List<String> pids, File archiveRootFolder) throws IllegalStateException {
        reslog = new ExportResultLog();
        //File archiveRootFolder = ExportUtils.createFolder(targetFolder, "archive_" + FoxmlUtils.pidAsUuid(pids.get(0)));

        try {
            archiveImpl(pids, archiveRootFolder);
            return archiveRootFolder;
        } finally {
            ExportUtils.writeExportResult(archiveRootFolder, reslog);
        }
    }

    private void archiveImpl(List<String> pids, File archiveRootFolder) {
        List<List<DigitalObjectElement>> objectPaths = selectObjects(pids);

        ArchiveObjectProcessor processor = new ArchiveObjectProcessor(crawler, archiveRootFolder, appConfig);
        for (List<DigitalObjectElement> path : objectPaths) {
            ExportResult result = new ExportResult();
            DigitalObjectElement dobj = path.get(0);
            result.setInputPid(dobj.getPid());
            reslog.getExports().add(result);
            try {
                processor.process(path);
                result.setStatus(ResultStatus.OK);
            } catch (MetsExportException ex) {
                result.setStatus(ResultStatus.FAILED);
                result.getError().add(new ResultError(dobj.getPid(), ex.getMessage(), dobj.toString(), null));
                throw new IllegalStateException("Archivation failed!", ex);
            } catch (Exception ex) {
                result.setStatus(ResultStatus.FAILED);
                result.getError().add(new ResultError(dobj.getPid(), null, dobj.toString(), ex));
                throw new IllegalStateException("Archivation failed!", ex);
            } finally {
                result.setEnd();
            }
        }
    }

    private List<List<DigitalObjectElement>> selectObjects(List<String> pids) {
        ArchiveObjectSelector selector = new ArchiveObjectSelector(crawler);
        try {
            selector.select(pids);
            return selector.getSelectedObjects();
        } catch (DigitalObjectException ex) {
            ExportResult archiveResult = new ExportResult();
            archiveResult.setInputPid(pids.get(0));
            reslog.getExports().add(archiveResult);
            archiveResult.setStatus(ResultStatus.FAILED);
            archiveResult.getError().add(new ResultError(ex.getPid(), null, ex));
            archiveResult.setEnd();
            throw new IllegalStateException("Archivation failed!", ex);
        }
    }

}
