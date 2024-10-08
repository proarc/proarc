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
package cz.cas.lib.proarc.common.process.export.crossref;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.export.ExportException;
import cz.cas.lib.proarc.common.process.export.ExportParams;
import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.process.export.cejsh.CejshStatusHandler;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Exports born-digital articles in CrossRef format.
 *
 * @see <a href='https://github.com/proarc/proarc/issues/444'>issue 444</a>
 * @author Jan Pokorsky
 */
public class CrossrefExport {

    private DigitalObjectManager dom;
    private final AppConfiguration appConfiguration;
    private final AkubraConfiguration akubraConfiguration;
    private List<String> pids;

    public CrossrefExport(DigitalObjectManager dom, AppConfiguration appConfiguration, AkubraConfiguration akubraConfiguration) {
        this.dom = dom;
        this.appConfiguration = appConfiguration;
        this.akubraConfiguration = akubraConfiguration;
        this.pids = new ArrayList<>();
    }

    public void export(File output, List<String> pids, CejshStatusHandler status, Batch batch) throws IOException {
        try {
            exportImpl(output, pids, status, batch);
            storeExportResult(output, "Export succesfull");
        } catch (ExportException ex) {
            status.error(ex);
        } finally {
            File targetFolder = status.getTargetFolder();
            if (targetFolder != null) {
                ExportUtils.writeExportResult(targetFolder, status.getReslog());
            }
        }
    }

    private void storeExportResult(File output, String log) {
        for (String pid : pids) {
            try {
                ExportUtils.storeObjectExportResult(pid, output.toURI().toASCIIString(), "CROSREFF", log);
            } catch (DigitalObjectException ex) {
                throw new IllegalStateException(ex);
            }
        }
    }

    private void exportImpl(File output, List<String> pids, CejshStatusHandler status, Batch batch) throws ExportException, IOException {
        output = prepareExportFolder(output, pids, "crossref_" + FoxmlUtils.pidAsUuid(pids.get(0)));
        if (batch != null) {
            BatchUtils.updateExportingBatch(BatchManager.getInstance(), batch, output);
        }
        status.setTargetFolder(output);
        SearchView search = null;
        if (Storage.FEDORA.equals(appConfiguration.getTypeOfStorage())) {
            search = FedoraStorage.getInstance(appConfiguration).getSearch();
        } else if (Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
            search = AkubraStorage.getInstance(akubraConfiguration).getSearch();
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfiguration.getTypeOfStorage());
        }
        final DigitalObjectCrawler crawler = new DigitalObjectCrawler(dom, search);
        CrossrefObjectSelector selector = new CrossrefObjectSelector(crawler, status);
        List<CrossrefPackage> packages = selector.select(pids);
        if (packages.isEmpty()) {
            status.error(pids.get(0), "Nothing to export!", null);
            return ;
        }

        CrossrefBuilder crossRefBuilder = initBuilder(output, status, pids.get(0), appConfiguration.getExportParams());
        if (crossRefBuilder == null) {
            return ;
        }

        for (CrossrefPackage aPackage : packages) {
            exportPackage(aPackage, selector, crossRefBuilder, status);
            if (!status.isOk()) {
                return;
            }
        }
    }

    private File prepareExportFolder(File output, List<String> pids, String folderName) throws ExportException {
        if (!output.exists() || !output.isDirectory()) {
            throw new ExportException(null, "Invalid output: " + output, null, null);
        }
        if (pids == null || pids.isEmpty()) {
            throw new ExportException(null, "Nothing to export. Missing input PID!", null, null);
        }
        return ExportUtils.createFolder(output, folderName, this.appConfiguration.getExportParams().isOverwritePackage());
    }

    private void exportPackage(
            CrossrefPackage aPackage,
            CrossrefObjectSelector selector,
            CrossrefBuilder crossRefBuilder,
            CejshStatusHandler status
    ) {
        DigitalObjectElement elm = aPackage.getPath().get(0);
        try {
            status.startInput(elm);
            List<DigitalObjectElement> articles = selector.selectArticles(
                    elm, aPackage.getArticleFilter());
            if (!articles.isEmpty()) {
                aPackage.setArticles(articles);
                crossRefBuilder.createPackage(aPackage);
                addExportedPids(aPackage, articles);
            }
        } catch (ExportException ex) {
            status.error(ex);
        } catch (Exception ex) {
            status.error(elm, "Unexpected error!", ex);
        } finally {
            status.finishInput(elm);
        }
    }

    private void addExportedPids(CrossrefPackage aPackage, List<DigitalObjectElement> articles) {
        for (DigitalObjectElement obj : aPackage.getPath()) {
            pids.add(obj.getPid());
        }

        for (DigitalObjectElement obj : articles) {
            pids.add(obj.getPid());
        }
    }

    private static CrossrefBuilder initBuilder(File output, CejshStatusHandler status, String pid, ExportParams options) {
        try {
            return new CrossrefBuilder(output, options);
        } catch (Exception ex) {
            if (ex.getMessage().contains("export.cejsh_crossref.journals.path=")) {
                status.error(pid, "Not configurated!", ex);
            } else {
                status.error(pid, "Broken context!", ex);
            }
            return null;
        }
    }

}
