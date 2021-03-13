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
package cz.cas.lib.proarc.common.export.crossref;

import cz.cas.lib.proarc.common.export.ExportException;
import cz.cas.lib.proarc.common.export.ExportOptions;
import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.export.cejsh.CejshStatusHandler;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import java.io.File;
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
    private final RemoteStorage remotes;
    private final ExportOptions options;
    private List<String> pids;

    public CrossrefExport(DigitalObjectManager dom, RemoteStorage remotes, ExportOptions options) {
        this.dom = dom;
        this.remotes = remotes;
        this.options = options;
        this.pids = new ArrayList<>();
    }

    public void export(File output, List<String> pids, CejshStatusHandler status) {
        try {
            exportImpl(output, pids, status);
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

    private void exportImpl(File output, List<String> pids, CejshStatusHandler status) throws ExportException {
        output = prepareExportFolder(output, pids, "crossref_" + FoxmlUtils.pidAsUuid(pids.get(0)));
        status.setTargetFolder(output);

        DigitalObjectCrawler crawler = new DigitalObjectCrawler(dom, remotes.getSearch());
        CrossrefObjectSelector selector = new CrossrefObjectSelector(crawler, status);
        List<CrossrefPackage> packages = selector.select(pids);
        if (packages.isEmpty()) {
            status.error(pids.get(0), "Nothing to export!", null);
            return ;
        }

        CrossrefBuilder crossRefBuilder = initBuilder(output, status, pids.get(0), options);
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
        return ExportUtils.createFolder(output, folderName, options.isOverwritePackage());
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

    private static CrossrefBuilder initBuilder(File output, CejshStatusHandler status, String pid, ExportOptions options) {
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
