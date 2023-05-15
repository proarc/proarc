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
package cz.cas.lib.proarc.common.export.cejsh;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.ExportParams;
import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.export.cejsh.CejshBuilder.Article;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.VisitorException;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.ndk.DefaultNdkVisitor;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import java.io.File;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

/**
 * Exports born-digital articles in CEJSH format.
 *
 * @see <a href='https://github.com/proarc/proarc/issues/286'>issue 286</a>
 * @author Jan Pokorsky
 */
public class CejshExport {

    private DigitalObjectManager dom;
    private final CejshConfig cejshConfig;
    private final ExportParams options;
    private final AppConfiguration appConfiguration;
    private final AkubraConfiguration akubraConfiguration;

    public CejshExport(DigitalObjectManager dom, AppConfiguration appConfiguration, AkubraConfiguration akubraConfiguration) {
        this.appConfiguration = appConfiguration;
        this.akubraConfiguration = akubraConfiguration;
        this.dom = dom;
        this.cejshConfig = CejshConfig.from(appConfiguration.getAuthenticators());
        this.options = appConfiguration.getExportParams();
    }

    public CejshStatusHandler export(File outputFolder, List<String> pids) throws IOException {
        return export(outputFolder, pids, new CejshStatusHandler(cejshConfig.getLogLevel()));
    }

    /**
     * Runs the export and writes the export log.
     * @param outputFolder a folder where to create the export folder
     * @param pids PIDs of digital objects to include in the export. NDK and born-digital
     *          objects are expected
     * @param status an export status
     * @return the export status including the target folder and errors
     */
    public CejshStatusHandler export(File outputFolder, List<String> pids, CejshStatusHandler status) throws IOException {
        try {
            return exportImpl(outputFolder, pids, status);
        } finally {
            File targetFolder = status.getTargetFolder();
            if (targetFolder != null) {
                ExportUtils.writeExportResult(targetFolder, status.getReslog());
            }
        }
    }

    private CejshStatusHandler exportImpl(File output, List<String> pids, CejshStatusHandler status) throws IOException {
        // XXX write export to RELS-EXT
        if (!output.exists() || !output.isDirectory()) {
            status.error((String) null, "Invalid output: " + output, null);
            return status;
        }
        if (pids == null || pids.isEmpty()) {
            status.error((String) null, "Nothing to export. Missing input PID!", null);
            return status;
        }
        output = ExportUtils.createFolder(output, "cejsh_" + FoxmlUtils.pidAsUuid(pids.get(0)), options.isOverwritePackage());
        status.setTargetFolder(output);
        SearchView search = null;
        if (Storage.FEDORA.equals(appConfiguration.getTypeOfStorage())) {
            search = RemoteStorage.getInstance(appConfiguration).getSearch();
        }  else if (Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())){
            search = AkubraStorage.getInstance(akubraConfiguration).getSearch();
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfiguration.getTypeOfStorage());
        }
        final DigitalObjectCrawler crawler = new DigitalObjectCrawler(dom, search);
        CejshContext ctx;
        try {
            ctx = new CejshContext(output, status, cejshConfig, options);
        } catch (Exception ex) {
            if (ex.getMessage().contains("export.cejsh_crossref.journals.path=")) {
                status.error(pids.get(0), "Not configurated!", ex);
            } else {
                status.error(pids.get(0), "Broken context!", ex);
            }
            return status;
        }
        CejshHierarchy hierarchy = new CejshHierarchy(crawler);
        LinkedHashMap<DigitalObjectElement, Set<DigitalObjectElement>> inputs =
                prepareInputQueue(pids, crawler, ctx);
        for (Entry<DigitalObjectElement, Set<DigitalObjectElement>> entry : inputs.entrySet()) {
            DigitalObjectElement dobj = entry.getKey();
            try {
                status.startInput(dobj);
                ctx.reset();
                ctx.setFilter(dobj, entry.getValue());
                hierarchy.visit(dobj, ctx);
            } catch (Throwable ex) {
                status.error(dobj, "Unexpected error", null, ex);
                // XXX continue in case of validation or always?
                return status;
            } finally {
                status.finishInput(dobj);
            }
        }
        return status;
    }

    /**
     * Transforms a list of PIDs to the list of digital objects to be exported.
     * When an object is the article then its parent is listed instead and
     * the article is included in the attached set.
     * Other children of the parent are ignored during the export.
     * @param pids input PIDs
     * @param crawler the search index
     * @param ctx the context
     * @return the list of unique digital objects and their articles to include.
     *      The {@code null} Set means include all children.
     */
    private LinkedHashMap<DigitalObjectElement, Set<DigitalObjectElement>> prepareInputQueue(
            List<String> pids, final DigitalObjectCrawler crawler, CejshContext ctx) {

        LinkedHashMap<DigitalObjectElement, Set<DigitalObjectElement>> dobjs =
                new LinkedHashMap<DigitalObjectElement, Set<DigitalObjectElement>>(pids.size());
        for (String pid : pids) {
            try {
                DigitalObjectElement elm = crawler.getEntry(pid);
                if (BornDigitalModsPlugin.MODEL_ARTICLE.equals(elm.getModelId())) {
                    // add article as inlude filter
                    DigitalObjectElement parent = crawler.getParent(pid);
                    if (parent == DigitalObjectElement.NULL) {
                        ctx.getStatus().error(elm, "No parent!", null, null);
                        break;
                    }
                    Set<DigitalObjectElement> children = dobjs.get(parent);
                    if (children == null) {
                        children = new HashSet<DigitalObjectElement>();
                        dobjs.put(parent, children);
                    }
                    children.add(elm);
                } else {
                    if (!dobjs.containsKey(elm)) {
                        dobjs.put(elm, null);
                    }
                }
            } catch (DigitalObjectException ex) {
                ctx.getStatus().error(pid, "No parent!", ex);
            }
        }
        return dobjs;
    }

    static final class CejshHierarchy extends DefaultNdkVisitor<Void, CejshContext> {

        private final Deque<DigitalObjectElement> traversePath = new ArrayDeque<DigitalObjectElement>();

        public CejshHierarchy(DigitalObjectCrawler crawler) {
            super(crawler);
        }

        protected DigitalObjectElement getParent() {
            Iterator<DigitalObjectElement> it = getParentPath();
            return it.hasNext() ? it.next() : null;
        }

        protected Iterator<DigitalObjectElement> getParentPath() {
            Iterator<DigitalObjectElement> it = getPath().iterator();
            it.next();
            return it;
        }

        protected Deque<DigitalObjectElement> getPath() {
            return traversePath;
        }

        @Override
        public Void visit(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            try {
                traversePath.addFirst(elm);
                String modelId = elm.getModelId();
                if (BornDigitalModsPlugin.MODEL_ARTICLE.equals(modelId)) {
                    return visitBdmArticle(elm, p);
                } else {
                    return super.visit(elm, p);
                }
            } finally {
                traversePath.removeFirst();
            }
        }

        @Override
        public Void visitChildren(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            Set<DigitalObjectElement> filter = p.getFilter(elm);
            if (filter.isEmpty()) {
                return super.visitChildren(elm, p);
            } else {
                try {
                    List<DigitalObjectElement> children = getCrawler().getChildren(elm.getPid());
                    for (DigitalObjectElement child : children) {
                        if (filter.contains(child)) {
                            child.accept(this, p);
                        }
                    }
                    return null;
                } catch (DigitalObjectException ex) {
                    throw new VisitorException(ex);
                }
            }
        }

        @Override
        public Void visitNdkArticle(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            return null;
        }

        public Void visitBdmArticle(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            CejshBuilder builder = p.getBuilder();
            List<Article> articles = p.getArticles();
            if (articles != null && p.acceptArticle(getParent(), elm)) {
                Article article = builder.addArticle(elm, p);
                if (article == null) {
                    // broken package, discard articles and ignore others
                    p.setArticles(null);
                } else if (article.isReviewed() && article.hasEnglishAbstract()) {
                    articles.add(article);
                }
            }
            return null;
        }

        @Override
        public Void visitNdkPeriodicalIssue(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            if (p.getIssue() != null) {
                p.getStatus().error(elm, "Issue inside issue: " + p.getIssue().toLog(), null, null);
                return null;
            }
            CejshBuilder builder = p.getBuilder();
            if (getPath().size() == 1) { // start point
                try {
                    p.setVolume(findParent(elm, p, NdkPlugin.MODEL_PERIODICALVOLUME));
                    if (!builder.addVolume(elm, p.getVolume(), p)) {
                        return null;
                    }
                    p.setTitle(findParent(p.getVolume(), p, NdkPlugin.MODEL_PERIODICAL));
                    if (!builder.addTitle(elm, p.getTitle(), p)) {
                        return null;
                    }
                } catch (DigitalObjectNotFoundException ex) {
                    p.getStatus().error(elm, "Parent not found!", null, ex);
                    return null;
                }
            }
            // process only articles linked by this issue; do not mix with articles from the volume level
            List<Article> parentArticles = p.getArticles();
            p.setArticles(new ArrayList<Article>());
            try {
                p.setIssue(elm);
                Void result = null;
                if (builder.addIssue(elm, elm, p)) {
                    result = super.visitNdkPeriodicalIssue(elm, p);
                    builder.writePackage(elm, p.getArticles(), p);
                }
                return result;
            } finally {
                p.setArticles(parentArticles);
                p.setIssue(null);
                builder.setIssue(null);
            }
        }

        @Override
        public Void visitNdkPeriodicalVolume(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            if (p.getVolume() != null) {
                p.getStatus().error(elm, "Volume inside volume: " + p.getVolume().toLog(), null, null);
                return null;
            }
            CejshBuilder builder = p.getBuilder();
            if (getPath().size() == 1) { // start point
                try {
                    p.setTitle(findParent(elm, p, NdkPlugin.MODEL_PERIODICAL));
                    if (!builder.addTitle(elm, p.getTitle(), p)) {
                        return null;
                    }
                } catch (DigitalObjectNotFoundException ex) {
                    p.getStatus().error(elm, "Parent not found!", null, ex);
                    return null;
                }
            }
            try {
                p.setVolume(elm);
                p.setArticles(new ArrayList<Article>());
                Void result = null;
                if (builder.addVolume(elm, elm, p)) {
                    result = super.visitNdkPeriodicalVolume(elm, p);
                    builder.writePackage(elm, p.getArticles(), p);
                }
                return result;
            } finally {
                p.setArticles(null);
                p.setVolume(null);
                builder.setVolume(null);
            }
        }

        @Override
        public Void visitNdkPeriodical(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            if (p.getTitle() != null) {
                p.getStatus().error(elm, "Title inside title: " + p.getTitle().toLog(), null, null);
                return null;
            }
            p.setTitle(elm);
            try {
                Void result = null;
                if (p.getBuilder().addTitle(elm, elm, p)) {
                    result = super.visitNdkPeriodical(elm, p);
                }
                return result;
            } finally {
                p.setTitle(null);
                p.getBuilder().setTitle(null);
            }
        }

        @Override
        public Void visitNdkCartographic(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            return null;
        }

        @Override
        public Void visitNdkChapter(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            return null;
        }

        @Override
        public Void visitNdkMonographSupplement(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            return null;
        }

        @Override
        public Void visitNdkPage(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            return null;
        }

        @Override
        public Void visitNdkPicture(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            return null;
        }

        @Override
        public Void visitNdkSheetMusic(DigitalObjectElement elm, CejshContext p) throws VisitorException {
            return null;
        }

        private DigitalObjectElement findParent(DigitalObjectElement elm, CejshContext p, String... modelId) throws DigitalObjectNotFoundException {
            HashSet<String> set = new HashSet<String>();
            for (String id : modelId) {
                set.add(id);
            }
            return findParent(elm, set, p);
        }

        private DigitalObjectElement findParent(DigitalObjectElement elm, Set<String> modelId, CejshContext p) throws DigitalObjectNotFoundException {
            DigitalObjectElement parent = getCrawler().getParent(elm.getPid());
            if (parent == DigitalObjectElement.NULL) {
                return null;
            } else if (modelId.contains(parent.getModelId())) {
                return parent;
            } else {
                return findParent(parent, modelId, p);
            }
        }

    }

}
