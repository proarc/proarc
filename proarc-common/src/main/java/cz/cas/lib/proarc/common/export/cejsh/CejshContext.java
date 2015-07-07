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

import cz.cas.lib.proarc.common.export.cejsh.CejshBuilder.Article;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import java.io.File;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Jan Pokorsky
 */
class CejshContext {
    
    private DigitalObjectElement title;
    private DigitalObjectElement volume;
    private DigitalObjectElement issue;
    private List<Article> articles;
    private final CejshStatusHandler statusHandler;
    private final CejshBuilder builder;
    private Set<DigitalObjectElement> includeArticleFilter;
    private DigitalObjectElement articleParentFilter;
    private final File output;

    public CejshContext(File output, CejshStatusHandler statusHandler, CejshConfig config) throws Exception {
        this.builder = new CejshBuilder(config);
        this.statusHandler = statusHandler;
        this.output = output;
    }

    /**
     * The list of cejsh articles from a given subtree. Or {@code null} when
     * articles should not be collected (e.g. error occurred, unknown subtree, ...).
     */
    public List<Article> getArticles() {
        return articles;
    }

    public void setArticles(List<Article> articles) {
        this.articles = articles;
    }

    public CejshBuilder getBuilder() {
        return builder;
    }

    public CejshStatusHandler getStatus() {
        return statusHandler;
    }

    /**
     * Gets the output folder of the export session.
     */
    public File getOutput() {
        return output;
    }

    /**
     * Use to include given articles.
     * @param articleParent parent issue
     * @param includeArticles articles to include
     */
    public void setFilter(DigitalObjectElement articleParent, Set<DigitalObjectElement> includeArticles) {
        this.articleParentFilter = articleParent;
        this.includeArticleFilter = includeArticles != null
                ? includeArticles : Collections.<DigitalObjectElement>emptySet();
    }

    /**
     * Gets articles that should be filtered from a given parent.
     * @param parent
     * @return either set of articles to include or empty set to include all.
     */
    public Set<DigitalObjectElement> getFilter(DigitalObjectElement parent) {
        return this.articleParentFilter == parent ? includeArticleFilter : Collections.<DigitalObjectElement>emptySet();
    }

    public boolean acceptArticle(DigitalObjectElement parent, DigitalObjectElement article) {
        if (parent == null || parent != articleParentFilter || includeArticleFilter.isEmpty()) {
            return true;
        } else {
            return includeArticleFilter.contains(article);
        }
    }

    /**
     * Gets an issue object of the current hierarchy path or {@code null}.
     */
    public DigitalObjectElement getIssue() {
        return issue;
    }

    public void setIssue(DigitalObjectElement issue) {
        this.issue = issue;
    }

    /**
     * Gets a volume object of the current hierarchy path or {@code null}.
     */
    public DigitalObjectElement getVolume() {
        return volume;
    }

    public void setVolume(DigitalObjectElement volume) {
        this.volume = volume;
    }

    /**
     * Gets a title object of the current hierarchy path or {@code null}.
     */
    public DigitalObjectElement getTitle() {
        return title;
    }

    public void setTitle(DigitalObjectElement title) {
        this.title = title;
    }

    public void reset() {
        articleParentFilter = null;
        articles = null;
        builder.setIssue(null);
        builder.setTitle(null);
        builder.setVolume(null);
        includeArticleFilter = null;
        title = null;
        volume = null;
    }

}
