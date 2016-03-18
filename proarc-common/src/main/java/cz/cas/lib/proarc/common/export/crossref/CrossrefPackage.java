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

import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Jan Pokorsky
 */
class CrossrefPackage {

    private List<DigitalObjectElement> path;
    private List<DigitalObjectElement> articles;
    private Set<DigitalObjectElement> articleFilter;

    /**
     * Gets the sorted list of all articles found in the storage.
     */
    public List<DigitalObjectElement> getArticles() {
        return articles;
    }

    public void setArticles(List<DigitalObjectElement> articles) {
        this.articles = articles;
    }

    /**
     * Gets a collection of articles that should be included in package or {@code null}
     * to include all articles.
     */
    public Set<DigitalObjectElement> getArticleFilter() {
        return articleFilter;
    }

    public void setArticleFilter(Set<DigitalObjectElement> articleFilter) {
        this.articleFilter = articleFilter;
    }

    /**
     * Gets a path from the NDK issue to the root.
     */
    public List<DigitalObjectElement> getPath() {
        return path;
    }

    public void setPath(List<DigitalObjectElement> path) {
        this.path = path;
    }

}
