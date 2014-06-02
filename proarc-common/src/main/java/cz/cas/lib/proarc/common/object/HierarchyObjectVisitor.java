/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import java.util.List;

/**
 * The default implementation to process the hierarchy of digital objects.
 *
 * @author Jan Pokorsky
 */
public class HierarchyObjectVisitor<R, P> implements DigitalObjectVisitor<R, P> {

    private DigitalObjectCrawler crawler;

    public HierarchyObjectVisitor(DigitalObjectCrawler crawler) {
        this.crawler = crawler;
    }

    public DigitalObjectCrawler getCrawler() {
        return crawler;
    }

    @Override
    public R visit(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitChildren(DigitalObjectElement elm, P p) throws VisitorException {
        try {
            List<DigitalObjectElement> children = crawler.getChildren(elm.getPid());
            R last = null;
            for (DigitalObjectElement child : children) {
                last = child.accept(this, p);
            }
            return last;
        } catch (DigitalObjectException ex) {
            throw new VisitorException(elm.getPid(), ex);
        }
    }
}
