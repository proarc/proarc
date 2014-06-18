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
package cz.cas.lib.proarc.common.object.ndk;

import cz.cas.lib.proarc.common.object.HierarchyObjectVisitor;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.VisitorException;

/**
 * It is a helper class to implement {@link NdkVisitor}.
 * It walks down the NDK hierarchy. Subclass to add custom behavior.
 *
 * @author Jan Pokorsky
 */
public class DefaultNdkVisitor<R, P> extends HierarchyObjectVisitor<R, P> implements NdkVisitor<R, P> {

    public DefaultNdkVisitor(DigitalObjectCrawler crawler) {
        super(crawler);
    }

    @Override
    public R visit(DigitalObjectElement elm, P p) throws VisitorException {
        if (elm == null) {
            return null;
        }
        String model = elm.getItem().getModel();
        if ("model:page".equals(model)) {
            return visitNdkPage(elm, p);
        } else if (NdkPlugin.MODEL_PERIODICALISSUE.equals(model)) {
            return visitNdkPeriodicalIssue(elm, p);
        } else if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(model)) {
            return visitNdkPeriodicalVolume(elm, p);
        } else if (NdkPlugin.MODEL_PERIODICAL.equals(model)) {
            return visitNdkPeriodical(elm, p);
        } else if (NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(model)) {
            return visitNdkMonographVolume(elm, p);
        } else if (NdkPlugin.MODEL_MONOGRAPHTITLE.equals(model)) {
            return visitNdkMonographTitle(elm, p);
        } else if (NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT.equals(model)) {
            return visitNdkMonographSupplement(elm, p);
        } else if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(model)) {
            return visitNdkPeriodicalSupplement(elm, p);
        } else if (NdkPlugin.MODEL_ARTICLE.equals(model)) {
            return visitNdkArticle(elm, p);
        } else if (NdkPlugin.MODEL_CARTOGRAPHIC.equals(model)) {
            return visitNdkCartographic(elm, p);
        } else if (NdkPlugin.MODEL_CHAPTER.equals(model)) {
            return visitNdkChapter(elm, p);
        } else if (NdkPlugin.MODEL_PICTURE.equals(model)) {
            return visitNdkPicture(elm, p);
        } else if (NdkPlugin.MODEL_SHEETMUSIC.equals(model)) {
            return visitNdkSheetMusic(elm, p);
        }
        return super.visit(elm, p);
    }

    @Override
    public R visitNdkPeriodical(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkPeriodicalVolume(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkPeriodicalIssue(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkPeriodicalSupplement(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkArticle(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkMonographTitle(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkMonographVolume(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkMonographSupplement(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkCartographic(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkSheetMusic(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkChapter(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkPicture(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    @Override
    public R visitNdkPage(DigitalObjectElement elm, P p) throws VisitorException {
        return null;
    }

}
