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

import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.HierarchyObjectVisitor;
import cz.cas.lib.proarc.common.object.VisitorException;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;

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
        if (NdkPlugin.MODEL_PAGE.equals(model) || NdkPlugin.MODEL_NDK_PAGE.equals(model)) {
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
        } else if (NdkEbornPlugin.MODEL_EMONOGRAPHTITLE.equals(model)) {
            return visitNdkEMonographTitle(elm, p);
        } else if (NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME.equals(model)) {
            return visitNdkEMonographVolume(elm, p);
        } else if (NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(model)) {
            return visitNdkEPeriodicalIssue(elm, p);
        } else if (NdkEbornPlugin.MODEL_EARTICLE.equals(model)) {
            return visitNdkEArticle(elm, p);
        } else if (NdkEbornPlugin.MODEL_ECHAPTER.equals(model)) {
            return visitNdkEChapter(elm, p);
        } else if (NdkAudioPlugin.MODEL_MUSICDOCUMENT.equals(model) || NdkAudioPlugin.MODEL_PHONOGRAPH.equals(model)) {
            return visitNdkMusicDocument(elm, p);
        } else if (OldPrintPlugin.MODEL_PAGE.equals(model)) {
            return visitOldPrintPage(elm, p);
        } else if (OldPrintPlugin.MODEL_CONVOLUTTE.equals(model)) {
            return visitOldPrintConvolutte(elm, p);
        } else if (OldPrintPlugin.MODEL_MONOGRAPHTITLE.equals(model)) {
            return visitOldPrintMonographtTitle(elm, p);
        } else if (OldPrintPlugin.MODEL_VOLUME.equals(model)) {
            return visitOldPrintMonographVolume(elm, p);
        } else if (OldPrintPlugin.MODEL_SUPPLEMENT.equals(model)) {
            return visitOldPrintMonographSupplement(elm, p);
        } else if (OldPrintPlugin.MODEL_CARTOGRAPHIC.equals(model)) {
            return visitOldPrintCartographic(elm, p);
        } else if (OldPrintPlugin.MODEL_CHAPTER.equals(model)) {
            return visitOldPrintChapter(elm, p);
        } else if (OldPrintPlugin.MODEL_GRAPHICS.equals(model)) {
            return visitOldPrintGraphics(elm, p);
        } else if (OldPrintPlugin.MODEL_SHEETMUSIC.equals(model)) {
            return visitOldPrintSheetmusic(elm, p);
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

    public R visitNdkEMonographTitle(DigitalObjectElement elm, P p) throws  VisitorException {
        return visitChildren(elm, p);
    }

    public R visitNdkEMonographVolume(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitNdkEPeriodicalIssue(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitNdkEArticle(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitNdkEChapter(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitNdkMusicDocument(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitOldPrintPage(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }
    public R visitOldPrintSheetmusic(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitOldPrintGraphics(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitOldPrintChapter(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitOldPrintCartographic(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitOldPrintMonographSupplement(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitOldPrintMonographVolume(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitOldPrintMonographtTitle(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }

    public R visitOldPrintConvolutte(DigitalObjectElement elm, P p) throws VisitorException {
        return visitChildren(elm, p);
    }
}
