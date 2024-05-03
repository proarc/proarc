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

import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectVisitor;
import cz.cas.lib.proarc.common.object.VisitorException;

/**
 * Implement to process NDK digital objects.
 *
 * @author Jan Pokorsky
 */
public interface NdkVisitor<R, P> extends DigitalObjectVisitor<R, P> {

    R visitNdkPeriodical(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkPeriodicalVolume(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkPeriodicalIssue(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkPeriodicalSupplement(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkArticle(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkMonographTitle(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkMonographUnit(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkMonographVolume(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkMonographSupplement(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkCartographic(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkGraphic(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkSheetMusic(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkChapter(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkPicture(DigitalObjectElement elm, P p) throws VisitorException;

    R visitNdkPage(DigitalObjectElement elm, P p) throws VisitorException;
}
