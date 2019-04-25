/*
 * Copyright (C) 2017 Lukas Sykora
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
package cz.cas.lib.proarc.common.mods.ndk;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.*;

import cz.cas.lib.proarc.mods.*;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.urnnbn.model.response.TitleInfo;

/**
 *
 * @author Lukas Sykora
 */
public class NdkSoundRecordingMapper extends RdaNdkMapper {

    /**
     * Updates missing required attribute and elements.
     */
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);

        //mods/genre="soundrecording"
        addGenre(mods, "sound recording");
        for (GenreDefinition genre : mods.getGenre()) {
            if ("soundrecording".equals(genre.getValue())) {
                genre.setType("model");
            }
        }
        fillLanguage(mods);
        fillRecordInfo(mods);
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            StringBuilder title = new StringBuilder();
            addNonSort(title, titleInfo);
            addTitle(title, titleInfo);
            addPartNumber(title, titleInfo);
            addElementType(dc.getTitles(), title.toString());
        }

        addName(mods.getName(), dc.getCreators());

        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            for (PlaceDefinition place : originInfo.getPlace()) {
                for (PlaceTermDefinition placeTerm : place.getPlaceTerm()) {
                    addElementType(dc.getCoverages(), placeTerm.getValue());
                }
            }
        }

        for (PhysicalDescriptionDefinition physicalDescription : mods.getPhysicalDescription()) {
            for (Extent extent : physicalDescription.getExtent()) {
                addElementType(dc.getCoverages(), extent.getValue());
            }
        }

        addLanguage(mods.getLanguage(), dc);

        for (TableOfContentsDefinition tableOfContents : mods.getTableOfContents()) {
            addElementType(dc.getDescriptions(), tableOfContents.getValue());
        }

        addStringPlusLanguage(dc.getDescriptions(), mods.getNote());

        for (SubjectDefinition subject : mods.getSubject()) {
            addStringPlusLanguage(dc.getSubjects(), subject.getTopic());
            addStringPlusLanguage(dc.getSubjects(), subject.getGeographic());
            addStringPlusLanguage(dc.getSubjects(), subject.getTemporal());
            for (SubjectNameDefinition subjectName : subject.getName()) {
                addStringPlusLanguage(dc.getSubjects(), subjectName.getNamePart());
            }
        }
        return dc;
    }
}
