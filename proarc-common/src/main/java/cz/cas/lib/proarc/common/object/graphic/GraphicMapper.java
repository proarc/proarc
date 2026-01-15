/*
 * Copyright (C) 2021 Lukas Sykora
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

package cz.cas.lib.proarc.common.object.graphic;


import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.mods.ndk.MapperUtils;
import cz.cas.lib.proarc.common.mods.ndk.RdaNdkMapper;
import cz.cas.lib.proarc.mods.Extent;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.SubjectDefinition;
import cz.cas.lib.proarc.mods.SubjectNameDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.mods.UrlDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addElementType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNameIdentifier;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addOriginInfo;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addStringPlusLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.createTitleString;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillAbstract;

/**
 * @author Lukas Sykora
 */
public class GraphicMapper extends RdaNdkMapper {
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        addGenre(mods);
        fillAbstract(mods);
    }

    @Override
    protected String createObjectLabel(ModsDefinition mods) {
        return GraphicMapperFactory.createObjectLabel(mods);
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            addElementType(dc.getTitles(), createTitleString(titleInfo));
        }
        addName(mods.getName(), dc.getCreators());
        addNameIdentifier(mods.getName(), dc.getCreators());
        addElementType(dc.getTypes(), getDcType());
        addOriginInfo(mods.getOriginInfo(), dc);
        addLanguage(mods.getLanguage(), dc);
        for (PhysicalDescriptionDefinition physicalDesc : mods.getPhysicalDescription()) {
            for (FormDefinition form : physicalDesc.getForm()) {
                addElementType(dc.getFormats(), form.getValue());
            }
            for (Extent extent : physicalDesc.getExtent()) {
                addElementType(dc.getFormats(), extent.getValue());
            }
        }
        addStringPlusLanguage(dc.getDescriptions(), mods.getAbstract());
        addStringPlusLanguage(dc.getDescriptions(), mods.getNote());
        for (SubjectDefinition subject : mods.getSubject()) {
            addStringPlusLanguage(dc.getSubjects(), subject.getTopic());
            addStringPlusLanguage(dc.getSubjects(), subject.getGeographic());
            addStringPlusLanguage(dc.getSubjects(), subject.getTemporal());
            for (SubjectNameDefinition subjectName : subject.getName()) {
                addStringPlusLanguage(dc.getSubjects(), subjectName.getNamePart());
            }
        }
        addStringPlusLanguage(dc.getSubjects(), mods.getClassification());
        for (LocationDefinition location : mods.getLocation()) {
            for (UrlDefinition url : location.getUrl()) {
                addElementType(dc.getSources(), url.getValue());
            }
            addStringPlusLanguage(dc.getSources(), location.getPhysicalLocation());
            addStringPlusLanguage(dc.getSources(), location.getShelfLocator());
        }
        return dc;
    }

    protected void addGenre(ModsDefinition mods) {
        MapperUtils.addGenre(mods, Const.GENRE_GRAPHIC);
    }
}
