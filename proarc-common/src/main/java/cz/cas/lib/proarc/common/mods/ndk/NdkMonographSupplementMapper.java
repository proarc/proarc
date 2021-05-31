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
package cz.cas.lib.proarc.common.mods.ndk;

import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.DateOtherDefinition;
import cz.cas.lib.proarc.mods.Extent;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.SubjectDefinition;
import cz.cas.lib.proarc.mods.SubjectNameDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.List;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addElementType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNameIdentifier;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNameWithEtal;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNonSort;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addOriginInfo;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addPartName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addStringPlusLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addTitle;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillAbstract;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillRecordInfo;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.findPartName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.findPartNumber;

/**
 *
 * @author Jan Pokorsky
 */
public class NdkMonographSupplementMapper extends RdaNdkMapper {


    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        addGenre(mods);
        addNameWithEtal(mods);
        // mods/language/languageTerm @type=code, @authority="iso639‐2b"
        fillLanguage(mods);
        // physicalDescription
        //  mods/classification@authority="udc"
        List<ClassificationDefinition> classifications = mods.getClassification();
        for (ClassificationDefinition classification : classifications) {
            repairAuthorityInClassification(classification);
        }
        for (OriginInfoDefinition oi : mods.getOriginInfo()) {
            // sets type in element dateOther
            for (DateOtherDefinition dateOther : oi.getDateOther()) {
                dateOther.setType(oi.getEventType());
            }
        }
        for (PhysicalDescriptionDefinition pd : mods.getPhysicalDescription()) {
            for (FormDefinition form : pd.getForm()) {
                if (ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDAMEDIA.equals(form.getAuthority())) {
                    form.setType("media");
                } else if (ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDACARRIER.equals(form.getAuthority())) {
                    form.setType("carrier");
                } else {
                    form.setType(null);
                }
            }
        }

        fillAbstract(mods);
        fillRecordInfo(mods);
    }

    protected void addGenre(ModsDefinition mods) {
        //  mods/genre="supplement"
        MapperUtils.addGenre(mods, Const.GENRE_SUPPLEMENT);
    }

    @Override
    protected String createObjectLabel(ModsDefinition mods) {
        String partNumber = findPartNumber(mods);
        String partName = findPartName(mods);
        if (partNumber != null && partName != null) {
            return partNumber + ", " + partName;
        } else {
            return partNumber != null ? partNumber : partName;
        }
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            StringBuilder title = new StringBuilder();
            addNonSort(title, titleInfo);
            addTitle(title, titleInfo);
            addPartName(title, titleInfo);
            addElementType(dc.getTitles(), title.toString());

            addStringPlusLanguage(dc.getDescriptions(), titleInfo.getPartNumber());
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
        return dc;
    }
}
