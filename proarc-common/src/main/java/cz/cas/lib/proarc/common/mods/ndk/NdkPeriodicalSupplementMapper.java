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

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.DateOtherDefinition;
import cz.cas.lib.proarc.mods.Extent;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
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
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.findPartName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.findTitle;

/**
 *
 * @author Jan Pokorsky
 */
public class NdkPeriodicalSupplementMapper extends RdaNdkMapper {

    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        // mods/originInfo/place/placeTerm/type="text" if null
        // mods/language/languageTerm @type=code, @authority="iso639‐2b"
        fillLanguage(mods);
        addNameWithEtal(mods);
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
        fillPhysicalDescription(mods);
        fixAndAddGenre(mods, ctx);
        fillAbstract(mods);
    }

    private void fillPhysicalDescription(ModsDefinition mods) {
        if (mods.getPhysicalDescription().size() == 0) {
            PhysicalDescriptionDefinition physicalDescription = new PhysicalDescriptionDefinition();
            mods.getPhysicalDescription().add(physicalDescription);
            FormDefinition form = new FormDefinition();
            physicalDescription.getForm().add(form);
            form.setValue("print");
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
    }

    protected void fixAndAddGenre(ModsDefinition mods, Context ctx) {
        MapperUtils.removeGenre(mods, Const.GENRE_ESUPPLEMENT);
        if (mods.getGenre().size() == 0) {
            //  mods/genre="supplement"
            MapperUtils.addGenre(mods, Const.GENRE_SUPPLEMENT);
        }
        for (GenreDefinition genre : mods.getGenre()) {
            String type = null;
            if (genre.getValue() == null || "".equals(genre.getValue())) {
                genre.setValue(Const.GENRE_SUPPLEMENT);
            } else if (genre.getValue() != null && Const.GENRE_SUPPLEMENT.equals(genre.getValue()) && genre.getType() == null) {
                if (NdkPlugin.MODEL_PERIODICALISSUE.equals(ctx.getParentModel())) {
                    genre.setType("issue_supplement");
                } else if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(ctx.getParentModel())) {
                    genre.setType("volume_supplement");
                }
            } else if (genre.getValue() != null && !Const.GENRE_SUPPLEMENT.equals(genre.getValue())) {
                if ("volume_supplement".equals(genre.getValue()) || "issue_supplement".equals(genre.getValue())) {
                    type = genre.getValue();
                    genre.setValue(Const.GENRE_SUPPLEMENT);
                    if (genre.getType() == null || genre.getType().isEmpty()) {
                        genre.setType(type);
                    }
                }
            }
        }
    }

    @Override
    protected String createObjectLabel(ModsDefinition mods) {
        String title = findTitle(mods);
        String partName = findPartName(mods);

        StringBuilder sb = new StringBuilder();
        if (title != null && !title.isEmpty()) {
            sb.append(title);
        }

        if (partName != null && !partName.isEmpty()) {
            if (sb.length() > 0) {
                sb.append(": ");
            }
            sb.append(partName);
        }

        return sb.length() > 0 ? sb.toString() : "?";
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
