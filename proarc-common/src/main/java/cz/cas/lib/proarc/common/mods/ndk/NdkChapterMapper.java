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
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.ExtentDefinition;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.RoleDefinition;
import cz.cas.lib.proarc.mods.RoleTermDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.SubjectDefinition;
import cz.cas.lib.proarc.mods.SubjectNameDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.List;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addDetailWithPageRangeToPart;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addElementType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNameIdentifier;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNameWithEtal;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addStringPlusLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.createTitleString;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillAbstract;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillRecordInfo;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.toValue;

/**
 *
 * @author Jan Pokorsky
 */
public class NdkChapterMapper extends RdaNdkMapper {

    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);

        // name/role/roleTerm@type="CODE"
        // name/role/roleTerm@authority="marcrelator"
        for (NameDefinition name : mods.getName()) {
            for (RoleDefinition role : name.getRole()) {
                for (RoleTermDefinition roleTerm : role.getRoleTerm()) {
                    if (roleTerm.getAuthority() == null) {
                        roleTerm.setAuthority("marcrelator");
                    }
                    if (roleTerm.getType() == null) {
                        roleTerm.setType(CodeOrText.CODE);
                    }
                }
            }
        }
        addGenre(mods);
        addNameWithEtal(mods);
        // mods/language/languageTerm @type=code, @authority="iso639‐2b"
        fillLanguage(mods);
        //  mods/physicalDescription/form@type="code"
        for (PhysicalDescriptionDefinition pd : mods.getPhysicalDescription()) {
            for (FormDefinition form : pd.getForm()) {
                if (form.getType() == null) {
                    form.setType("code");
                }
            }
        }
        //  mods/classification@authority="udc"
        List<ClassificationDefinition> classifications = mods.getClassification();
        for (ClassificationDefinition classification : classifications) {
            repairAuthorityInClassification(classification);
        }
        fillAbstract(mods);
        fillRecordInfo(mods);
        addDetailWithPageRangeToPart(mods);
    }

    protected void addGenre(ModsDefinition mods) {
        //  mods/genre="chapter"
        MapperUtils.addGenre(mods, Const.GENRE_CHAPTER);
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
        for (PhysicalDescriptionDefinition physicalDesc : mods.getPhysicalDescription()) {
            for (FormDefinition form : physicalDesc.getForm()) {
                addElementType(dc.getFormats(), form.getValue());
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
        addElementType(dc.getTypes(), getDcType());
        addStringPlusLanguage(dc.getSubjects(), mods.getClassification());
        for (PartDefinition part : mods.getPart()) {
            for (ExtentDefinition extent : part.getExtent()) {
                StringBuilder sb = new StringBuilder();
                StringPlusLanguage start = extent.getStart();
                StringPlusLanguage end = extent.getEnd();
                String startValue;
                String endValue;
                if (start != null && (startValue = toValue(start.getValue())) != null) {
                    sb.append(startValue);
                }
                if (end != null && (endValue = toValue(end.getValue())) != null) {
                    if (sb.length() > 0) {
                        sb.append('-');
                    }
                    sb.append(endValue);
                }
                if (sb.length() > 0) {
                    addElementType(dc.getCoverages(), sb.toString());
                }
            }
        }
        addLanguage(mods.getLanguage(), dc);
        return dc;
    }

}
