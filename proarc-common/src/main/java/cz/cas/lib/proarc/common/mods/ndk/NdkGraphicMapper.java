/*
 * Copyright (C) 2024 Lukas Sykora
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

import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.mods.CartographicsDefinition;
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.Extent;
import cz.cas.lib.proarc.mods.ExtentDefinition;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
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
import cz.cas.lib.proarc.mods.UrlDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.Collections;
import java.util.List;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addElementType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNameIdentifier;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNameWithEtal;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addOriginInfo;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addStringPlusLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.createTitleString;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillAbstract;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.toValue;

/**
 *
 * @author Lukas Sykora
 */
public final class NdkGraphicMapper extends RdaNdkMapper {

    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);

        // name/role/roleTerm@type="CODE"
        // name/role/roleTerm@authority="marcrelator"
        for (NameDefinition name : mods.getName()) {
            for (RoleDefinition role : name.getRoleDefinition()) {
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
        // genre="picture"
        addGenre(mods);
        addNameWithEtal(mods);
        // mods/language/languageTerm @type=code, @authority="iso639‐2b"
        fillLanguage(mods);
        //  mods/classification@authority="udc"
        List<ClassificationDefinition> classifications = mods.getClassification();
        for (ClassificationDefinition classification : classifications) {
            repairAuthorityInClassification(classification);
        }
        fillAbstract(mods);
    }

    protected void addGenre(ModsDefinition mods) {
        //  mods/genre="picture"
        MapperUtils.addGenre(mods, Const.GENRE_GRAPHIC);
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
        addStringPlusLanguage(dc.getDescriptions(), mods.getAbstract());
        addStringPlusLanguage(dc.getDescriptions(), mods.getNote());
        for (SubjectDefinition subject : mods.getSubject()) {
            addStringPlusLanguage(dc.getSubjects(), subject.getTopic());
            addStringPlusLanguage(dc.getSubjects(), subject.getGeographic());
            addStringPlusLanguage(dc.getSubjects(), subject.getTemporal());
            for (SubjectNameDefinition subjectName : subject.getName()) {
                addStringPlusLanguage(dc.getSubjects(), subjectName.getNamePart());
            }
            for (CartographicsDefinition cartographics : subject.getCartographics()) {
                addStringPlusLanguage(dc.getCoverages(), Collections.singletonList(cartographics.getScale()));
                addStringPlusLanguage(dc.getCoverages(), cartographics.getCoordinates());
            }
        }
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
        for (PhysicalDescriptionDefinition physicalDesc : mods.getPhysicalDescription()) {
            for (FormDefinition form : physicalDesc.getForm()) {
                addElementType(dc.getFormats(), form.getValue());
            }
            for (Extent extent : physicalDesc.getExtent()) {
                addElementType(dc.getFormats(), extent.getValue());
            }
        }
        addOriginInfo(mods.getOriginInfo(), dc);
        addLanguage(mods.getLanguage(), dc);
        for (LocationDefinition location : mods.getLocation()) {
            for (UrlDefinition url : location.getUrl()) {
                addElementType(dc.getSources(), url.getValue());
            }
            addStringPlusLanguage(dc.getSources(), location.getPhysicalLocation());
            addStringPlusLanguage(dc.getSources(), location.getShelfLocator());
        }
        return dc;
    }

}
