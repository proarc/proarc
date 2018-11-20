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
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.Extent;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.RoleDefinition;
import cz.cas.lib.proarc.mods.RoleTermDefinition;
import cz.cas.lib.proarc.mods.SubjectDefinition;
import cz.cas.lib.proarc.mods.SubjectNameDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.mods.UrlDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.List;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addElementType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNonSort;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addOriginInfo;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addStringPlusLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addSubTitle;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addTitle;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.findPartNumber;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.toValue;

/**
 *
 * @author Jan Pokorsky
 */
public class NdkPeriodicalIssueMapper extends NdkMapper {

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
        // genre="issue"
        GenreDefinition genre = addGenre(mods);
        // genre@type="normal" if null
        if (genre.getType() == null) {
            genre.setType("normal");
        }
        // mods/language/languageTerm @type=code, @authority="iso639‐2b"
        fillLanguage(mods);
        //  mods/location/physicalLocation@authority="siglaADR"
        List<LocationDefinition> locations = mods.getLocation();
        for (LocationDefinition location : locations) {
            List<PhysicalLocationDefinition> physicals = location.getPhysicalLocation();
            for (PhysicalLocationDefinition physical : physicals) {
                if (physical.getAuthority() == null) {
                    physical.setAuthority("siglaADR");
                }
            }
        }
        // mods/part@type=="issue"
        for (PartDefinition part : mods.getPart()) {
            if (part.getDetail().size() > 0 && part.getDetail().get(0).getCaption().size() > 0
                    && part.getDetail().get(0).getCaption().get(0).getValue() != null) {
                part.setType("issue");
            }
            if (!"issue".equals(part.getType())) {
                mods.getPart().clear();
                break;
            }
        }
    }

    protected GenreDefinition addGenre(ModsDefinition mods) {
        //  mods/genre="volume"
       return MapperUtils.addGenre(mods, Const.GENRE_ISSUE);
    }

    @Override
    protected String createObjectLabel(ModsDefinition mods) {
        String partNumber = findPartNumber(mods);
        partNumber = partNumber == null ? "?" : partNumber;
        String dateIssued = findFullDateIssued(mods);
        dateIssued = dateIssued == null ? "?" : dateIssued;
        return String.format("%s, %s", partNumber, dateIssued);
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            StringBuilder title = new StringBuilder();
            addNonSort(title, titleInfo);
            addTitle(title, titleInfo);
            addSubTitle(title, titleInfo);
            addElementType(dc.getTitles(), title.toString());

            addStringPlusLanguage(dc.getDescriptions(), titleInfo.getPartNumber());
            addStringPlusLanguage(dc.getDescriptions(), titleInfo.getPartName());
        }
        addName(mods.getName(), dc.getCreators());
        addElementType(dc.getTypes(), getDcType());
        addOriginInfo(mods.getOriginInfo(), dc);
        addLanguage(mods.getLanguage(), dc);
        for (PhysicalDescriptionDefinition physicalDesc : mods.getPhysicalDescription()) {
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
        for (LocationDefinition location : mods.getLocation()) {
            for (UrlDefinition url : location.getUrl()) {
                addElementType(dc.getSources(), url.getValue());
            }
            addStringPlusLanguage(dc.getSources(), location.getPhysicalLocation());
            addStringPlusLanguage(dc.getSources(), location.getShelfLocator());
        }
        return dc;
    }

    /** Searches first dateIssued without {@code @point}. */
    private static String findFullDateIssued(ModsDefinition mods) {
        List<OriginInfoDefinition> originInfos = mods.getOriginInfo();
        for (OriginInfoDefinition originInfo : originInfos) {
            if (!originInfo.getDateIssued().isEmpty()) {
                DateDefinition dateIssued = originInfo.getDateIssued().get(0);
                if (dateIssued.getPoint() == null) {
                    return toValue(dateIssued.getValue());
                }
            }
        }
        return null;
    }

}
