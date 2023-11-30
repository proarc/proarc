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

package cz.cas.lib.proarc.common.object.oldprint;


import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.mods.ndk.MapperUtils;
import cz.cas.lib.proarc.common.mods.ndk.RdaNdkMapper;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addElementType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.createTitleString;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillAbstract;

/**
 * @author Lukas Sykora
 */
public class OldPrintOmnibusVolumeMapper extends RdaNdkMapper {
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        addGenre(mods);
        fillAbstract(mods);
        deleteOthers(mods);
        removeOtherTitleInfo(mods);
    }

    private void removeOtherTitleInfo(ModsDefinition mods) {
        TitleInfoDefinition titleInfoDefinition = null;
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            if (titleInfo != null && titleInfo.getType() == null) {
                titleInfoDefinition = titleInfo;
            }
        }
        if (titleInfoDefinition != null) {
            if (titleInfoDefinition.getTitle() != null && titleInfoDefinition.getTitle().size() > 0) {
                StringPlusLanguage title = titleInfoDefinition.getTitle().get(0);
                if (title != null && title.getValue() != null && !title.getValue().startsWith("Konvolut začínající dílem:")) {
                    title.setValue("Konvolut začínající dílem: " + title.getValue());
                }
            }
            mods.getTitleInfo().clear();
            mods.getTitleInfo().add(titleInfoDefinition);
        }
    }

    private void deleteOthers(ModsDefinition mods) {
        mods.getAbstract().clear();
        mods.getClassification().clear();
        mods.getExtension().clear();
        mods.getLanguage().clear();
        mods.getOriginInfo().clear();
        //mods.getPart().clear();
        mods.getPhysicalDescription().clear();
        //mods.getRelatedItem().clear();
        //mods.getSubject().clear();
        mods.getTableOfContents().clear();
        mods.getTargetAudience().clear();
        mods.getTypeOfResource().clear();
        mods.getNote().clear();
        //mods.getName().clear();
    }

    @Override
    protected String createObjectLabel(ModsDefinition mods) {
        return OldPrintMapperFactory.createObjectLabel(mods);
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            addElementType(dc.getTitles(), createTitleString(titleInfo));
        }
        addElementType(dc.getTypes(), getDcType());
        addLanguage(mods.getLanguage(), dc);
        addName(mods.getName(), dc.getCreators());
        return dc;
    }

    protected void addGenre(ModsDefinition mods) {
        MapperUtils.addGenre(mods, Const.GENRE_CONVOLUTE);
    }
}
