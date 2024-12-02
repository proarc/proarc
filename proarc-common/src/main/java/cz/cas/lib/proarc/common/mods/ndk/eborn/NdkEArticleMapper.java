/*
 * Copyright (C) 2018 Martin Rumanek
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

package cz.cas.lib.proarc.common.mods.ndk.eborn;

import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.MapperUtils;
import cz.cas.lib.proarc.common.mods.ndk.NdkArticleMapper;
import cz.cas.lib.proarc.mods.DigitalOriginDefinition;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

import java.util.ArrayList;
import java.util.List;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addDigitalOrigin;
import static cz.cas.lib.proarc.common.object.emods.BdmArticleMapper.checkNewFormDefinition;
import static cz.cas.lib.proarc.common.object.emods.BdmArticleMapper.setFormDefinition;

public class NdkEArticleMapper extends NdkArticleMapper {

    /**
     * Updates missing required attribute and elements.
     */
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        PhysicalDescriptionDefinition reqPhysicalDescription = null;

        List<PhysicalDescriptionDefinition> listPhysicalDescription = new ArrayList<>();
        for (PhysicalDescriptionDefinition pd : mods.getPhysicalDescription()) {
            for (FormDefinition form : pd.getForm()) {
                FormDefinition newFormDefinition = new FormDefinition();
                if ("bez média".equals(form.getValue())) {
                    setFormDefinition(form, newFormDefinition, "svazek");
                } else if ("počítač".equals(form.getValue())) {
                    setFormDefinition(form, newFormDefinition, "online zdroj");
                } else if ("jiný".equals(form.getValue()) && (form.getAuthority() == null || "rdamedia".equals(form.getAuthority()))) {
                    setFormDefinition(form, newFormDefinition, "jiný");
                } else {
                    if (form.getAuthority() == null) {
                        form.setAuthority("marcform");
                    }
                }
                PhysicalDescriptionDefinition pdd = new PhysicalDescriptionDefinition();
                pdd.getForm().add(newFormDefinition);
                listPhysicalDescription.add(pdd);
            }
        }
        for (PhysicalDescriptionDefinition pd : listPhysicalDescription) {
            if (checkNewFormDefinition(pd.getForm().get(0), mods)) {
                mods.getPhysicalDescription().add(pd);
            }
        }

        for (PhysicalDescriptionDefinition pd : mods.getPhysicalDescription()) {
            reqPhysicalDescription = pd;
        }
        if (reqPhysicalDescription == null) {
            reqPhysicalDescription = new PhysicalDescriptionDefinition();
            reqPhysicalDescription.getDigitalOrigin().add(DigitalOriginDefinition.BORN_DIGITAL);
            reqPhysicalDescription.getForm().add(newFormDefinition());
            mods.getPhysicalDescription().add(reqPhysicalDescription);
        } else {
            mods.getPhysicalDescription().stream().map(PhysicalDescriptionDefinition::getDigitalOrigin).filter(origin -> origin.isEmpty()).forEach(origin -> origin.add(DigitalOriginDefinition.BORN_DIGITAL));
            mods.getPhysicalDescription().stream().map(PhysicalDescriptionDefinition::getForm).filter(origin -> origin.isEmpty()).forEach(origin -> origin.add(newFormDefinition()));
        }
    }

    private FormDefinition newFormDefinition() {
        FormDefinition reqForm = new FormDefinition();
        reqForm.setAuthority(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM);
        reqForm.setValue("electronic");
        return reqForm;
    }

    @Override
    protected void addGenre(ModsDefinition mods) {
        //  mods/genre="electronic_article"
        MapperUtils.removeGenre(mods, "electronic article");
        MapperUtils.removeGenre(mods, Const.GENRE_ARTICLE);
        MapperUtils.addGenre(mods, Const.GENRE_EARTICLE);
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        for (PhysicalDescriptionDefinition physicalDescription : mods.getPhysicalDescription()) {
            addDigitalOrigin(dc.getDescriptions(), physicalDescription.getDigitalOrigin());
        }

        return dc;
    }
}
