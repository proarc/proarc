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
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.DateOtherDefinition;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.List;

/**
 *
 * @author Lukas Sykora
 */
public class NdkMusicDocumentMapper extends NdkMapper {

    /**
     * Updates missing required attribute and elements.
     */
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);

        //  mods/typeOfResource="text" or fill with MARC21 06; see NDK
        List<TypeOfResourceDefinition> typeOfResources = mods.getTypeOfResource();
        TypeOfResourceDefinition reqTypeOfResource = null;
        for (TypeOfResourceDefinition typeOfResource : typeOfResources) {
            if ("text".equals(typeOfResource.getValue())) {
                reqTypeOfResource = typeOfResource;
                break;
            }
        }
        if (reqTypeOfResource == null) {
            TypeOfResourceDefinition type = new TypeOfResourceDefinition();
            type.setValue("text");
            typeOfResources.add(0, type);
        }
        //  mods/genre="title"
        addGenre(mods, "sound recording");
        //  mods/originInfo/issuance="continuing"
        //  mods/originInfo/place/placeTerm/type="text"
        List<OriginInfoDefinition> originInfos = mods.getOriginInfo();
        for (OriginInfoDefinition oi : originInfos) {

            List<PlaceDefinition> places = oi.getPlace();
            for (PlaceDefinition place : places) {
                List<PlaceTermDefinition> placeTerms = place.getPlaceTerm();
                for (PlaceTermDefinition placeTerm : placeTerms) {
                    if (placeTerm.getType() == null) {
                        System.out.println(CodeOrText.TEXT);
                        placeTerm.setType(CodeOrText.TEXT);
                    }
                }
            }
            // sets type in element dateOther
            for(DateOtherDefinition dateOther : oi.getDateOther()){
                dateOther.setType(oi.getEventType());
            }
        }
        // mods/physicalDescription/form="print"
        // mods/physicalDescription/form@authority="marcform"
        List<PhysicalDescriptionDefinition> physicalDescriptions = mods.getPhysicalDescription();
        for (PhysicalDescriptionDefinition pd : physicalDescriptions) {
            List<FormDefinition> forms = pd.getForm();
            for (FormDefinition form : forms) {
                if (ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDAMEDIA.equals(form.getAuthority())) {
                    form.setType("media");
                } else if (ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDACARRIER.equals(form.getAuthority())) {
                    form.setType("carrier");
                } else {
                    form.setType(null);
                }
            }
        }
        //  mods/classification@authority="udc"
        List<ClassificationDefinition> classifications = mods.getClassification();
        for (ClassificationDefinition classification : classifications) {
            repairAuthorityInClassification(classification);
        }
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
        // mods/language/languageTerm @type=code, @authority="iso639‐2b"
        fillLanguage(mods);

        fillRecordInfo(mods);
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        return dc;
    }
}
