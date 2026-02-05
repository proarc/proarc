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

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.DateOtherDefinition;
import cz.cas.lib.proarc.mods.Extent;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.TableOfContentsDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.List;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addElementType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNameIdentifier;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNonSort;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addOriginInfo;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addPartName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addPartNumber;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addStringPlusLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addSubTitle;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addTitle;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillLanguage;

/**
 *
 * @author Lukas Sykora
 */
public class NdkSoundCollectionMapper extends RdaNdkMapper {

    /**
     * Updates missing required attribute and elements.
     */
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);

        //  mods/genre="title"
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
            for (DateOtherDefinition dateOther : oi.getDateOther()) {
                dateOther.setTypeString(oi.getEventType());
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
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            StringBuilder title = new StringBuilder();
            addNonSort(title, titleInfo);
            addTitle(title, titleInfo);
            addSubTitle(title, titleInfo);
            addPartNumber(title, titleInfo);
            addPartName(title, titleInfo);
            addElementType(dc.getTitles(), title.toString());
        }

        addName(mods.getName(), dc.getCreators());
        addNameIdentifier(mods.getName(), dc.getCreators());
        if (mods.getTypeOfResource() != null && mods.getTypeOfResource().size() > 0) {
            addElementType(dc.getTypes(), mods.getTypeOfResource().get(0).getValue());
        }
        addElementType(dc.getTypes(), getDcType());
        addOriginInfo(mods.getOriginInfo(), dc);

        for (PhysicalDescriptionDefinition physicalDescription : mods.getPhysicalDescription()) {
            for (FormDefinition form : physicalDescription.getForm()) {
                addElementType(dc.getFormats(), form.getValue());
            }
            for (Extent extent : physicalDescription.getExtent()) {
                addElementType(dc.getCoverages(), extent.getValue());
            }
        }

        addStringPlusLanguage(dc.getDescriptions(), mods.getNote());

        for (LocationDefinition location : mods.getLocation()) {
            addStringPlusLanguage(dc.getSources(), location.getPhysicalLocation());
            addStringPlusLanguage(dc.getSources(), location.getShelfLocator());
        }
        for (TableOfContentsDefinition tableOfContents : mods.getTableOfContents()) {
            addElementType(dc.getDescriptions(), tableOfContents.getValue());
        }
        return dc;
    }
}
