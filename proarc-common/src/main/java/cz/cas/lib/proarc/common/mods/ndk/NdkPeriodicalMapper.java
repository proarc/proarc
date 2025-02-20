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

import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.DateOtherDefinition;
import cz.cas.lib.proarc.mods.Extent;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.IssuanceDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.SubjectDefinition;
import cz.cas.lib.proarc.mods.SubjectNameDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import cz.cas.lib.proarc.mods.UrlDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.List;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addElementType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNameIdentifier;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNonSort;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addOriginInfo;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addStringPlusLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addSubTitle;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addTitle;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillAbstract;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillLanguage;

/**
 *
 * @author Jan Pokorsky
 */
public class NdkPeriodicalMapper extends RdaNdkMapper {

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
        addGenre(mods);
        //  mods/originInfo/issuance="continuing"
        //  mods/originInfo/place/placeTerm/type="text"
        OriginInfoDefinition reqOriginInfo = null;
        List<OriginInfoDefinition> originInfos = mods.getOriginInfo();
        for (OriginInfoDefinition oi : originInfos) {
            reqOriginInfo = oi;
            List<IssuanceDefinition> issuances = oi.getIssuance();
            IssuanceDefinition reqIssuance = null;
            // zmena na serial - viz issue 1564
//            for (Iterator<IssuanceDefinition> it = issuances.iterator(); it.hasNext();) {
//                IssuanceDefinition issuance = it.next();
//                if (IssuanceDefinition.CONTINUING == issuance) {
//                    // replace SERIAL with CONTINUING to comply with NDK spec
//                    it.remove();
//                    continue;
//                }
//                if (IssuanceDefinition.SERIAL == issuance) {
//                    reqIssuance = issuance;
//                    break;
//                }
//            }
//            if (reqIssuance == null) {
//                issuances.add(0, IssuanceDefinition.SERIAL);
//            }
            List<PlaceDefinition> places = oi.getPlace();
            for (PlaceDefinition place : places) {
                List<PlaceTermDefinition> placeTerms = place.getPlaceTerm();
                for (PlaceTermDefinition placeTerm : placeTerms) {
                    if (placeTerm.getType() == null) {
                        placeTerm.setType(CodeOrText.TEXT);
                    }
                }
            }
            // sets type in element dateOther
            for(DateOtherDefinition dateOther : oi.getDateOther()){
                dateOther.setType(oi.getEventType());
            }
        }
//        if (reqOriginInfo == null) {
//            reqOriginInfo = new OriginInfoDefinition();
//            reqOriginInfo.getIssuance().add(IssuanceDefinition.SERIAL);
//            originInfos.add(reqOriginInfo);
//        }
        // mods/physicalDescription/form="print"
        // mods/physicalDescription/form@authority="marcform"
        List<PhysicalDescriptionDefinition> physicalDescriptions = mods.getPhysicalDescription();
        PhysicalDescriptionDefinition reqPhysicalDescription = null;
        FormDefinition reqForm = null;
        for (PhysicalDescriptionDefinition pd : physicalDescriptions) {
            reqPhysicalDescription = pd;
            List<FormDefinition> forms = pd.getForm();
            for (FormDefinition form : forms) {
                if ("print".equals(form.getValue()) || "electronic".equals(form.getValue())) {
                    if (!ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM.equals(form.getAuthority())) {
                        form.setAuthority(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM);
                    }
                    reqForm = form;
                }
                if (ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDAMEDIA.equals(form.getAuthority())) {
                    form.setType("media");
                } else if (ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDACARRIER.equals(form.getAuthority())) {
                    form.setType("carrier");
                } else {
                    form.setType(null);
                }
            }
            if (reqForm == null) {
                reqForm = newFormDefinition();
                forms.add(0, reqForm);
            }
        }
        if (reqPhysicalDescription == null) {
            reqForm = newFormDefinition();
            reqPhysicalDescription = new PhysicalDescriptionDefinition();
            reqPhysicalDescription.getForm().add(reqForm);
            physicalDescriptions.add(reqPhysicalDescription);
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
        fillAbstract(mods);
    }

    protected FormDefinition newFormDefinition() {
        FormDefinition reqForm = new FormDefinition();
        reqForm.setAuthority(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM);
        reqForm.setValue("print");
        return reqForm;
    }

    protected void addGenre(ModsDefinition mods) {
        //  mods/genre="title"
        MapperUtils.removeGenre(mods, Const.GENRE_ETITLE_WRONG_SYNTAX);
        MapperUtils.removeGenre(mods, Const.GENRE_ETITLE);
        MapperUtils.addGenre(mods, Const.GENRE_TITLE);
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

}
