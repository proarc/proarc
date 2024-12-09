/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.object.emods;

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkArticleMapper;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.ModsWrapper;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.RelatedItemDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Maps born digital articles.
 *
 * @author Jan Pokorsky
 */
public class BdmArticleMapper extends NdkArticleMapper {

    /** {@code mods/genre/text()='article'}. */
    public static final String GENRE_ARTICLE_VALUE = "article";
    /** {@code mods/genre/@type='peer-reviewed'}. */
    public static final String GENRE_PEER_REVIEWED_TYPE = "peer-reviewed";
    private static final Logger LOG = Logger.getLogger(BdmArticleMapper.class.getName());

    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);

        /*for (GenreDefinition genre : mods.getGenre()) {
            if (genre.getType() == null  || genre.getType().isEmpty()) {
                genre.setType(GENRE_PEER_REVIEWED_TYPE);
            }
        }*/
        List<PhysicalDescriptionDefinition> listPhysicalDescription = new ArrayList<>();
        for (PhysicalDescriptionDefinition pd : mods.getPhysicalDescription()) {
            for (FormDefinition form : pd.getForm()) {
                FormDefinition newFormDefinition = new FormDefinition();
                if ("bez media".equals(form.getValue())) {
                    form.setValue("bez média");
                }
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

        setPageInterval(mods);
        fixUnreviewedGenre(mods);
        fillSecondRelatedItemType(mods);
    }

    public static void fillSecondRelatedItemType(ModsDefinition mods) {
        for (RelatedItemDefinition ri : mods.getRelatedItem()) {
            for (RelatedItemDefinition relatedItem : ri.getRelatedItem()) {
                if (relatedItem.getType() == null || relatedItem.getType().isEmpty()) {
                    relatedItem.setType("host");
                }
            }
        }
    }

    private void fixUnreviewedGenre(ModsDefinition mods) {
        int countArticle = 0;
        boolean isEmpty = false;

        for (GenreDefinition genre : mods.getGenre()) {
            if (Const.GENRE_ARTICLE.equals(genre.getValue())) {
                countArticle++;
                if (genre.getType() == null) {
                    isEmpty = true;
                }
            }
        }

        if (countArticle > 1 && isEmpty) {
            Iterator<GenreDefinition> iterator = mods.getGenre().iterator();
            while (iterator.hasNext()) {
                GenreDefinition genre = iterator.next();
                if (Const.GENRE_ARTICLE.equals(genre.getValue()) && genre.getType() == null) {
                    iterator.remove();
                }
            }
        }
    }

    private void setPageInterval(ModsDefinition mods) {
        try {
            if (mods.getPart().size() > 0
                    && mods.getPart().get(0).getExtent().size() > 0
                    && mods.getPart().get(0).getExtent().get(0).getStart() != null
                    && mods.getPart().get(0).getExtent().get(0).getStart().getValue() != null
                    && mods.getPart().get(0).getExtent().get(0).getStart().getValue().length() > 0
                    && mods.getPart().get(0).getExtent().get(0).getEnd() != null
                    && mods.getPart().get(0).getExtent().get(0).getEnd().getValue() != null
                    && mods.getPart().get(0).getExtent().get(0).getEnd().getValue().length() > 0) {
                return;
            } else if (mods.getRelatedItem().size() > 0
                    && mods.getRelatedItem().get(0).getPart().size() > 0
                    && mods.getRelatedItem().get(0).getPart().get(0).getExtent().size() > 0
                    && mods.getRelatedItem().get(0).getPart().get(0).getExtent().get(0).getStart() != null
                    && mods.getRelatedItem().get(0).getPart().get(0).getExtent().get(0).getStart().getValue() != null
                    && mods.getRelatedItem().get(0).getPart().get(0).getExtent().get(0).getStart().getValue().length() > 0
                    && mods.getRelatedItem().get(0).getPart().get(0).getExtent().get(0).getEnd() != null
                    && mods.getRelatedItem().get(0).getPart().get(0).getExtent().get(0).getEnd().getValue() != null
                    && mods.getRelatedItem().get(0).getPart().get(0).getExtent().get(0).getEnd().getValue().length() > 0) {

                PartDefinition part = mods.getRelatedItem().get(0).getPart().get(0);
                mods.getPart().clear();
                mods.getPart().add(part);
                mods.getRelatedItem().get(0).getPart().clear();
            }
        } catch (NullPointerException ex) {
            LOG.log(Level.INFO, "Mapper can not rewrite mods:relatedItem:part to mods:part, catch NullPointer " + ex);
        } catch (IndexOutOfBoundsException ex) {
            LOG.log(Level.INFO, "Mapper can not rewrite mods:relatedItem:part to mods:part, catch IndexOutOfBounds " + ex);
        }
    }

    /**
     * Checks formDefinition created by computer
     *
     * @return true if formDefinition is not empty and if formDefinition is unique
     */
    public static boolean checkNewFormDefinition(FormDefinition newForm, ModsDefinition mods) {
        for (PhysicalDescriptionDefinition pd : mods.getPhysicalDescription()) {
            for (FormDefinition form : pd.getForm()) {
                if (newForm.getValue() == null || form.getValue() == null) {
                    return false;
                }
                if (form.getValue().equals(newForm.getValue()) && form.getAuthority().equals(newForm.getAuthority()) && form.getType().equals(newForm.getType())) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Sets FormDefinitions
     *
     * @param form element created by user
     * @param newFormDefinition element created by computer
     */
    public static void setFormDefinition(FormDefinition form, FormDefinition newFormDefinition, String value) {
        form.setType("media");
        form.setAuthority("rdamedia");
        newFormDefinition.setType("carrier");
        newFormDefinition.setAuthority("rdacarrier");
        newFormDefinition.setValue(value);
    }

    @Override
    public ModsWrapper toJsonObject(ModsDefinition mods, Context ctx) {
        BdmModsWrapper mw = new BdmModsWrapper();
        mw.setMods(mods);
        GenreDefinition rGenre = null;
        for (GenreDefinition genre : mods.getGenre()) {
            if (GENRE_ARTICLE_VALUE.equals(genre.getValue()) && GENRE_PEER_REVIEWED_TYPE.equals(genre.getType())) {
                rGenre = genre;
                break;
            }
        }
        if (rGenre != null) {
            mods.getGenre().remove(rGenre);
            mw.setReviewed(Boolean.TRUE);
        } else {
            mw.setReviewed(Boolean.FALSE);
        }
        if (mods.getRecordInfo().isEmpty() || mods.getRecordInfo().get(0).getDescriptionStandard().isEmpty()){
            return mw;
        } else {
           String descriptionStandard = mods.getRecordInfo().get(0).getDescriptionStandard().get(0).getValue();
           if (descriptionStandard.equals(ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA)) {
               mw.setRdaRules(true);
           } else {
               mw.setRdaRules(false);
           }
            mods.getRecordInfo().get(0).getDescriptionStandard().clear();
           return mw;
        }
    }

    @Override
    public ModsDefinition fromJsonObject(ObjectMapper jsMapper, String json, Context ctx) throws IOException {
        BdmModsWrapper wrapper = jsMapper.readValue(json, BdmModsWrapper.class);
        ModsDefinition mods = wrapper.getMods();
        if (wrapper.getReviewed() != null && wrapper.getReviewed()) {
            GenreDefinition reviewed = new GenreDefinition();
            reviewed.setValue(GENRE_ARTICLE_VALUE);
            reviewed.setType(GENRE_PEER_REVIEWED_TYPE);
            mods.getGenre().add(0, reviewed);
        }
        StringPlusLanguagePlusAuthority descriptionStandard = new StringPlusLanguagePlusAuthority();
        if (wrapper.getRdaRules() != null && wrapper.getRdaRules()) {
            descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA);
        } else {
            descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR);
        }
        mods.getRecordInfo().get(0).getDescriptionStandard().add(0, descriptionStandard);
        return mods;
    }

    public static class BdmModsWrapper extends ModsWrapper {

        /**
         * A reviewed article. {@code mods/genre[@type'peer-reviewed' and text()='article'] }
         */
        private Boolean reviewed;
        private Boolean rdaRules;

        public Boolean getRdaRules() {
            return rdaRules == null ? true : rdaRules;
        }

        public void setRdaRules(Boolean rdaRules) {
            this.rdaRules = rdaRules;
        }

        public Boolean getReviewed() {
            return reviewed;
        }

        public void setReviewed(Boolean reviewed) {
            this.reviewed = reviewed;
        }
    }

}
