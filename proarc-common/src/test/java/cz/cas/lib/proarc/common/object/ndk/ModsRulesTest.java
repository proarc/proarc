/*
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.object.ndk;

import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.storage.DigitalObjectValidationException;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import java.util.Set;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ModsRulesTest {

    @Test
    public void selectsPageTypesByParentModel() {
        assertSame(ModsRules.PERIODICAL_PAGE_PART_TYPES,
                ModsRules.getPagePartTypes(NdkPlugin.MODEL_PERIODICALISSUE));
        assertSame(ModsRules.PERIODICAL_PAGE_PART_TYPES,
                ModsRules.getPagePartTypes(NdkPlugin.MODEL_PERIODICALSUPPLEMENT));
        assertSame(ModsRules.MONOGRAPH_PAGE_PART_TYPES,
                ModsRules.getPagePartTypes(NdkPlugin.MODEL_MONOGRAPHVOLUME));
        assertSame(ModsRules.MONOGRAPH_PAGE_PART_TYPES, ModsRules.getPagePartTypes(null));
    }

    @Test
    public void keepsSpecialPageTypesForBothStandards() {
        for (String pageType : Set.of(
                "imgDisc", "manuscriptNotes", "calibrationTable", "fragmentsOfBookbinding", "scaleReference")) {
            assertTrue(ModsRules.MONOGRAPH_PAGE_PART_TYPES.contains(pageType), pageType);
            assertTrue(ModsRules.PERIODICAL_PAGE_PART_TYPES.contains(pageType), pageType);
        }
    }

    @Test
    public void keepsMonographOnlyTypesOutOfPeriodicals() {
        for (String pageType : Set.of("appendix", "frontispiece", "impressum", "edge", "imprimatur")) {
            assertTrue(ModsRules.MONOGRAPH_PAGE_PART_TYPES.contains(pageType), pageType);
            assertFalse(ModsRules.PERIODICAL_PAGE_PART_TYPES.contains(pageType), pageType);
        }
    }

    @Test
    public void rejectsUnsupportedPageTypes() {
        for (String pageType : Set.of(
                "Jacket", "abstract", "anotation", "audio", "bibliographicalPortrait", "booklet", "case",
                "editorial", "interview", "listOfSupplements", "mainArticle", "news", "obituary", "other",
                "review", "unknown")) {
            assertFalse(ModsRules.MONOGRAPH_PAGE_PART_TYPES.contains(pageType), pageType);
            assertFalse(ModsRules.PERIODICAL_PAGE_PART_TYPES.contains(pageType), pageType);
        }
        assertTrue(ModsRules.MONOGRAPH_PAGE_PART_TYPES.contains("jacket"));
        assertTrue(ModsRules.PERIODICAL_PAGE_PART_TYPES.contains("jacket"));
    }

    @Test
    public void validatesPageTypeOnlyWithKnownParent() {
        assertTrue(validatePageType(null, "appendix").getValidations().isEmpty());
        assertTrue(validatePageType(NdkPlugin.MODEL_MONOGRAPHVOLUME, "appendix").getValidations().isEmpty());
        assertFalse(validatePageType(NdkPlugin.MODEL_PERIODICALISSUE, "appendix").getValidations().isEmpty());
    }

    @Test
    public void acceptsOnlyEChapterGenreTypesFromStandard() {
        for (String genreType : Set.of(
                "tableOfContents", "advertisement", "abstract", "introduction", "review", "dedication",
                "bibliography", "editorsNote", "preface", "chapter", "article", "index", "unspecified")) {
            assertTrue(validateGenreType(NdkEbornPlugin.MODEL_ECHAPTER, genreType).getValidations().isEmpty(), genreType);
        }

        assertTrue(validateGenreType(NdkPlugin.MODEL_CHAPTER, "afterword").getValidations().isEmpty());
        assertTrue(validateGenreType(NdkEbornPlugin.MODEL_ECHAPTER, "afterword").getValidations().isEmpty());
    }

    private static DigitalObjectValidationException validateGenreType(String model, String genreType) {
        ModsDefinition mods = new ModsDefinition();
        GenreDefinition genre = new GenreDefinition();
        genre.setType(genreType);
        mods.getGenre().add(genre);
        DigitalObjectValidationException exception = new DigitalObjectValidationException(
                "uuid:test", null, "BIBLIO_MODS", "MODS validation", null);
        ModsRules rules = new ModsRules(model, mods, exception, (NdkMapper.Context) null, null);
        rules.checkGenreType(mods, model);
        return exception;
    }

    private static DigitalObjectValidationException validatePageType(String parentModel, String pageType) {
        ModsDefinition mods = new ModsDefinition();
        PartDefinition part = new PartDefinition();
        part.setType(pageType);
        mods.getPart().add(part);
        DigitalObjectValidationException exception = new DigitalObjectValidationException(
                "uuid:test", null, "BIBLIO_MODS", "MODS validation", null);
        ModsRules rules = new ModsRules(NdkPlugin.MODEL_PAGE, mods, exception, parentModel, null, null);
        rules.checkGenreType(mods, NdkPlugin.MODEL_PAGE);
        return exception;
    }
}
