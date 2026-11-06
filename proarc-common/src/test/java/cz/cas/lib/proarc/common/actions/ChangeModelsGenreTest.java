package cz.cas.lib.proarc.common.actions;

import cz.cas.lib.proarc.common.mods.ndk.NdkArticleMapper;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEArticleMapper;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.List;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ChangeModelsGenreTest {

    @Test
    void convertsNamedTypesInBothDirections() throws Exception {
        String[][] pairs = {
            {"annotation", "annotations"},
            {"mainArticle", "main article"},
            {"bibliographicalPortrait", "biographical portrait"},
            {"tableOfContents", "table of content"}
        };
        for (String[] pair : pairs) {
            assertConversion(NdkPlugin.MODEL_ARTICLE, NdkEbornPlugin.MODEL_EARTICLE, pair[0], pair[1]);
            assertConversion(NdkEbornPlugin.MODEL_EARTICLE, NdkPlugin.MODEL_ARTICLE, pair[1], pair[0]);
        }
    }

    @Test
    void replacesDigitizedOnlyTypesWithUnspecified() throws Exception {
        for (String type : List.of("afterword", "sheetMusic", "contributors", "summary", "study",
                "technicalPlanScheme", "editorial", "conclusion", "otherNote")) {
            assertConversion(NdkPlugin.MODEL_ARTICLE, NdkEbornPlugin.MODEL_EARTICLE, type, "unspecified");
        }
    }

    @Test
    void replacesElectronicOnlyTypesWithUnspecified() throws Exception {
        for (String type : List.of("cover", "colophon", "title page", "direktory", "peer-reviewed")) {
            assertConversion(NdkEbornPlugin.MODEL_EARTICLE, NdkPlugin.MODEL_ARTICLE, type, "unspecified");
        }
    }

    @Test
    void preservesCommonAndMissingTypesInBothDirections() throws Exception {
        for (String type : List.of("abstract", "bibliography", "dedication", "editorsNote",
                "advertisement", "obituary", "preface", "review", "index", "interview",
                "introduction", "news", "unspecified", "")) {
            assertConversion(NdkPlugin.MODEL_ARTICLE, NdkEbornPlugin.MODEL_EARTICLE, type, type);
            assertConversion(NdkEbornPlugin.MODEL_EARTICLE, NdkPlugin.MODEL_ARTICLE, type, type);
        }
        assertConversion(NdkPlugin.MODEL_ARTICLE, NdkEbornPlugin.MODEL_EARTICLE, null, null);
        assertConversion(NdkEbornPlugin.MODEL_EARTICLE, NdkPlugin.MODEL_ARTICLE, null, null);
    }

    @Test
    void preservesTypesBetweenElectronicModels() throws Exception {
        for (String type : List.of("annotations", "main article", "cover", "peer-reviewed")) {
            assertConversion(NdkEbornPlugin.MODEL_EARTICLE, BornDigitalModsPlugin.MODEL_ARTICLE, type, type);
            assertConversion(BornDigitalModsPlugin.MODEL_ARTICLE, NdkEbornPlugin.MODEL_EARTICLE, type, type);
        }

    }

    @Test
    void convertsEveryGenreAndAllowsAnEmptyList() throws Exception {
        ModsDefinition mods = new ModsDefinition();
        new ChangeModels(null, null, "uuid:test", NdkPlugin.MODEL_ARTICLE, NdkEbornPlugin.MODEL_EARTICLE).fixMods("uuid:test", mods, null);
        assertEquals(0, mods.getGenre().size());
        mods.getGenre().add(genre("mainArticle"));
        mods.getGenre().add(genre("summary"));
        new ChangeModels(null, null, "uuid:test", NdkPlugin.MODEL_ARTICLE, NdkEbornPlugin.MODEL_EARTICLE).fixMods("uuid:test", mods, null);
        assertEquals("main article", mods.getGenre().get(0).getType());
        assertEquals("unspecified", mods.getGenre().get(1).getType());
    }

    private static void assertConversion(String oldModel, String newModel, String source, String expected) throws Exception {
        ModsDefinition mods = new ModsDefinition();
        GenreDefinition genre = genre(source);
        if (NdkEbornPlugin.MODEL_EARTICLE.equals(oldModel)) {
            genre.setValue(Const.GENRE_EARTICLE);
        }
        mods.getGenre().add(genre);
        new ChangeModels(null, null, "uuid:test", oldModel, newModel).fixMods("uuid:test", mods, null);
        // Exercise the mapper step that used to remove the original typed genre.
        if (NdkEbornPlugin.MODEL_EARTICLE.equals(newModel)) {
            new ElectronicArticleMapper().normalizeGenre(mods);
        } else if (NdkPlugin.MODEL_ARTICLE.equals(newModel)
                || BornDigitalModsPlugin.MODEL_ARTICLE.equals(newModel)) {
            new ArticleMapper().normalizeGenre(mods);
        }
        assertEquals(1, mods.getGenre().size());
        assertEquals(genre, mods.getGenre().get(0));
        assertEquals(expected, genre.getType(), oldModel + " -> " + newModel + ": " + source);
        assertEquals(NdkEbornPlugin.MODEL_EARTICLE.equals(newModel)
                ? Const.GENRE_EARTICLE : Const.GENRE_ARTICLE, genre.getValue());
        assertEquals("local", genre.getAuthority());
    }

    private static GenreDefinition genre(String type) {
        GenreDefinition genre = new GenreDefinition();
        genre.setValue("article");
        genre.setType(type);
        genre.setAuthority("local");
        return genre;
    }

    private static class ArticleMapper extends NdkArticleMapper {
        void normalizeGenre(ModsDefinition mods) {
            addGenre(mods);
        }
    }

    private static class ElectronicArticleMapper extends NdkEArticleMapper {
        void normalizeGenre(ModsDefinition mods) {
            addGenre(mods);
        }
    }
}
