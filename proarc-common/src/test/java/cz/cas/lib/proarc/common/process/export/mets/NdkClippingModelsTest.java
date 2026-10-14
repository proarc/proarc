package cz.cas.lib.proarc.common.process.export.mets;

import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.ndk.NdkClippingPlugin;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class NdkClippingModelsTest {

    @Test
    void allFundUnitModelsBecomeIndependentPackages() {
        Map<String, MetaModel> models = new NdkClippingPlugin().getModel().stream()
                .collect(Collectors.toMap(MetaModel::getPid, Function.identity()));

        for (String model : new String[]{NdkClippingPlugin.MODEL_CLIPPING_UNIT,
                NdkClippingPlugin.MODEL_CLIPPING_INDEX, NdkClippingPlugin.MODEL_CARD_INDEX}) {
            assertTrue(models.containsKey(model));
            assertEquals(Const.CLIPPING_UNIT, Const.typeMap.get(Const.FEDORAPREFIX + model));
            assertTrue(Const.PSPElements.contains(Const.typeMap.get(Const.FEDORAPREFIX + model)));
            assertEquals(0.1f, MetsContext.getPackageVersion(model));
        }
        assertEquals(Const.CLIPPING_COLLECTION,
                Const.typeMap.get(Const.FEDORAPREFIX + NdkClippingPlugin.MODEL_CLIPPING_COLLECTION));
        assertEquals(Const.CLIPPING_DIRECTORY,
                Const.typeMap.get(Const.FEDORAPREFIX + NdkClippingPlugin.MODEL_CLIPPING_DIRECTORY));
        assertFalse(Const.PSPElements.contains(Const.CLIPPING_COLLECTION));
        assertFalse(Const.PSPElements.contains(Const.CLIPPING_DIRECTORY));
    }

    @Test
    void mappersProduceStandardFundUnitGenres() {
        Map<String, String> genres = Map.of(
                NdkClippingPlugin.MODEL_CLIPPING_UNIT, "clipping",
                NdkClippingPlugin.MODEL_CLIPPING_INDEX, "clippingIndex",
                NdkClippingPlugin.MODEL_CARD_INDEX, "cardIndex",
                NdkClippingPlugin.MODEL_CLIPPING_COLLECTION, "unit collection",
                NdkClippingPlugin.MODEL_CLIPPING_DIRECTORY, "directory");
        genres.forEach((model, genre) -> {
            ModsDefinition mods = new ModsDefinition();
            NdkMapper.get(model).createMods(mods, new NdkMapper.Context("uuid:test", null));
            assertTrue(mods.getGenre().stream().anyMatch(value -> genre.equals(value.getValue())), model);
        });
    }
}
