package cz.cas.lib.proarc.common.object.model;

import cz.cas.lib.proarc.common.object.chronicle.ChroniclePlugin;
import cz.cas.lib.proarc.common.object.graphic.GraphicPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkClippingPlugin;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MetaModelUtilsTest {

    @Test
    void loadsModelsFromAllPlugins() {
        assertTrue(Arrays.asList(MetaModelUtils.ALL_MODELS)
                .contains(NdkClippingPlugin.MODEL_CLIPPING_COLLECTION));
        assertTrue(Arrays.asList(MetaModelUtils.LEAF_MODELS)
                .contains(ChroniclePlugin.MODEL_PAGE));
        assertTrue(Arrays.asList(MetaModelUtils.TOP_MODELS)
                .contains(GraphicPlugin.MODEL_GRAPHIC));
    }

    @Test
    void removesDuplicateModelPids() {
        assertEquals(MetaModelUtils.ALL_MODELS.length,
                new HashSet<>(Arrays.asList(MetaModelUtils.ALL_MODELS)).size());
    }

    @Test
    void createsTopModelsComplement() {
        HashSet<String> allModels = new HashSet<>(Arrays.asList(MetaModelUtils.ALL_MODELS));
        HashSet<String> topModels = new HashSet<>(Arrays.asList(MetaModelUtils.TOP_MODELS));
        HashSet<String> nonTopModels = new HashSet<>(Arrays.asList(MetaModelUtils.NON_TOP_MODELS));

        assertTrue(Collections.disjoint(topModels, nonTopModels));
        topModels.addAll(nonTopModels);
        assertEquals(allModels, topModels);
    }
}
