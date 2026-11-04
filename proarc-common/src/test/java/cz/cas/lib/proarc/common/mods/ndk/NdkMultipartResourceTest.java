package cz.cas.lib.proarc.common.mods.ndk;

import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import java.util.List;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class NdkMultipartResourceTest {

    @Test
    void savingMultipartDocumentsPreservesEveryExplicitResourceType() {
        for (String model : List.of(NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_MONOGRAPHUNIT,
                NdkEbornPlugin.MODEL_EMONOGRAPHUNIT)) {
            for (String value : List.of("text", "cartographic", "still image", "notated music")) {
                ModsDefinition mods = new ModsDefinition();
                TypeOfResourceDefinition resource = new TypeOfResourceDefinition();
                resource.setValue(value);
                mods.getTypeOfResource().add(resource);
                NdkMapper mapper = NdkMapper.get(model);
                NdkMapper.Context context = new NdkMapper.Context("uuid:volume");
                context.setOperation(NdkMetadataHandler.OPERATION_UPDATE);
                mapper.createMods(mods, context);
                mapper.createMods(mods, context);
                assertEquals(List.of(value), mods.getTypeOfResource().stream().map(TypeOfResourceDefinition::getValue).toList());
            }
        }
    }

    @Test
    void emptyVolumeRetainsTextDefaultAndDcModel() {
        NdkMapper mapper = NdkMapper.get(NdkPlugin.MODEL_MONOGRAPHUNIT);
        ModsDefinition mods = new ModsDefinition();
        NdkMapper.Context context = new NdkMapper.Context("uuid:volume");
        context.setOperation(NdkMetadataHandler.OPERATION_UPDATE);
        mapper.createMods(mods, context);
        assertEquals("text", mods.getTypeOfResource().get(0).getValue());
        assertEquals("model:monographunit", mapper.createDc(mods, context).getTypes().get(0).getValue());
    }
}
