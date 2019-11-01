package cz.cas.lib.proarc.common.object.chronicle;

import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import cz.cas.lib.proarc.common.mods.ndk.*;


import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * Creates default NDK mappers. Subclasses can implement own mappings.
 *
 * @author Lukas Sykora
 */
public class ChronicleMapperFactory extends  NdkMapperFactory{

    private static final Map<String, Supplier<NdkMapper>> mappers = new HashMap<>();

    static {
        mappers.put(ChroniclePlugin.MODEL_PAGE, NdkPageMapper::new);
        mappers.put(ModsCutomEditorType.EDITOR_PAGE, NdkPageMapper::new);
        mappers.put(ChroniclePlugin.MODEL_CHRONICLETITLE, ChronicleTitleMapper::new);
        mappers.put(ChroniclePlugin.MODEL_CHRONICLEVOLUME, ChronicleVolumeMapper::new);
        mappers.put(ChroniclePlugin.MODEL_CHRONICLESUPPLEMENT, ChronicleSupplementMapper::new);
    }

    /**
     * Gets a NDK mapper for the given model ID.
     *
     * @param modelId model ID
     * @return the mapper
     */
    @Override
    public NdkMapper get(String modelId) {
        Optional<Supplier<NdkMapper>> ndkMapper = Optional.ofNullable(mappers.get(modelId));
        return ndkMapper.map(s -> s.get()).orElseThrow(() -> new IllegalStateException("Unsupported model: " + modelId));
    }

}
