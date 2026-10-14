package cz.cas.lib.proarc.common.object.ndk;

import cz.cas.lib.proarc.common.mods.ndk.NdkMapperFactory;
import cz.cas.lib.proarc.common.mods.ndk.NdkSoundCollectionMapper;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import java.util.Set;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

public class NdkAudioPluginTest {

    @Test
    public void registersNewSoundCollectionsWithTheMusicDocumentMapper() {
        NdkAudioPlugin plugin = new NdkAudioPlugin();
        Set<String> modelIds = plugin.getModel().stream().map(MetaModel::getPid).collect(Collectors.toSet());
        NdkMapperFactory factory = new NdkMapperFactory();

        for (String model : new String[]{NdkAudioPlugin.MODEL_AUDIO_DISC, NdkAudioPlugin.MODEL_DIGITAL_AUDIO}) {
            assertTrue(modelIds.contains(model));
            assertTrue(NdkAudioPlugin.isNdkAudioModel(model));
            assertTrue(factory.get(model) instanceof NdkSoundCollectionMapper);
        }
    }
}
