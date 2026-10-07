package cz.cas.lib.proarc.common.object.technicalMetadata;

import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class CopyrightMdMapperTest {
    @Test void excludesAllFourPageModels() {
        for (String model : new String[] {"model:page", "model:ndkpage", "model:oldprintpage", "model:ndkaudiopage"}) {
            assertFalse(TechnicalMetadataMapper.supportsCopyrightMd(model));
            assertFalse(TechnicalMetadataMapper.supportsCopyrightMd("info:fedora/" + model));
        }
        for (String model : new String[] {"model:ndkperiodical", "model:ndkperiodicalvolume", "model:ndkperiodicalissue", "model:collection", "model:ndktrack"}) {
            assertTrue(TechnicalMetadataMapper.supportsCopyrightMd(model));
        }
    }

    @Test void rejectsImportObjects() throws Exception {
        LocalStorage.LocalObject object = new LocalStorage().create();
        TechnicalMetadataMapper mapper = new TechnicalMetadataMapper("model:ndkperiodicalissue", 42, object.getPid(), null, null);
        assertThrows(DigitalObjectException.class, () -> mapper.getMetadataAsXml(object, null, null, TechnicalMetadataMapper.COPYRIGHTMD));
    }
}
