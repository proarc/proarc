package cz.cas.lib.proarc.common.mods.ndk;

import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.mods.ModsDefinition;

/** MODS and DC mapping for a collection of related catalog cards. */
public class NdkCardIndexMapper extends NdkClippingUnitMapper {

    @Override
    protected void addGenre(ModsDefinition mods) {
        MapperUtils.addGenre(mods, Const.GENRE_CARD_INDEX);
    }
}
