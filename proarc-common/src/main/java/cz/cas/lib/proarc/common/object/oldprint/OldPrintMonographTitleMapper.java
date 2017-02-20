package cz.cas.lib.proarc.common.object.oldprint;

import cz.cas.lib.proarc.common.mods.ndk.NdkMonographTitleMapper;
import cz.cas.lib.proarc.mods.ModsDefinition;

/**
 * @author Jakub Kremlacek
 */
public class OldPrintMonographTitleMapper extends NdkMonographTitleMapper {
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        //  mods/typeOfResource="text"
//        OldPrintMapperUtils.addTypeOfResource(mods);
        //  mods/genre="title"
    }
}
