package cz.cas.lib.proarc.common.object.chronicle;

import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.ndk.NdkMonographVolumeMapper;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addPid;

/**
 *
 * @author Lukas Sykora
 */
public class ChronicleVolumeMapper extends NdkMonographVolumeMapper {

    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        mods.setVersion(ModsUtils.VERSION);
        if (ctx.getPid() != null) {
            addPid(mods, ctx.getPid());
            checkUuidIdentifier(mods, ctx.getPid());
        }
    }

    private void checkUuidIdentifier(ModsDefinition mods, String pid) {
        String uuid = FoxmlUtils.pidAsUuid(pid);
        for (IdentifierDefinition id : mods.getIdentifier()) {
            if ("uuid".equals(id.getType()) && !uuid.equals(id.getValue())) {
                id.setInvalid("yes");
            }
        }
    }

    @Override
    protected void addGenre(ModsDefinition mods) {
    }
}
