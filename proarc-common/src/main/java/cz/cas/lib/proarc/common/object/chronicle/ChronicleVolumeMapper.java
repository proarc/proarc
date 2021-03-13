package cz.cas.lib.proarc.common.object.chronicle;

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMonographVolumeMapper;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.RelatedItemDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addPid;

/**
 *
 * @author Lukas Sykora
 */
public class ChronicleVolumeMapper extends NdkMonographVolumeMapper {

    private final Set<String> ALLOW_IDENTIFIER_TYPE = Collections.unmodifiableSet(new HashSet<>(
            Arrays.asList("signature1", "signature2", "officialNumber", "inventaryNumber", "OtherNumber")));


    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        mods.setVersion(ModsUtils.VERSION);
        if (ctx.getPid() != null) {
            addPid(mods, ctx.getPid());
            checkUuidIdentifier(mods, ctx.getPid());
        }
        for (RecordInfoDefinition recordInfo : mods.getRecordInfo()) {
            for (StringPlusLanguagePlusAuthority descriptionStandard : recordInfo.getDescriptionStandard()) {
                descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR);
            }
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

    @Override
    public RdaModsWrapper toJsonObject(ModsDefinition mods, Context ctx) {
        NdkMapper.RdaModsWrapper wrapper = new NdkMapper.RdaModsWrapper();
        splitIdentifier(mods);
        wrapper.setMods(mods);
        if (mods.getRecordInfo().isEmpty() || mods.getRecordInfo().get(0).getDescriptionStandard().isEmpty()) {
            return wrapper;
        }
        String descriptionStandard = mods.getRecordInfo().get(0).getDescriptionStandard().get(0).getValue();
        mods.getRecordInfo().get(0).getDescriptionStandard().clear();
        if (descriptionStandard.equalsIgnoreCase(ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA)) {
            wrapper.setRdaRules(true);
        } else {
            wrapper.setRdaRules(false);
        }
        return wrapper;
    }

    @Override
    public ModsDefinition fromJsonObject(ObjectMapper jsMapper, String json, Context ctx) throws IOException {
        RdaModsWrapper wrapper = jsMapper.readValue(json, RdaModsWrapper.class);
        ModsDefinition mods = wrapper.getMods();
        mergeIdentifier(mods);
        StringPlusLanguagePlusAuthority descriptionStandard = new StringPlusLanguagePlusAuthority();
        if (wrapper.getRdaRules() != null && wrapper.getRdaRules()) {
            descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA);
        } else {
            descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR);
        }
        if (mods.getRecordInfo().isEmpty()) {
            RecordInfoDefinition recordInfo = new RecordInfoDefinition();
            recordInfo.getDescriptionStandard().add(0, descriptionStandard);
            mods.getRecordInfo().add(0, recordInfo);
        } else {
            mods.getRecordInfo().get(0).getDescriptionStandard().add(0, descriptionStandard);
        }
        return mods;
    }

    private void mergeIdentifier(ModsDefinition mods) {
        List<IdentifierDefinition> identifiers = new ArrayList<>();
        for (RelatedItemDefinition relatedItem : mods.getRelatedItem()) {
            for (IdentifierDefinition identifier : relatedItem.getIdentifier()) {
                identifiers.add(identifier);
            }
            relatedItem.getIdentifier().clear();
        }
        mods.getRelatedItem().clear();
        mods.getIdentifier().addAll(identifiers);
    }

    private void splitIdentifier(ModsDefinition mods) {
        List<IdentifierDefinition> identifiers = new ArrayList<>();
        List<IdentifierDefinition> relatedItemIdentifiers = new ArrayList<>();
        for (IdentifierDefinition identifier : mods.getIdentifier()) {
            if (ALLOW_IDENTIFIER_TYPE.contains(identifier.getType())) {
                relatedItemIdentifiers.add(identifier);
            } else {
                identifiers.add(identifier);
            }
        }
        mods.getIdentifier().clear();
        mods.getIdentifier().addAll(identifiers);
        if (mods.getRelatedItem().size() == 0) {
            mods.getRelatedItem().add(new RelatedItemDefinition());
        }
        mods.getRelatedItem().get(0).getIdentifier().addAll(relatedItemIdentifiers);
    }

    @Override
    protected String createObjectLabel(ModsDefinition mods) {
        StringBuilder label = new StringBuilder();
        /*for (TitleInfoDefinition ti : mods.getTitleInfo()) {
            if (toValue(ti.getType()) != null) {
                continue;
            }
            label.append(createTitleString(ti)).append(", ");
        }*/
        if (mods.getLocation().size() > 0 && mods.getLocation().get(0).getPhysicalLocation().size() > 0) {
            String value = mods.getLocation().get(0).getPhysicalLocation().get(0).getDisplayLabel();
            label.append(value != null ? value : "?");
            label.append(", ");
        }
        if (mods.getOriginInfo().size() > 0 && mods.getOriginInfo().get(0).getDateIssued().size() > 0) {
            label.append(mods.getOriginInfo().get(0).getDateIssued().get(0).getValue());
        }
        return label.length() > 0 ? label.toString() : null;
    }
}
