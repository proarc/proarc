/*
 * Copyright (C) 2022 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.object.ndk;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.fedora.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.RelatedItemDefinition;
import java.util.Arrays;
import java.util.List;
import org.apache.commons.configuration.Configuration;

/**
 * Checks Mods rules.
 *
 * @author Lukas Sykora
 */
public class ModsRules {

    private String modelId;
    private ModsDefinition mods;
    private DigitalObjectValidationException exception;
    private NdkMapper.Context context;
    private AppConfiguration config;

    private static final String PROP_MODS_PHYSICAL_LOCATION_SIGLA = "metadata.mods.location.physicalLocation.sigla";
    private List<String> acceptableSiglaId;

    private static final String ERR_NDK_SUPPLEMENT_GENRE_TYPE ="Err_Ndk_Supplement_Genre_Type";
    private static final String ERR_NDK_PHYSICALLOCATION_SIGLA ="Err_Ndk_PhysicalLocation_Sigla";
    private static final String ERR_NDK_RELATEDITEM_PHYSICALLOCATION_SIGLA ="Err_Ndk_RelatedItem_PhysicalLocation_Sigla";

    private ModsRules() {}

    public ModsRules(String modelId, ModsDefinition mods, DigitalObjectValidationException ex, NdkMapper.Context context, AppConfiguration appConfiguration) {
        this.modelId = modelId;
        this.mods = mods;
        this.exception = ex;
        this.context = context;
        this.config = appConfiguration;
    }

    public void check() throws DigitalObjectValidationException{
        if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            checkGenreType(mods);
        }
        checkPhysicalLocation(mods.getLocation());
        checkRelatedItemPhysicalLocation(mods.getRelatedItem());

        if (!exception.getValidations().isEmpty()){
            throw exception;
        }
    }

    public void checkPhysicalLocation(List<LocationDefinition> locations) {
        checkPhysicalLocation(locations, ERR_NDK_PHYSICALLOCATION_SIGLA);
    }

    public void checkRelatedItemPhysicalLocation(List<RelatedItemDefinition> relatedItems) {
        for (RelatedItemDefinition relatedItem : relatedItems) {
            checkPhysicalLocation(relatedItem.getLocation(), ERR_NDK_RELATEDITEM_PHYSICALLOCATION_SIGLA);
        }
    }

    public void checkPhysicalLocation (List<LocationDefinition> locations, String bundleKey) {
        for (LocationDefinition location : locations) {
            for (PhysicalLocationDefinition physicalLocation : location.getPhysicalLocation()) {
                if ("siglaADR".equals(physicalLocation.getAuthority())) {
                    List<String> accepted = config.getModsOptions().getAcceptableSiglaId();
                    if (!accepted.contains(physicalLocation.getValue())) {
                        exception.addValidation("MODS rules", bundleKey, false, physicalLocation.getValue());
                    }
                }
            }
        }
    }

    private void checkGenreType(ModsDefinition mods) {
        String expectedType = getExpectedType();
        if (mods != null) {
            for (GenreDefinition genre : mods.getGenre()) {
                if (expectedType == null) {
                    continue; // nenalezen expected type
                } else if (!expectedType.equals(genre.getType())) {
                    exception.addValidation("MODS rules", ERR_NDK_SUPPLEMENT_GENRE_TYPE, false, expectedType, genre.getType());
                }
            }
        }
    }

    private String getExpectedType() {
        String parentModel = context.getParentModel();
        if (parentModel == null) {
            return null;
        } else if (NdkPlugin.MODEL_PERIODICALISSUE.equals(parentModel)) {
            return "issue_supplement";
        } else if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(parentModel)) {
            return "volume_supplement";
        }
        return null;
    }

    public static ModsRules getOptions(Configuration config) {
        ModsRules options = new ModsRules();

        String[] modsRules = config.getStringArray(PROP_MODS_PHYSICAL_LOCATION_SIGLA);
        options.setAcceptableSiglaId(Arrays.asList(modsRules));
        return options;
    }

    public List<String> getAcceptableSiglaId() {
        return acceptableSiglaId;
    }

    public void setAcceptableSiglaId(List<String> acceptableSiglaId) {
        this.acceptableSiglaId = acceptableSiglaId;
    }
}
