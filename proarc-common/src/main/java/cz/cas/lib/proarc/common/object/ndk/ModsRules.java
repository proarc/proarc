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

import cz.cas.lib.proarc.common.fedora.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;

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
    private static final String PROP_MODS_RULES = "metadata.mods.rules";

    private static final String ERR_NDK_SUPPLEMENT_GENRE_TYPE_ISSUE ="Err_Ndk_Supplement_Genre_Type_Issue";
    private static final String ERR_NDK_SUPPLEMENT_GENRE_TYPE_VOLUME ="Err_Ndk_Supplement_Genre_Type_Volume";

    ModsRules(String modelId, ModsDefinition mods, DigitalObjectValidationException ex, NdkMapper.Context context) {
        this.modelId = modelId;
        this.mods = mods;
        this.exception = ex;
        this.context = context;
    }

    public void check() throws DigitalObjectValidationException{
        if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            checkGenreType(mods);
        }
        if (!exception.getValidations().isEmpty()){
            throw exception;
        }
    }

    private void checkGenreType(ModsDefinition mods) {
        String expectedType = getExpectedType();
        if (mods != null) {
            for (GenreDefinition genre : mods.getGenre()) {
                if (expectedType == null) {
                    continue; // nenalezen expected type
                } else if (!expectedType.equals(genre.getType())) {
                    exception.addValidation("MODS rules", ERR_NDK_SUPPLEMENT_GENRE_TYPE_ISSUE, expectedType, genre.getType());
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
}
