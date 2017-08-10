/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.mods.ndk;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.*;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.List;

/**
 *
 * @author Jan Pokorsky
 */
public final class NdkPeriodicalVolumeMapper extends NdkMapper {
    public static final String GENRE = "volume";

    /**
     * Updates missing required attribute and elements.
     */
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);

        //  mods/genre="volume"
        addGenre(mods, GENRE);
    }

    @Override
    protected String createObjectLabel(ModsDefinition mods) {
        StringBuilder sb = new StringBuilder();
        String partNumber = findPartNumber(mods);
        String dateIssued = findDateIssued(mods);

        if (dateIssued != null) {
            sb.append(dateIssued);
        }
        if (partNumber != null) {
            if (sb.length() > 0) {
                sb.append(", ");
            }
            sb.append(partNumber);
        }
        return sb.toString();
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        String partNumber = findPartNumber(mods);
        String dateIssued = findDateIssued(mods);
        if (partNumber != null) {
            dc.getDescriptions().add(new ElementType(partNumber, null));
        }
        if (dateIssued == null) {
            dc.getDates().add(new ElementType(dateIssued, null));
        }
        addName(mods.getName(), dc.getCreators());
        dc.getTypes().add(new ElementType(GENRE, null));
        return dc;
    }

    static String findDateIssued(ModsDefinition mods) {
        List<OriginInfoDefinition> originInfos = mods.getOriginInfo();
        if (!originInfos.isEmpty()) {
            List<DateDefinition> issueDates = originInfos.get(0).getDateIssued();
            if (!issueDates.isEmpty()) {
                return toValue(issueDates.get(0).getValue());
            }
        }
        return null;
    }

}
