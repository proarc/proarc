/*
 * Copyright (C) 2015 Jan Pokorsky
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

package cz.cas.lib.proarc.common.object.oldprint;

import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public class OldPrintPageMapper extends NdkPageMapper {

    private static final Logger LOG = Logger.getLogger(OldPrintPageMapper.class.getName());

    public static ResourceBundle getPageTypeLabels(Locale locale) {
        ResourceBundle rb = ResourceBundle.getBundle(
                BundleName.MODS_OLDPRINT_PAGE_TYPES.toString(),
                locale,
                ResourceBundle.Control.getNoFallbackControl(ResourceBundle.Control.FORMAT_PROPERTIES));
        return rb;
    }

    public static String getPageTypeLabel(String pageType, Locale locale) {
        if (pageType == null || pageType.isEmpty()) {
            pageType = NdkPageMapper.PAGE_TYPE_NORMAL;
        }
        try {
            return getPageTypeLabels(locale).getString(pageType);
        } catch (MissingResourceException ex) {
            LOG.warning("Missing page type resource for " + pageType + " locale " + locale);
            return pageType;
        }
    }

    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        //  mods/typeOfResource="text"
//        OldPrintMapperUtils.addTypeOfResource(mods);
    }

}
