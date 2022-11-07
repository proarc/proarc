/*
 * Copyright (C) 2018 Lukas Sykora
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

import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.Text;
import java.util.List;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.Logger;

/**
 *
 * @author Lukas Sykora
 */
public class NdkAudioPageMapper extends NdkPageMapper {

    /** {@code /mods/part@type} */
    public static final String AUDIO_TYPE_NORMAL = "audio";

    private static final Logger LOG = Logger.getLogger(NdkAudioPageMapper.class.getName());

    public static ResourceBundle getPageTypeLabels(Locale locale) {
        ResourceBundle rb = ResourceBundle.getBundle(
                BundleName.MODS_AUDIO_PAGE_TYPES.toString(),
                locale,
                ResourceBundle.Control.getNoFallbackControl(ResourceBundle.Control.FORMAT_PROPERTIES));
        return rb;
    }

    public static String getPageTypeLabel(String pageType, Locale locale) {
        if (pageType == null || pageType.isEmpty())  {
            return AUDIO_TYPE_NORMAL;
        }
        try {
            return getPageTypeLabels(locale).getString(pageType);
        } catch (MissingResourceException ex) {
            LOG.warning("Missing page type resource for " + pageType + " locale " + locale);
            return AUDIO_TYPE_NORMAL;
        }
    }

    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        mods.getTypeOfResource().clear();
    }

    @Override
    public Page toJsonObject(ModsDefinition mods, Context ctx) {
        Page page = new Page();
        List<PartDefinition> parts = mods.getPart();
        if (!parts.isEmpty()) {
            PartDefinition part = parts.get(0);
            List<Text> partTexts = part.getText();
            if (!partTexts.isEmpty()) {
                Text partText = partTexts.get(0);
                page.setPhysicalDescription(partText.getValue());
            }
        }
        if (!mods.getPart().isEmpty()) {
            String pageIndex = null;
            String pageNumber = null;
            String pageType = null;
            for (PartDefinition part : mods.getPart()) {
                if (pageIndex == null) {
                    pageIndex = getNumber(getDetail(part.getDetail(), NUMBER_TYPE_PAGE_INDEX));
                }
                if (pageNumber == null) {
                    pageNumber = getNumber(getDetail(part.getDetail(), NUMBER_TYPE_PAGE_NUMBER));
                }
                if (pageType == null) {
                    if (part.getType() != null) {
                        pageType = part.getType();
                    }
                }
            }
            if (pageType == null) {
                pageType = AUDIO_TYPE_NORMAL;
            }
            page.setType(pageType);
            page.setNumber(pageNumber);
            page.setIndex(pageIndex);
        } else {
            page.setType(AUDIO_TYPE_NORMAL);
        }
        page.setIdentifiers(getIdentifierItems(mods.getIdentifier()));
        return page;
    }

    @Override
    protected String createObjectLabel(ModsDefinition mods) {
        Page page = toJsonObject(mods, null);
        StringBuilder sb = new StringBuilder();
        if (page.getIndex() != null) {
            sb.append(page.getIndex());
        } else {
            sb.append('?');
        }
        return sb.toString();
    }

}
