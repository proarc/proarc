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
package cz.cas.lib.proarc.common.i18n;

import cz.cas.lib.proarc.common.object.ValueMap;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.ResourceBundle.Control;

/**
 * Provides a bundle content as a value map.
 *
 * @author Jan Pokorsky
 */
public class BundleValueMap extends ValueMap<BundleValue> {

    public static ValueMap<BundleValue> fromBundle(BundleName bundle, Locale locale) {
        return fromBundle(bundle.getValueMapId(), bundle.toString(), locale);
    }

    /**
     * Creates a value map from a resource bundle.
     * @param mapId map ID
     * @param baseName resource bundle name
     * @param locale locale
     * @return the sorted value map
     */
    public static ValueMap<BundleValue> fromBundle(String mapId, String baseName, Locale locale) {
        Control control = ResourceBundle.Control.getNoFallbackControl(ResourceBundle.Control.FORMAT_PROPERTIES);
        ResourceBundle rb = ResourceBundle.getBundle(baseName, locale, control);
        ArrayList<BundleValue> items = new ArrayList<BundleValue>();
        for (String key : rb.keySet()) {
            items.add(new BundleValue(key, rb.getString(key)));
        }
        Collections.sort(items, new BundleValueComparator(locale));
        return new ValueMap<BundleValue>(mapId, items);
    }

    private static class BundleValueComparator implements Comparator<BundleValue> {

        private final Collator collator;

        public BundleValueComparator(Locale locale) {
            collator = Collator.getInstance(locale);
        }

        @Override
        public int compare(BundleValue o1, BundleValue o2) {
            return collator.compare(o1.getValue(), o2.getValue());
        }

    }

}
