/*
 * Copyright (C) 2012 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.webapp.shared.rest.LocalizationResourceApi;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.ResourceBundle.Control;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 *
 * @author Jan Pokorsky
 */
@Path(LocalizationResourceApi.PATH)
public class LocalizationResource {

    private static final Logger LOG = Logger.getLogger(LocalizationResource.class.getName());

    private final HttpHeaders httpHeaders;

    public LocalizationResource(
            @Context HttpHeaders httpHeaders
            ) {
        this.httpHeaders = httpHeaders;
    }

    /**
     * Gets localization bundle.
     * 
     * @param bundleNames name of bundle. If {@code null} all bundles are included.
     * @param locale optional locale. If {@code null} HTTP headers are queried for acceptable language
     * @param sorted optional flag to partially sort result bundle items by value.
     *              Default is {@code true}.
     * @return the bundle
     */
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Item> getBundle(
            @QueryParam(LocalizationResourceApi.ITEM_BUNDLENAME) Set<BundleName> bundleNames,
            @DefaultValue("")
            @QueryParam(LocalizationResourceApi.GETBUNDLE_LOCALE_PARAM) String locale,
            @DefaultValue("true")
            @QueryParam(LocalizationResourceApi.GETBUNDLE_SORTED_PARAM) boolean sorted) {

        if (bundleNames == null || bundleNames.isEmpty()) {
            bundleNames = EnumSet.allOf(BundleName.class);
        }
        Locale localeObj;
        if (locale == null || locale.isEmpty()) {
            List<Locale> acceptableLanguages = httpHeaders.getAcceptableLanguages();
            localeObj = acceptableLanguages.isEmpty() ? Locale.ENGLISH : acceptableLanguages.get(0);
        } else {
            localeObj = new Locale(locale);
        }

        Control control = ResourceBundle.Control.getNoFallbackControl(ResourceBundle.Control.FORMAT_PROPERTIES);
        ArrayList<Item> result = new ArrayList<Item>();
        for (BundleName bundleName : bundleNames) {
            try {
                if (!BundleName.PROPERTIES.equals(bundleName.getFormat())) {
                    continue;
                }
                result.addAll(readBundle(bundleName, localeObj, control, sorted));
            } catch (MissingResourceException ex) {
                LOG.log(Level.WARNING, bundleNames.toString(), ex);
                throw RestException.plainNotFound(
                        LocalizationResourceApi.ITEM_BUNDLENAME,
                        bundleName.toString());
            }
        }
        return new SmartGwtResponse<Item>(result);
    }

    private ArrayList<Item> readBundle(BundleName bundleName, Locale localeObj, Control control, boolean sorted) {
        // to read properties file in UTF-8 use PropertyResourceBundle(Reader)
        ResourceBundle rb = ResourceBundle.getBundle(bundleName.toString(), localeObj, control);
        ArrayList<Item> result = new ArrayList<Item>();
        for (String key : rb.keySet()) {
            result.add(new Item(key, rb.getString(key), bundleName.toString()));
        }
        if (sorted) {
            Collections.sort(result, new LocalizedItemComparator(localeObj));
        }
        return result;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Item {

        @XmlElement(name = LocalizationResourceApi.ITEM_KEY)
        private String key;
        @XmlElement(name = LocalizationResourceApi.ITEM_VALUE)
        private String value;
        @XmlElement(name = LocalizationResourceApi.ITEM_BUNDLENAME)
        private String bundleName;

        public Item(String key, String value, String bundleName) {
            this.key = key;
            this.value = value;
            this.bundleName = bundleName;
        }

        public Item() {
        }

        public String getKey() {
            return key;
        }

        public void setKey(String key) {
            this.key = key;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public String getBundleName() {
            return bundleName;
        }

        public void setBundleName(String bundleName) {
            this.bundleName = bundleName;
        }

        @Override
        public String toString() {
            return "Item{" + "code=" + key + ", value=" + value + '}';
        }
    }

    private static final class LocalizedItemComparator implements Comparator<Item> {

        private final Collator collator;

        public LocalizedItemComparator(Locale locale) {
            collator = Collator.getInstance(locale);
        }

        @Override
        public int compare(Item o1, Item o2) {
            return collator.compare(o1.value, o2.value);
        }

    }

}
