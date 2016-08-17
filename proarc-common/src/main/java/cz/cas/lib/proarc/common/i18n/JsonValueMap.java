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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.MapType;
import com.fasterxml.jackson.databind.type.TypeFactory;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.object.ValueMap;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle.Control;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Holds JSON content as a value map. It expects attribute {@code data} as
 * a list of mappings.
 *
 * <p/>Later it could include field descriptions like SmartGWT datasource.
 *
 * @author Jan Pokorsky
 */
public class JsonValueMap extends ValueMap<Map<String, Object>>{

    private static final JsonControl CONTROL = new JsonControl();
    private static final Logger LOG = Logger.getLogger(JsonValueMap.class.getName());

    public static JsonValueMap fromBundle(BundleName bundle, Locale locale) {
        JsonValueMap jvm = new JsonValueMap();
        jvm.setMapId(bundle.getValueMapId());

        URL resource = findResource(bundle.toString(), CONTROL, locale);
        if (resource == null) {
            return jvm;
        }
        ObjectMapper om = JsonUtils.defaultObjectMapper();
        try {
            TypeFactory typeFactory = om.getTypeFactory();
            MapType jsonType = typeFactory.constructMapType(Map.class, String.class, Object.class);
            Map<String, Object> jsonMap = om.readValue(resource, jsonType);
            if (LOG.isLoggable(Level.FINE)) {
                LOG.fine(String.valueOf(jsonMap));
            }
            Object data = jsonMap.get("data");
            jvm.setValues((List<Map<String, Object>>) data);
        } catch (Exception ex) {
            LOG.log(Level.WARNING, "Cannot read resource " + resource, ex);
        }
        return jvm;
    }

    private static URL findResource(String basename, Control ctrl, Locale locale) {
        String format = ctrl.getFormats(basename).get(0);
        String resourceName = '/' + ctrl.toResourceName(ctrl.toBundleName(basename, locale), format);
        URL resource = JsonValueMap.class.getResource(resourceName);
        if (resource == null) {
            resourceName = '/' + ctrl.toResourceName(ctrl.toBundleName(basename, Locale.ROOT), format);
            resource = JsonValueMap.class.getResource(resourceName);
        }
        if (resource == null) {
            LOG.log(Level.WARNING, null, new IllegalStateException(
                    "Cannot find resource " + basename + ", " + locale));
        }
        return resource;
    }

    public static class JsonControl extends Control {

        public static final List<String> FORMAT_JSON =
                Collections.unmodifiableList(Arrays.asList("json"));

        @Override
        public List<String> getFormats(String baseName) {
            return FORMAT_JSON;
        }

    }
}
