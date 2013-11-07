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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.presenter;

import com.google.gwt.core.client.JsonUtils;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Place tokenizer to facilitate persistence in JSON format.
 *
 * @author Jan Pokorsky
 */
public final class JsonTokenizer {

    private static final Logger LOG = Logger.getLogger(JsonTokenizer.class.getName());

    public static JSONObject parseObject(String token) {
        JSONValue val = parse(token);
        return val == null ? null : val.isObject();
    }

    public static JSONValue parse(String token) {
        try {
            if (JsonUtils.safeToEval(token)) {
                JSONValue jsonv = JSONParser.parseStrict(token);
                return jsonv;
            }
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, token, ex);
        }
        return null;
    }

    public static void putString(JSONObject json, String name, String value) {
        if (value != null) {
            json.put(name, new JSONString(value));
        }
    }

    public static String getString(JSONObject json, String name) {
        JSONValue jsonVal = json.get(name);
        JSONString jsonString = (jsonVal == null) ? null : jsonVal.isString();
        return (jsonString == null) ? null : jsonString.stringValue();
    }

}
