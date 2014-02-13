/*
 * Copyright (C) 2013 Pavel Stastny
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
package cz.cas.lib.proarc.authentication;

import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupString;

import cz.cas.lib.proarc.authentication.utils.IOUtils;

public class StringTemplateTest {

    @Test
    public void testTemplate() throws IOException {
        URL urlRes = IOUtils.class.getClassLoader()
                .getResource("loginfile.stg");
        InputStream isStream = urlRes.openStream();
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        IOUtils.copyStreams(isStream, bos);
        String str = new String(bos.toByteArray(), "UTF-8");
        STGroup stGroup = new STGroupString(str, str, '$', '$');
        ST html = stGroup.getInstanceOf("html");
        html.add("error", true);
        html.add("url", "http://localhost:8080/proarc");
        assertNotNull(html.render());
    }
}
