/*
 * Copyright (C) 2014 Robert Simonovsky
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

package cz.cas.lib.proarc.common.export.mets;

import java.io.InputStream;
import java.util.HashMap;

import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSInput;
import org.w3c.dom.ls.LSResourceResolver;

public class MetsLSResolver implements LSResourceResolver {

    static final  HashMap<String, String> URL_MAP = new HashMap<String, String>();
    private static MetsLSResolver INSTANCE;

    private final DOMImplementationLS dls;

    static {
        URL_MAP.put("http://www.openarchives.org/OAI/2.0/oai_dc.xsd", "/cz/cas/lib/proarc/oaidublincore/dc_oai.xsd");
        URL_MAP.put("http://dublincore.org/schemas/xmls/simpledc20021212.xsd", "/cz/cas/lib/proarc/oaidublincore/simpledc20021212.xsd");
        URL_MAP.put("http://www.w3.org/2001/03/xml.xsd", "/cz/cas/lib/proarc/mods/xml.xsd");
        URL_MAP.put("http://www.loc.gov/standards/xlink/xlink.xsd", "/cz/cas/lib/proarc/mods/xlink.xsd");
        URL_MAP.put("http://www.loc.gov/mods/xml.xsd", "/cz/cas/lib/proarc/mods/xml.xsd");
        URL_MAP.put("http://hul.harvard.edu/ois/xml/xsd/jhove/1.4/jhoveConfig.xsd", "/cz/cas/lib/proarc/common/export/mets/jhoveConfig.xsd");
        URL_MAP.put("http://www.loc.gov/standards/mets/mets.xsd", "/cz/cas/lib/proarc/mets/mets.xsd");
    }

    public static MetsLSResolver getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new MetsLSResolver();
        }
        return INSTANCE;
    }

    private MetsLSResolver() {
        try {
            dls = (DOMImplementationLS) DOMImplementationRegistry.newInstance().getDOMImplementation("LS");
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public LSInput resolveResource(String type, String namespaceURI, String publicId, String systemId, String baseURI) {
        String location = URL_MAP.get(systemId);
        if (location == null) {
            throw new IllegalStateException("Unable to find mapping for:" + systemId);
        }
        InputStream is = this.getClass().getResourceAsStream(location);
        LSInput input = dls.createLSInput();
        input.setByteStream(is);
        input.setPublicId(publicId);
        input.setSystemId(systemId);
        return input;
    }

}
