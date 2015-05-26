/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.xml;

import javax.xml.xpath.XPathFactory;

/**
 * Helper to handle ProArc XMLs.
 *
 * @author Jan Pokorsky
 */
public final class ProarcXmlUtils {

    public static final String NS_IMPORT = "http://proarc.lib.cas.cz/xml/common/import/v1/";

    public static final String NS_EXPORT = "http://proarc.lib.cas.cz/xml/common/export/v1/";

    private static ThreadLocal<XPathFactory> XPATH_FACTORY = new ThreadLocal<XPathFactory>();

    /**
     * Gets a thread safe XPath factory.
     * @return the factory
     */
    public static XPathFactory defaultXPathFactory() {
        XPathFactory xPathFactory = XPATH_FACTORY.get();
        if (xPathFactory == null) {
            xPathFactory = XPathFactory.newInstance();
        }
        return xPathFactory;
    }
}
