/*
 * Copyright (C) 2024 Lukas Sykora
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

import com.sun.xml.bind.marshaller.NamespacePrefixMapper;
import cz.cas.lib.proarc.common.xml.docmd.DocumentMdUtils;
import cz.cas.lib.proarc.common.xml.ndktech.NdkTechnicalUtils;
import cz.cas.lib.proarc.premis.PremisUtils;


/**
 * The namespace mapper that returns a custom prefix if needed
 * otherwise it uses the suggested value
 *
 * @author Lukas Sykora
 */
public class ProArcPrefixNamespaceMapper extends NamespacePrefixMapper {

    private static final String DOCMD_PREFIX = "docmd";
    private static final String DOCMD_URI = DocumentMdUtils.NS;

    private static final String PREMIS_PREFIX = "premis";
    private static final String PREMIS_URI = PremisUtils.NS;

    private static final String NDKTECH_PREFIX = "ndktech";
    private static final String NDKTECH_URI = NdkTechnicalUtils.NS;

    @Override
    public String getPreferredPrefix(String namespaceUri, String suggestion, boolean requirePrefix) {
        if (DOCMD_URI.equals(namespaceUri)) {
            return DOCMD_PREFIX;
        } else if (NDKTECH_URI.equals(namespaceUri)) {
            return NDKTECH_PREFIX;
        }
        return suggestion;
    }

    @Override
    public String[] getPreDeclaredNamespaceUris() {
        return new String[] {DOCMD_URI, NDKTECH_URI};
    }
}
