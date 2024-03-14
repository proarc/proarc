package cz.cas.lib.proarc.common.xml;

import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

public class ProArcPrefixNamespaceMapper extends NamespacePrefixMapper {

    private static final String DOCMD_PREFIX = "docmd"; // DEFAULT NAMESPACE
    private static final String DOCMD_URI = "http://www.fcla.edu/docmd";

    private static final String PREMIS_PREFIX = "premis";
    private static final String PREMIS_URI = "info:lc/xmlns/premis-v2";

    @Override
    public String getPreferredPrefix(String namespaceUri, String suggestion, boolean requirePrefix) {
        if(DOCMD_URI.equals(namespaceUri)) {
            return DOCMD_PREFIX;
        }
        return suggestion;
    }

    @Override
    public String[] getPreDeclaredNamespaceUris() {
        return new String[] {DOCMD_URI};
    }
}
