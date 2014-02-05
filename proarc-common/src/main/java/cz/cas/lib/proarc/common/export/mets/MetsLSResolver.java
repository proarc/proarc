package cz.cas.lib.proarc.common.export.mets;

import java.io.InputStream;
import java.util.HashMap;

import org.w3c.dom.ls.LSInput;
import org.w3c.dom.ls.LSResourceResolver;

public class MetsLSResolver implements LSResourceResolver {
    public static HashMap<String, String> urlMap = new HashMap<String, String>();

    static {
        urlMap.put("http://www.openarchives.org/OAI/2.0/oai_dc.xsd", "/cz/cas/lib/proarc/oaidublincore/dc_oai.xsd");
        urlMap.put("http://dublincore.org/schemas/xmls/simpledc20021212.xsd", "/cz/cas/lib/proarc/oaidublincore/simpledc20021212.xsd");
        urlMap.put("http://www.w3.org/2001/03/xml.xsd", "/cz/cas/lib/proarc/mods/xml.xsd");
        urlMap.put("http://www.loc.gov/standards/xlink/xlink.xsd", "/cz/cas/lib/proarc/mods/xlink.xsd");
        urlMap.put("http://www.loc.gov/mods/xml.xsd", "/cz/cas/lib/proarc/mods/xml.xsd");
        urlMap.put("http://hul.harvard.edu/ois/xml/xsd/jhove/1.4/jhoveConfig.xsd", "/cz/cas/lib/proarc/common/export/mets/jhoveConfig.xsd");
        urlMap.put("http://www.loc.gov/standards/mets/mets.xsd", "/cz/cas/lib/proarc/mets/mets.xsd");
    }
    @Override
    public LSInput resolveResource(String type, String namespaceURI, String publicId, String systemId, String baseURI) {
        String location = urlMap.get(systemId);
        if (location == null) {
            throw new IllegalStateException("Unable to find mapping for:" + systemId);
        }
        InputStream is = this.getClass().getResourceAsStream(location);
        return new Input(publicId, systemId, is);
    }

}
