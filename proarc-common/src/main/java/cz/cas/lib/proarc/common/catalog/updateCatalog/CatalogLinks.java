package cz.cas.lib.proarc.common.catalog.updateCatalog;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.mods.ModsCollectionDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.LinkedHashSet;
import java.util.List;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/** Catalog links used both for idempotency and for a reviewable conflict log. */
public final class CatalogLinks {
    private CatalogLinks() { }

    public static List<String> read(String xml) {
        ModsCollectionDefinition collection = ModsUtils.unmarshal(xml, ModsCollectionDefinition.class);
        ModsDefinition mods = collection == null || collection.getMods().isEmpty()
                ? ModsUtils.unmarshal(xml, ModsDefinition.class) : collection.getMods().get(0);
        if (mods == null) {
            throw new IllegalArgumentException("Nelze přečíst MODS z katalogu.");
        }
        LinkedHashSet<String> links = new LinkedHashSet<>();
        mods.getLocation().forEach(location -> location.getUrl().forEach(url -> {
            if (url.getValue() != null && !url.getValue().isBlank()) {
                links.add(url.getValue().trim());
            }
        }));
        return List.copyOf(links);
    }

    /** Verbis stores the object ID in a configurable MARC subfield, not necessarily 856$u. */
    public static List<String> readMarcObjectIds(Node marc, String field, String appSubfield, String objectSubfield) {
        LinkedHashSet<String> values = new LinkedHashSet<>();
        NodeList fields = (marc instanceof org.w3c.dom.Document document ? document.getDocumentElement() : (Element) marc)
                .getElementsByTagNameNS("http://www.loc.gov/MARC21/slim", "datafield");
        for (int i = 0; i < fields.getLength(); i++) {
            Element element = (Element) fields.item(i);
            if (!field.equals(element.getAttribute("tag"))) continue;
            NodeList subfields = element.getElementsByTagNameNS("http://www.loc.gov/MARC21/slim", "subfield");
            boolean proarc = false;
            for (int j = 0; j < subfields.getLength(); j++) {
                Element subfield = (Element) subfields.item(j);
                if (appSubfield.equals(subfield.getAttribute("code")) && "proarcId".equals(subfield.getTextContent().trim())) {
                    proarc = true;
                }
            }
            if (proarc) {
                for (int j = 0; j < subfields.getLength(); j++) {
                    Element subfield = (Element) subfields.item(j);
                    if (objectSubfield.equals(subfield.getAttribute("code")) && !subfield.getTextContent().isBlank()) {
                        values.add(subfield.getTextContent().trim());
                    }
                }
            }
        }
        return List.copyOf(values);
    }

    public static boolean contains(List<String> links, String expected) {
        return links.stream().anyMatch(link -> normalize(link).equals(normalize(expected)));
    }

    private static String normalize(String link) {
        // Aleph historically uses both /view/ and /uuid/ for the same object.
        return link.replace("/view/", "/uuid/");
    }

    public static String link(String base, String pid) {
        return base == null || base.isBlank() ? pid : base + (base.endsWith("/") ? "" : "/") + pid;
    }

    public static CatalogUpdateResult result(List<String> previous, String expected) {
        if (previous.isEmpty()) {
            return CatalogUpdateResult.success("Zápis do katalogu proveden.\nNový odkaz: " + expected);
        }
        return CatalogUpdateResult.review("Zápis proveden, zkontrolujte rozdílné odkazy pro stejný katalogový identifikátor."
                + "\nPůvodní odkazy:\n" + String.join("\n", previous) + "\nNový odkaz: " + expected);
    }
}
