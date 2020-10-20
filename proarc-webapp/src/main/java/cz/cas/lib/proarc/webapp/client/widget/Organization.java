package cz.cas.lib.proarc.webapp.client.widget;

import java.util.LinkedHashMap;

public class Organization {

    public static LinkedHashMap<String, String> getMap() {
        LinkedHashMap<String, String> valueMap = new LinkedHashMap();
        valueMap.put("ABA007", "Knihovna Akademie věd ČR");
        valueMap.put("BOA001", "Moravská zemská knihovna");
        valueMap.put("ABA010", "Knihovna Národního muzea");
        valueMap.put("ABA012", "Národní pedagogická knihovna Komenského Praha");
        valueMap.put("HKA001", "Studijní a vědecká knihovna v Hradci Králové");
        valueMap.put("ABE301", "Český rozhlas - knihovna");
        valueMap.put("ABG001", "Městská knihovna v Praze");
        valueMap.put("ABD068", "Univerzita Karlova v Praze - 1.LF a VFN");
        valueMap.put("ABD103", "Univerzita Karlova v Praze - Fakulta sociálních věd");
        valueMap.put("PAG001", "Krajská knihovna v Pardubicích");
        valueMap.put("JIG001", "Městská knihovna Jihlava");
        valueMap.put("221610200","SOkA Jihlava");
        valueMap.put("226103010","SOkA Pelhřimov");
        valueMap.put("226101010","SOkA Havlíčkův Brod");
        valueMap.put("226104010","SOkA Třebíč");
        valueMap.put("226105010","SOkA Žďár nad Sázavou");
        return valueMap;
    }
}