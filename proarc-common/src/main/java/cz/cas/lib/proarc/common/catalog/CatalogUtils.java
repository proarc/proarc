package cz.cas.lib.proarc.common.catalog;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.DateOtherDefinition;
import cz.cas.lib.proarc.mods.IssuanceDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsCollectionDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusSupplied;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Logger;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class CatalogUtils {

    private static final Logger LOG = Logger.getLogger(CatalogUtils.class.getName());

    public static String repairHtml(String s) {
        s = sortHtml(s);
        s = s.replaceAll("\\n", "");
        s = s.replaceAll("\\r", "");
        s = replaceMoreSpace(s);
        s = s.replace(") </b>", ") ");
        s = s.replace("( ", "</b>( ");
        s = s.replace("( ", " (");
        s = s.replace("=\" ", " = ");
        s = s.replace("\"", "");
        s = repairGeographicCode(s);
        return s;
    }

    private static String repairGeographicCode(String s) {
        s = s.replace("e-xr---", "Česko");
        s = s.replace("e-xr-cc", "Čechy");
        s = s.replace("e-xr-pg", "Praha (Česko : kraj)");
        s = s.replace("e-xr-st", "Středočeský kraj");
        s = s.replace("e-xr-kr", "Královéhradecký kraj");
        s = s.replace("e-xr-pa", "Pardubický kraj");
        s = s.replace("e-xr-us", "Ústecký kraj");
        s = s.replace("e-xr-li", "Liberecký kraj");
        s = s.replace("e-xr-pl", "Plzeňský kraj");
        s = s.replace("e-xr-ka", "Karlovarský kraj");
        s = s.replace("e-xr-jc", "Jihočeský kraj");
        s = s.replace("e-xr-jm", "Jihomoravský kraj");
        s = s.replace("e-xr-zl", "Zlínský kraj");
        s = s.replace("e-xr-vy", "Vysočina");
        s = s.replace("e-xr-mo", "Moravskoslezský kraj");
        s = s.replace("e-xr-ol", "Olomoucký kraj");
        s = s.replace("e-xr-mr", "Morava");
        s = s.replace("e-xr-sl", "Slezsko (Česko)");
        return s;
    }

    private static String replaceMoreSpace(String s) {
        while (s.contains("  ")) {
            s = s.replace("  ", " ");
        }
        return s;
    }

    /**
     * Uprava <mods> zdvojene <originInfo>
     * Vzdy nutne otestovat:
     * KNAV 001: 000036403, 000908303, 001044879, 002695932, 002705702, 002714840
     * KNAV carkod: 26002101695
     * SVKHK 001: 000162401
     */
    public static byte[] repairModsBytes(byte[] modsBytes, Document marcXml) throws UnsupportedEncodingException {
        String modsAsString = new String(modsBytes, "UTF-8");
        ModsCollectionDefinition modsCollection = ModsUtils.unmarshal(modsAsString, ModsCollectionDefinition.class);
        ModsDefinition mods = null;
        if (modsCollection == null || modsCollection.getMods().isEmpty()) {
            mods = ModsUtils.unmarshal(modsAsString, ModsDefinition.class);
        } else {
            mods = modsCollection.getMods().get(0);
        }
        repairLocation(mods);
        List<String> couples = new ArrayList<>();
        int updateNode = 0;
        int nodesCount = 0;

        if (containsNode(marcXml, "260")) {
            couples = findAndSplitNode(marcXml, "260");
            nodesCount = countNodes(marcXml, "260"); //svkhk 001 000162401
            updateNode = 260;
        } else if (containsNode(marcXml, "264")) {
            couples = findAndSplitNode(marcXml, "264");
            nodesCount = countNodes(marcXml, "264");
            updateNode = 264;
        }

        if (couples.size() < 1  || nodesCount != 1 || (couples.size() == 1 && onlyOneOriginInfoValues(mods.getOriginInfo()))) { // knav monografie 001 000938836
            modsAsString = repairIssuance(mods);
            return modsAsString.getBytes(StandardCharsets.UTF_8);
        } else {
            if (260 == updateNode) { //knav monografie isbn 80-200-0953-1
                modsAsString = repairMods(mods, couples);
                return modsAsString.getBytes(StandardCharsets.UTF_8);
            } else if (264 == updateNode) { // knav monografie - name: History of nanotechnology from pre-historic to modern times; isbn: 978-1-119-46008-4
                modsAsString = repairMods_264(mods, couples);
                return modsAsString.getBytes(StandardCharsets.UTF_8);
            } else {
                return modsBytes;
            }
        }
    }

    private static void repairLocation(ModsDefinition mods) {
        String physicalLocationValue = getPhysicalLocationValue(mods);
        if (physicalLocationValue != null) {
            for (LocationDefinition locationDefinition : mods.getLocation()) {
                if (locationDefinition.getPhysicalLocation().isEmpty()) {
                    PhysicalLocationDefinition physicalLocationDefinition = new PhysicalLocationDefinition();
                    physicalLocationDefinition.setAuthority("siglaADR");
                    physicalLocationDefinition.setValue(physicalLocationValue);
                    locationDefinition.getPhysicalLocation().add(physicalLocationDefinition);
                }
            }
            ListIterator<LocationDefinition> iterator = mods.getLocation().listIterator();
            while (iterator.hasNext()) {
                LocationDefinition location = iterator.next();
                if (location.getUrl().isEmpty() && location.getShelfLocator().isEmpty()) {
                    iterator.remove();
                }
            }
        }
    }

    private static String getPhysicalLocationValue(ModsDefinition mods) {
        for (LocationDefinition location : mods.getLocation()) {
            for (PhysicalLocationDefinition physicalLocation : location.getPhysicalLocation()) {
                if (physicalLocation.getValue() != null && !physicalLocation.getValue().isEmpty()) {
                    return physicalLocation.getValue();
                }
            }
        }
        return null;
    }


    private static boolean onlyOneOriginInfoValues(List<OriginInfoDefinition> originInfos) {
        int placeCount = 0;
        int publisherCount = 0;
        int dateCount = 0;

        for (OriginInfoDefinition originInfo : originInfos) {
            for (PlaceDefinition place : originInfo.getPlace()) {
                for (PlaceTermDefinition placeTerm : place.getPlaceTerm()) {
                    if ("text".equalsIgnoreCase(placeTerm.getType().value())) {
                        placeCount++;
                    }
                }
            }
            for (StringPlusLanguagePlusSupplied publisher : originInfo.getPublisher()) {
                publisherCount++;
            }
            for (DateDefinition date : originInfo.getDateIssued()) {
                if (date.getPoint() == null) {
                    dateCount++;
                }
            }
        }
        return 1 == placeCount && 1 == publisherCount && 1 == dateCount;

    }

    private static String repairMods_264(ModsDefinition mods, List<String> couples) {
        List<OriginInfoDefinition> fixedOriginInfo = new ArrayList<>();
        for (String couple : couples) {
            OriginInfoDefinition newOriginInfo = null;
            if (couple.contains("a")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    if (oldOriginInfo.getEventType() != null) {
                        for (PlaceDefinition oldPlace : oldOriginInfo.getPlace()) {
                            boolean delete = false;
                            for (PlaceTermDefinition oldPlaceTerm : oldPlace.getPlaceTerm()) {
                                if (oldPlaceTerm.getAuthority() == null) {
                                    PlaceDefinition newPlace = new PlaceDefinition();
                                    newPlace.getPlaceTerm().add(oldPlaceTerm);
                                    if (newOriginInfo == null) {
                                        newOriginInfo = new OriginInfoDefinition();
                                        newOriginInfo.setEventType(oldOriginInfo.getEventType());
                                    }
                                    newOriginInfo.getPlace().add(newPlace);
                                    oldPlace.getPlaceTerm().remove(oldPlace);
                                    delete = true;
                                    break;
                                }
                            }
                            if (delete == true) {
                                oldOriginInfo.getPlace().remove(oldPlace);
                                break;
                            }
                        }
                    }
                }
            }
            if (couple.contains("b")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    if (oldOriginInfo.getEventType() != null) {
                        for (StringPlusLanguagePlusSupplied oldPublisher : oldOriginInfo.getPublisher()) {
                            if (newOriginInfo == null) {
                                newOriginInfo = new OriginInfoDefinition();
                                newOriginInfo.setEventType(oldOriginInfo.getEventType());
                            }
                            newOriginInfo.getPublisher().add(oldPublisher);
                            oldOriginInfo.getPublisher().remove(oldPublisher);
                            break;
                        }
                    }
                }
            }
            if (couple.contains("c")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    if (oldOriginInfo.getEventType() != null) {
                        for (DateDefinition oldDate : oldOriginInfo.getDateIssued()) {
                            if (newOriginInfo == null) {
                                newOriginInfo = new OriginInfoDefinition();
                                newOriginInfo.setEventType(oldOriginInfo.getEventType());
                            }
                            newOriginInfo.getDateIssued().add(oldDate);
                            oldOriginInfo.getDateIssued().remove(oldDate);
                            break;
                        }
                        for (DateDefinition oldDate : oldOriginInfo.getDateCreated()) {
                            if (newOriginInfo == null) {
                                newOriginInfo = new OriginInfoDefinition();
                                newOriginInfo.setEventType(oldOriginInfo.getEventType());
                            }
                            newOriginInfo.getDateCreated().add(oldDate);
                            oldOriginInfo.getDateCreated().remove(oldDate);
                            break;
                        }
                        for (DateDefinition oldDate : oldOriginInfo.getCopyrightDate()) {
                            if (newOriginInfo == null) {
                                newOriginInfo = new OriginInfoDefinition();
                                newOriginInfo.setEventType(oldOriginInfo.getEventType());
                            }
                            newOriginInfo.getCopyrightDate().add(oldDate);
                            oldOriginInfo.getCopyrightDate().remove(oldDate);
                            break;
                        }
                        for (DateOtherDefinition oldDate : oldOriginInfo.getDateOther()) {
                            if (newOriginInfo == null) {
                                newOriginInfo = new OriginInfoDefinition();
                                newOriginInfo.setEventType(oldOriginInfo.getEventType());
                            }
                            newOriginInfo.getDateOther().add(oldDate);
                            oldOriginInfo.getDateOther().remove(oldDate);
                            break;
                        }
                    }
                }
            }
            if (newOriginInfo != null) {
                fixedOriginInfo.add(newOriginInfo);
            }
        }
        mods.getOriginInfo().addAll(fixedOriginInfo);
        cleanOriginInfo(mods);
        mergeFirstTwoOriginInfo(mods);
        copyPlaceDatePublisher(mods);
        deleteDoubleDateIssued(mods);
        repairIssuance(mods);
        return ModsUtils.toXml(mods, true);
    }

    private static void cleanOriginInfo(ModsDefinition mods) {
        ListIterator<OriginInfoDefinition> iterator = mods.getOriginInfo().listIterator();
        while (iterator.hasNext()) {
            OriginInfoDefinition originInfo = iterator.next();
            if (originInfo.getEventType() != null &&
                    originInfo.getPlace().isEmpty() &&
                    originInfo.getPublisher().isEmpty() &&
                    originInfo.getDateCreated().isEmpty() &&
                    originInfo.getDateIssued().isEmpty() &&
                    originInfo.getCopyrightDate().isEmpty() &&
                    originInfo.getDateOther().isEmpty()) {
                iterator.remove();
            }
        }
    }

    private static String repairIssuance(ModsDefinition mods) {
        IssuanceDefinition issuanceDefinition = null;
        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            for (IssuanceDefinition issuance : originInfo.getIssuance()) {
                if (issuance != null && !issuance.value().isEmpty()) {
                    issuanceDefinition = issuance;
                    break;
                }
            }
        }

        if (issuanceDefinition != null) {
            for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
                if (originInfo.getIssuance().isEmpty()) {
                    originInfo.getIssuance().add(issuanceDefinition);
                }
            }
        }
        return ModsUtils.toXml(mods, true);
    }


    private static String repairMods(ModsDefinition mods, List<String> couples) {
        List<OriginInfoDefinition> fixedOriginInfo = new ArrayList<>();
        for (String couple : couples) {
            OriginInfoDefinition newOriginInfo = null;
            if (couple.contains("a")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    for (PlaceDefinition oldPlace : oldOriginInfo.getPlace()) {
                        boolean delete = false;
                        for (PlaceTermDefinition oldPlaceTerm : oldPlace.getPlaceTerm()) {
                            if (oldPlaceTerm.getAuthority() == null) {
                                PlaceDefinition newPlace = new PlaceDefinition();
                                newPlace.getPlaceTerm().add(oldPlaceTerm);
                                if (newOriginInfo == null) {
                                    newOriginInfo = new OriginInfoDefinition();
                                }
                                newOriginInfo.getPlace().add(newPlace);
                                oldPlace.getPlaceTerm().remove(oldPlace);
                                delete = true;
                                break;
                            }
                        }
                        if (delete == true) {
                            oldOriginInfo.getPlace().remove(oldPlace);
                            break;
                        }
                    }
                }
            }
            if (couple.contains("b")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    for (StringPlusLanguagePlusSupplied oldPublisher : oldOriginInfo.getPublisher()) {
                        if (newOriginInfo == null) {
                            newOriginInfo = new OriginInfoDefinition();
                        }
                        newOriginInfo.getPublisher().add(oldPublisher);
                        oldOriginInfo.getPublisher().remove(oldPublisher);
                        break;
                    }
                }
            }
            if (couple.contains("c")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    for (DateDefinition oldDate : oldOriginInfo.getDateIssued()) {
                        if (newOriginInfo == null) {
                            newOriginInfo = new OriginInfoDefinition();
                        }
                        newOriginInfo.getDateIssued().add(oldDate);
                        oldOriginInfo.getDateIssued().remove(oldDate);
                        break;
                    }
                }
            }

            if (couple.contains("g")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    for (DateDefinition oldDate : oldOriginInfo.getDateCreated()) {
                        if (newOriginInfo == null) {
                            newOriginInfo = new OriginInfoDefinition();
                        }
                        newOriginInfo.getDateCreated().add(oldDate);
                        oldOriginInfo.getDateCreated().remove(oldDate);
                        break;
                    }
                }
            }
            if (newOriginInfo != null) {
                fixedOriginInfo.add(newOriginInfo);
            }
        }
        mods.getOriginInfo().addAll(fixedOriginInfo);
        mergeFirstTwoOriginInfo(mods);
        copyPlaceDatePublisher(mods);
        deleteDoubleDateIssued(mods);
        repairIssuance(mods);
        return ModsUtils.toXml(mods, true);
    }

    private static void deleteDoubleDateIssued(ModsDefinition mods) {
        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            DateDefinition dateDefinition = null;
            for (DateDefinition date : originInfo.getDateIssued()) {
                if ("marc".equals(date.getEncoding())) {
                    dateDefinition = date;
                    break;
                }
            }
            if (dateDefinition == null) {
                for (DateDefinition date : originInfo.getDateIssued()) {
                    dateDefinition = date;
                    break;
                }
            }
            if (dateDefinition != null) {
                originInfo.getDateIssued().clear();
                originInfo.getDateIssued().add(dateDefinition);
            }
        }
    }

    private static void copyPlaceDatePublisher(ModsDefinition mods) {
        DateDefinition dateValue = null;
        StringPlusLanguagePlusSupplied publisherValue = null;
        PlaceTermDefinition placeValue = null;
        IssuanceDefinition issuanceDefinition = null;

        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            if (issuanceDefinition == null && !originInfo.getIssuance().isEmpty() && originInfo.getIssuance().get(0) != null) {
                issuanceDefinition = originInfo.getIssuance().get(0);
            }
            for (StringPlusLanguagePlusSupplied publisher : originInfo.getPublisher()) {
                if (publisher.getValue() != null) {
                    if (publisherValue == null) {
                        publisherValue = publisher;
                        break;
                    } else {
                        publisherValue = null;
                        break;
                    }
                }
            }
            for (DateDefinition date : originInfo.getDateIssued()) {
                if (date.getValue() != null) {
                    if (dateValue == null) {
                        dateValue = date;
                        break;
                    } else {
                        dateValue = null;
                        break;
                    }
                }
            }
            for (PlaceDefinition place : originInfo.getPlace()) {
                for (PlaceTermDefinition placeTerm : place.getPlaceTerm()) {
                    if ("text".equalsIgnoreCase(placeTerm.getType().value()) && placeTerm.getValue() != null) {
                        if (placeValue == null) {
                            placeValue = placeTerm;
                            break;
                        } else {
                            placeValue = null;
                            break;
                        }
                    }
                }
            }
        }

        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            if (placeValue != null) {
                boolean containsPlace = false;
                for (PlaceDefinition place : originInfo.getPlace()) {
                    for (PlaceTermDefinition placeTermDefinition : place.getPlaceTerm()) {
                        if (placeTermDefinition.getValue().equals(placeValue.getValue())) {
                            containsPlace = true;
                            break;
                        }
                    }
                    if (!place.getPlaceTerm().contains(placeValue)) {
                        containsPlace = true;
                    }
                }
                if (containsPlace == false) {
                    if (originInfo.getPlace().isEmpty()) {
                        PlaceDefinition place = new PlaceDefinition();
                        originInfo.getPlace().add(place);
                    }
                    originInfo.getPlace().get(0).getPlaceTerm().add(placeValue);
                }
            }
            if (dateValue != null && !originInfo.getDateIssued().contains(dateValue)) {
                originInfo.getDateIssued().add(dateValue);
            }
            if (publisherValue != null && originInfo.getPublisher().isEmpty()) { // KNAV monografie, sysno, 000038982
                originInfo.getPublisher().add(publisherValue);
            }
            if (issuanceDefinition != null && originInfo.getIssuance().isEmpty()) {
                originInfo.getIssuance().add(issuanceDefinition);
            }
        }
    }

    private static void mergeFirstTwoOriginInfo(ModsDefinition mods) {
        List<OriginInfoDefinition> originInfos = mods.getOriginInfo();
        OriginInfoDefinition firstInfo = null;
        OriginInfoDefinition secondInfo = null;

        if (originInfos.size() > 1) {
            firstInfo = originInfos.get(0);
            secondInfo = originInfos.get(1);
            if (hasOnlyOnePlaceValue(firstInfo.getPlace(), secondInfo.getPlace()) &&
                    hasOnlyOneDateValue(firstInfo.getDateIssued(), secondInfo.getDateIssued()) &&
                    hasOnlyOneValue(firstInfo.getPublisher(), secondInfo.getPublisher())) {
                firstInfo.getPlace().addAll(secondInfo.getPlace());
                firstInfo.getPublisher().addAll(secondInfo.getPublisher());
                mergeDate(firstInfo.getDateIssued(), secondInfo.getDateIssued());
                firstInfo.getDateCreated().addAll(secondInfo.getDateCreated());
                firstInfo.getDateCaptured().addAll(secondInfo.getDateCaptured());
                firstInfo.getDateValid().addAll(secondInfo.getDateValid());
                firstInfo.getDateModified().addAll(secondInfo.getDateModified());
                firstInfo.getCopyrightDate().addAll(secondInfo.getCopyrightDate());
                firstInfo.getDateOther().addAll(secondInfo.getDateOther());
                firstInfo.getEdition().addAll(secondInfo.getEdition());
                firstInfo.getIssuance().addAll(secondInfo.getIssuance());
                firstInfo.getFrequency().addAll(secondInfo.getFrequency());
                firstInfo.setEventType(secondInfo.getEventType());
                originInfos.remove(secondInfo);
            }
        }
    }

    private static void mergeDate(List<DateDefinition> origin, List<DateDefinition> copy) {
        for (DateDefinition dateCopy : copy) {
            for (DateDefinition dateOrigin : origin) {
                if (dateCopy.getPoint() != null) {
                    if (!(dateCopy.getPoint().equals(dateOrigin.getPoint()) && dateCopy.getValue().equals(dateCopy.getValue()))) {
                        origin.add(dateCopy);
                        break;
                    }
                } else {
                    if (!(dateCopy.getValue().equals(dateCopy.getValue()))) {
                        origin.add(dateCopy);
                        break;
                    }
                }
            }
        }
    }

    private static boolean hasOnlyOneDateValue(List<DateDefinition> listFirst, List<DateDefinition> listSecond) {
        if (listFirst == null || listFirst.isEmpty()) {
            if (listSecond != null || !listSecond.isEmpty()) {
                return true;
            }
            return false;
        }
        if (listSecond == null || listSecond.isEmpty()) {
            if (listFirst != null || !listFirst.isEmpty()) {
                return true;
            }
            return false;
        }
        for (DateDefinition first : listFirst) {
            for (DateDefinition second : listSecond) {
                if (first.getValue() != null && first.getValue().equals(second.getValue())) {
                    return true;
                }
            }
        }
        return false;
    }

    private static boolean hasOnlyOneValue(List listFirst, List listSecond) {
        if (listFirst == null || listFirst.isEmpty()) {
            if (listSecond != null || !listSecond.isEmpty()) {
                return true;
            }
            return false;
        }
        if (listSecond == null || listSecond.isEmpty()) {
            if (listFirst != null || !listFirst.isEmpty()) {
                return true;
            }
            return false;
        }
        return false;
    }

    private static boolean hasOnlyOnePlaceValue(List<PlaceDefinition> placeFirst, List<PlaceDefinition> placeSecond) {
        if (placeFirst == null || placeFirst.isEmpty()) {
            if (placeSecond != null || !placeSecond.isEmpty()) {
                return true;
            }
            return false;
        }
        if (placeSecond == null || placeSecond.isEmpty()) {
            if (placeFirst != null || !placeFirst.isEmpty()) {
                return true;
            }
            return false;
        }
        if (placeFirst.size() > 1) {
            return false;
        } else if (placeSecond.size() > 1) {
            return false;
        } else {
            PlaceDefinition placeDefinitionFirst = placeFirst.get(0);
            PlaceDefinition placeDefinitionSecond = placeSecond.get(0);
            if (placeDefinitionFirst.getPlaceTerm().isEmpty() && placeDefinitionSecond.getPlaceTerm().isEmpty()) {
                return false;
            } else if (placeDefinitionFirst.getPlaceTerm().isEmpty() && !placeDefinitionSecond.getPlaceTerm().isEmpty()) {
                return true;
            } else if (!placeDefinitionFirst.getPlaceTerm().isEmpty() && placeDefinitionSecond.getPlaceTerm().isEmpty()) {
                return true;
            } else {
                PlaceTermDefinition place1 = placeDefinitionFirst.getPlaceTerm().get(0);
                PlaceTermDefinition place2 = placeDefinitionSecond.getPlaceTerm().get(0);
                if (place1.getType() == null || place2.getType() == null) {
                    if (place1.getValue() != null && place1.getValue().equals(place2.getValue())) {
                        return false;
                    }
                    return true;
                }
                if (place1.getType().value().equals(place2.getType().value())) {
                    return false;
                }
                return true;
            }
        }
    }

    private static List<String> findAndSplitNode(Document marcXml, String tagValue) {
        List<String> couples = new ArrayList<>();
        try {
            XPathFactory xPathFactory = ProarcXmlUtils.defaultXPathFactory();
            XPath xPath = xPathFactory.newXPath();
            xPath.setNamespaceContext(new SimpleNamespaceContext().add("m", "http://www.loc.gov/MARC21/slim"));
            XPathExpression originInfoPath = xPath.compile("m:collection/m:record/m:datafield[@tag=" + tagValue + "]");
            Node node = (Node) originInfoPath.evaluate(marcXml, XPathConstants.NODE);
            if (node == null) {
                originInfoPath = xPath.compile("m:record/m:datafield[@tag=" + tagValue + "]");
                node = (Node) originInfoPath.evaluate(marcXml, XPathConstants.NODE);
            }
            List<String> listOfSubelements = new ArrayList<>();
            if (node != null && node.hasChildNodes()) {
                NodeList listOfNodes = node.getChildNodes();
                for (int i = 0; i < listOfNodes.getLength(); i++) {
                    Node subelement = listOfNodes.item(i);
                    if (subelement.getAttributes() != null && subelement.getAttributes().getNamedItem("code") != null) {
                        listOfSubelements.add(subelement.getAttributes().getNamedItem("code").getNodeValue());
                    }
                }
            }
            int position = 0;
            for (String subelement : listOfSubelements) {
                if (couples.isEmpty()) {
                    couples.add(subelement);
                } else {
                    if (couples.get(position).contains(subelement)) {
                        couples.add(subelement);
                        position++;
                    } else {
                        String element = couples.get(position);
                        couples.set(position, element + subelement);
                    }
                }
            }
        } catch (XPathExpressionException e) {
            LOG.warning("Impossible to parse node with tag " + tagValue + " from downloaded marcXml");
            e.printStackTrace();
        }
        return couples;
    }

    private static boolean containsNode(Document marcXml, String tagValue) {
        Node node = null;
        try {
            XPathFactory xPathFactory = ProarcXmlUtils.defaultXPathFactory();
            XPath xPath = xPathFactory.newXPath();
            xPath.setNamespaceContext(new SimpleNamespaceContext().add("m", "http://www.loc.gov/MARC21/slim"));
            XPathExpression originInfoPath = xPath.compile("m:collection/m:record/m:datafield[@tag=" + tagValue + "]");
            node = (Node) originInfoPath.evaluate(marcXml, XPathConstants.NODE);
            if (node == null) {
                originInfoPath = xPath.compile("m:record/m:datafield[@tag=" + tagValue + "]");
                node = (Node) originInfoPath.evaluate(marcXml, XPathConstants.NODE);
            }
        } catch (XPathExpressionException e) {
            LOG.warning("Impossible to parse node with tag " + tagValue + " from downloaded marcXml");
            e.printStackTrace();
        } finally {
            return node != null;
        }
    }

    private static int countNodes(Document marcXml, String tagValue) {
        Double counts = null;
        try {
            XPathFactory xPathFactory = ProarcXmlUtils.defaultXPathFactory();
            XPath xPath = xPathFactory.newXPath();
            xPath.setNamespaceContext(new SimpleNamespaceContext().add("m", "http://www.loc.gov/MARC21/slim"));
            String expression = "count(m:collection//m:record//m:datafield[@tag=" + tagValue + "])";
            counts = (Double) xPath.evaluate(expression, marcXml, XPathConstants.NUMBER);

            if (counts == null) {
                expression = "count(m:record//m:datafield[@tag=" + tagValue + "])";
                counts = (Double) xPath.evaluate(expression, marcXml, XPathConstants.NUMBER);
            }
        } catch (XPathExpressionException e) {
            LOG.warning("Impossible to parse double with tag " + tagValue + " from downloaded marcXml");
            e.printStackTrace();
        } finally {
            if (counts == null) {
                return 0;
            } else {
                return counts.intValue();
            }
        }
    }

    private static String sortHtml(String html) {
        String htmlBck = html;
        html = html.replaceAll("<table xmlns:xlink=\"http://www.w3.org/1999/xlink\">", "");
        html = html.replaceAll("</table>", "");
        String[] arr = html.split("<tr>");
        List<String> lines = Arrays.asList(arr);
        List<HtmlLine> htmlLine = new ArrayList<>();
        ModsConstants constant = null;
        for (String line : lines) {
            if (line.isEmpty() || containsOnlyWhiteSpaces(line)) {
                continue;
            } else if (line.contains("<b>abstract") || line.contains("<b>Abstract") || line.contains("<b>Abstrakt")) {
                constant = ModsConstants.ABSTRACT;
            } else if (line.contains("<b>accessCondition") || line.contains("<b>Access Condition")) {
                constant = ModsConstants.ACCESS_CONDITION;
            } else if (line.contains("<b>classification") || line.contains("<b>Classification") || line.contains("<b>Klasifikace")) {
                constant = ModsConstants.CLASSIFICATION;
            } else if (line.contains("<b>extension")) {
                constant = ModsConstants.EXTENSION;
            } else if (line.contains("<b>genre") || line.contains("<b>Genre") || line.contains("<b>Ž&aacute;nr")) {
                constant = ModsConstants.GENRE;
            } else if (line.contains("<b>identifier") || line.contains("<b>Identifier") || line.contains("<b>Identifik&aacute;tor")) {
                constant = ModsConstants.IDENTIFIER;
            } else if (line.contains("td colspan=2><b>language") || line.contains("td colspan=2><b>Language") || line.contains("<b>Jazyk")) {
                constant = ModsConstants.LANGUAGE;
            } else if (line.contains("<b>location") || line.contains("<b>Location") || line.contains("<b>Lokace")) {
                constant = ModsConstants.LOCATION;
            } else if (line.contains("<b>name") || line.contains("<b>Name") || line.contains("<b>Jm&eacute;no")) {
                constant = ModsConstants.NAME;
            } else if (line.contains("<b>note") || line.contains("<b>Note") || line.contains("<b>Pozn&aacute;mka")) {
                constant = ModsConstants.NOTE;
            } else if (line.contains("<b>originInfo") || line.contains("<b>Origin Information")) {
                constant = ModsConstants.ORIGIN_INFO;
            } else if (line.contains("<b>part") || line.contains("<b>Part")) {
                constant = ModsConstants.PART;
            } else if (line.contains("<b>physicalDescription") || line.contains("<b>Physical Description") || line.contains("<b>Fyzick&yacute; popis")) {
                constant = ModsConstants.PHYSICAL_DESCRIPTION;
            } else if (line.contains("<b>recordInfo") || line.contains("<b>Record Information") || line.contains("<b>Informace o z&aacute;znamu")) {
                constant = ModsConstants.RECORD_INFO;
            } else if (line.contains("<b>relatedItem") || line.contains("<b>Related Item") || line.contains("<b>Souvisej&iacute;c&iacute; položka")) {
                constant = ModsConstants.RELATED_ITEM;
            } else if (line.contains("<b>subject") || line.contains("<b>Subject") || line.contains("<b>Předmět")) {
                constant = ModsConstants.SUBJECT;
            } else if (line.contains("<b>tableOfContents") || line.contains("<b>Table of Contents")) {
                constant = ModsConstants.TABLE_OF_CONTENTS;
            } else if (line.contains("<b>targetAudience")) {
                constant = ModsConstants.TARGET_AUDIENCE;
            } else if (line.contains("<b>titleInfo") || line.contains("<b>Title Information)") || line.contains("<b>Informace o titulu")) {
                constant = ModsConstants.TITLE_INFO;
            } else if (line.contains("<b>typeOfResource") || line.contains("<b>Type of Resource")|| line.contains("<b>Typ zdroje")) {
                constant = ModsConstants.TYPE_OF_RESOURCE;
            }
            htmlLine.add(new HtmlLine("<tr>" + line, constant));
        }

        try {
            Collections.sort(htmlLine, new ModsComparator());
        } catch (NullPointerException ex) {
            ex.printStackTrace();
            return htmlBck;
        }

        StringWriter writer = new StringWriter();
        writer.append("<table xmlns:xlink=http://www.w3.org/1999/xlink>");
        for (HtmlLine line : htmlLine) {
            writer.append(line.getLine());
        }
        writer.append("</table>");
        return writer.toString();
    }

    private static boolean containsOnlyWhiteSpaces(String line) {
        return line.replaceAll("\r", "").replaceAll("\n", "").replaceAll(" ", "").isEmpty();
    }

    public enum ModsConstants {
        TITLE_INFO, NAME, ORIGIN_INFO, LOCATION, IDENTIFIER, LANGUAGE, PHYSICAL_DESCRIPTION,
        ABSTRACT, NOTE, TYPE_OF_RESOURCE, GENRE, CLASSIFICATION, SUBJECT, PART, TABLE_OF_CONTENTS,
        ACCESS_CONDITION, EXTENSION, TARGET_AUDIENCE, RECORD_INFO, RELATED_ITEM
    }

    private static class HtmlLine {
        private String line;
        private ModsConstants constants;

        public HtmlLine(String line, ModsConstants constants) {
            this.line = line;
            this.constants = constants;
        }

        public String getLine() {
            return line;
        }

        public ModsConstants getConstants() {
            return constants;
        }
    }

    private static class ModsComparator implements Comparator<HtmlLine> {

        @Override
        public int compare(HtmlLine line1, HtmlLine line2) {
            return line1.getConstants().compareTo(line2.getConstants());
        }
    }
}
