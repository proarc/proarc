package cz.cas.lib.proarc.common.catalog;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.DateOtherDefinition;
import cz.cas.lib.proarc.mods.IssuanceDefinition;
import cz.cas.lib.proarc.mods.ModsCollectionDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusSupplied;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
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

    public static byte[] repairModsBytes(byte[] modsBytes, Document marcXml) throws UnsupportedEncodingException {
        String modsAsString = new String(modsBytes, "UTF-8");
        ModsCollectionDefinition modsCollection = ModsUtils.unmarshal(modsAsString, ModsCollectionDefinition.class);
        ModsDefinition mods = null;
        if (modsCollection == null || modsCollection.getMods().isEmpty()) {
            mods = ModsUtils.unmarshal(modsAsString, ModsDefinition.class);
        } else {
            mods = modsCollection.getMods().get(0);
        }
        List<String> couples = new ArrayList<>();
        int updateNode = 0;

        if (containsNode(marcXml, "260")) {
            couples = findAndSplitNode(marcXml, "260");
            updateNode = 260;
        } else if (containsNode(marcXml, "264")) {
            couples = findAndSplitNode(marcXml, "264");
            updateNode = 264;
        }

        if (couples.size() < 1) {
            return modsBytes;
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
            if (publisherValue != null && !originInfo.getPublisher().contains(publisherValue)) {
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
            firstInfo.getPlace().addAll(secondInfo.getPlace());
            firstInfo.getPublisher().addAll(secondInfo.getPublisher());
            firstInfo.getDateIssued().addAll(secondInfo.getDateIssued());
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

}
