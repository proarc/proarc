

package cz.cas.lib.proarc.audiopremis;

import cz.cas.lib.proarc.mets.AmdSecType;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.premis.AgentComplexType;
import cz.cas.lib.proarc.premis.EventComplexType;
import cz.cas.lib.proarc.premis.ExtensionComplexType;
import cz.cas.lib.proarc.premis.LinkingAgentIdentifierComplexType;
import cz.cas.lib.proarc.premis.ObjectFactory;
import cz.cas.lib.proarc.premis.PremisComplexType;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

public class AudioPremisUtils {
    private Mets mets;
    private String id;
    private String label;
    private String model;
    private String audioDescription;


    public AudioPremisUtils(String id, String label, String model, String audioDescription) {
        this.id = id;
        this.label = label;
        this.model = model;
        this.audioDescription = audioDescription;
    }

    public Mets getMets() {
        return mets;
    }

    public void createAudioDescription(String oldAudioDescription) throws Exception {
        String audio = oldAudioDescription.replace("},{\"digiprovMD\":", "}&&&{\"digiprovMD\":");
        String[] premis = audio.split("&&&");

        Mets amdSecMets = new Mets();
        amdSecMets.setLabel1(label);
        amdSecMets.setTYPE(model);
        for (int i = 0; i < premis.length; i++) {
            String newAudioDescription = repaireString(premis[i]);
            String[] audioDescriptionTokens = newAudioDescription.split("-");


            AmdSecType amdSec = new AmdSecType();
            addNodeToPremis(amdSec, createEventNode(audioDescriptionTokens), "EVENT");
            addNodeToPremis(amdSec, createAgentNode(audioDescriptionTokens), "AGENT");
            amdSecMets.getAmdSec().add(amdSec);
        }
        this.mets = amdSecMets;
    }

    private String repaireString(String retval) {
        String[] separators = {":{", "},", ":", "{", "}", ","};
        retval = retval.replace("\"", "");
        for (int i = 0; i < separators.length; i++) {
            retval = retval.replace(separators[i], "-");
        }
        return retval;
    }

    private Node createNode(JAXBElement jaxb, JAXBContext jc, String expression) throws  Exception{
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document document = db.newDocument();

        // Marshal the Object to a Document
        Marshaller marshaller = jc.createMarshaller();
        marshaller.marshal(jaxb, document);
        XPath xpath = XPathFactory.newInstance().newXPath();
        Node agentNode = (Node) xpath.compile(expression).evaluate(document, XPathConstants.NODE);
        return agentNode;
    }

    private void addNodeToPremis(AmdSecType amdSec, Node eventNode, String Id) {
        MdSecType mdSec = new MdSecType();
        mdSec.setID(Id);
        MdSecType.MdWrap mdWrap = new MdSecType.MdWrap();
        mdWrap.setMIMETYPE("text/xml");
        mdWrap.setMDTYPE("PREMIS");
        MdSecType.MdWrap.XmlData xmlData = new MdSecType.MdWrap.XmlData();
        xmlData.getAny().add(eventNode);
        mdWrap.setXmlData(xmlData);
        mdSec.setMdWrap(mdWrap);
        amdSec.getDigiprovMD().add(mdSec);
    }

    private Node createEventNode(String[] audiodescription) throws Exception {
        PremisComplexType premis = new PremisComplexType();
        ObjectFactory factory = new ObjectFactory();
        JAXBElement<PremisComplexType> jaxb = factory.createPremis(premis);
        EventComplexType event = factory.createEventComplexType();
        premis.getEvent().add(event);

        LinkingAgentIdentifierComplexType linkingAgentIdentifier = new LinkingAgentIdentifierComplexType();
        linkingAgentIdentifier.setLinkingAgentIdentifierType(getValue(audiodescription, "linkingAgentIdentifierType", 1));
        linkingAgentIdentifier.setLinkingAgentIdentifierValue(getValue(audiodescription, "linkingAgentIdentifierValue", 1));
        linkingAgentIdentifier.getLinkingAgentRole().add(getValue(audiodescription, "linkingAgentRole", 1));

        event.getLinkingAgentIdentifier().add(linkingAgentIdentifier);
        JAXBContext jc = JAXBContext.newInstance(PremisComplexType.class);
        return createNode(jaxb, jc, "*[local-name()='premis']/*[local-name()='event']");

    }

    private Node createAgentNode(String[] audiodescription) throws Exception {
        AgentComplexType agent = new AgentComplexType();
        ObjectFactory factory = new ObjectFactory();
        JAXBElement<AgentComplexType> jaxb = factory.createAgent(agent);
        agent.setAgentType(getValue(audiodescription, "agentType", 1));
        agent.getAgentName().add(getValue(audiodescription, "agentName", 1));
        ExtensionComplexType extension = factory.createExtensionComplexType();
        agent.getAgentExtension().add(extension);
        extension.getAny().add(addNkManufacturerNode(audiodescription));
        extension.getAny().add(addNkSerialNumberNode(audiodescription));
        extension.getAny().add(addNkSettingNode(audiodescription));

        JAXBContext jc = JAXBContext.newInstance(AgentComplexType.class);
        return createNode(jaxb, jc, "*[local-name()='agent']" );
    }



    private Node addNkSettingNode(String[] audiodescription) throws  Exception {
        NkSettingsComplexType nkSettings = new NkSettingsComplexType();
        AudioObjectFactory factory = new AudioObjectFactory();
        JAXBElement<NkSettingsComplexType> jaxb = factory.createNkSetting(nkSettings);
        nkSettings.setNkSettings((getValue(audiodescription, "NKsettings", 2)));
        JAXBContext jc = JAXBContext.newInstance(NkSettingsComplexType.class);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document document = db.newDocument();

            // Marshal the Object to a Document
            Marshaller marshaller = jc.createMarshaller();
            marshaller.marshal(jaxb, document);
            XPath xpath = XPathFactory.newInstance().newXPath();
            Node node = (Node) xpath.compile("*[local-name()='nkSettings']").evaluate(document, XPathConstants.NODE);
            return node;
    }


    private Node addNkSerialNumberNode(String[] audiodescription) throws Exception {
        NkSerialNumberComplexType nkSerialNumber = new NkSerialNumberComplexType();
        AudioObjectFactory factory = new AudioObjectFactory();
        JAXBElement<NkSerialNumberComplexType> jaxb = factory.createNkSerialNumber(nkSerialNumber);
        nkSerialNumber.setNkSerialNumber((getValue(audiodescription, "NKserialNumber", 2)));
        JAXBContext jc = JAXBContext.newInstance(NkSerialNumberComplexType.class);
        return createNode(jaxb, jc, "*[local-name()='nkSerialNumber']");
            /*DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document document = db.newDocument();

            // Marshal the Object to a Document
            Marshaller marshaller = jc.createMarshaller();
            marshaller.marshal(jaxbPremix, document);
            XPath xpath = XPathFactory.newInstance().newXPath();
            Node node = (Node) xpath.compile("*[local-name()='NkSerialNumber']").evaluate(document, XPathConstants.NODE);
            return node;
        } catch (Exception e) {
            throw new DeviceException("Error while generating NkManufacturer node in premis data", e);
        }*/
    }

    private Node addNkManufacturerNode(String[] audiodescription) throws Exception {
        NkManufacturerComplexType nkManufacturer = new NkManufacturerComplexType();
        AudioObjectFactory factory = new AudioObjectFactory();
        JAXBElement<NkManufacturerComplexType> jaxb = factory.createNkManufacturer(nkManufacturer);
        nkManufacturer.setNkManufacturer(getValue(audiodescription, "NKmanufacturer", 2));
        JAXBContext jc = JAXBContext.newInstance(NkManufacturerComplexType.class);

            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document document = db.newDocument();

            // Marshal the Object to a Document
            Marshaller marshaller = jc.createMarshaller();
            marshaller.marshal(jaxb, document);
            XPath xpath = XPathFactory.newInstance().newXPath();
                        Node nkManufacturerNode = (Node) xpath.compile("*[local-name()='nkManufacturer']").evaluate(document, XPathConstants.NODE);
            return nkManufacturerNode;


    }

    private String getValue(String[] audioDescription, String value, int number) {
        int position = -1;
        for (int i = 0; i < audioDescription.length; i++) {
            if (value.equals(audioDescription[i])) {
                position = i;
                break;
            }
        }
        if (position < 0) {
            return null;
        } else {
            int valuePosition = position + number;
            if (valuePosition >= 0 && valuePosition <= audioDescription.length) {
                return audioDescription[valuePosition];
            } else {
                return null;
            }
        }
    }
}
