

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
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBElement;
import jakarta.xml.bind.Marshaller;
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

    public static final String NS = "info:ndk/xmlns/nk-v1";

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
            addNodeToPremis(amdSec, createPremisNode(audioDescriptionTokens), "premis");
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

    private Node createNode(JAXBElement jaxb, JAXBContext jc, String expression) throws Exception {
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

    private Node createPremisNode(String[] audiodescription) throws Exception {
        PremisComplexType premis = new PremisComplexType();
        ObjectFactory factory = new ObjectFactory();
        JAXBElement<PremisComplexType> jaxb = factory.createPremis(premis);
        EventComplexType event = factory.createEventComplexType();
        AgentComplexType agent = factory.createAgentComplexType();
        premis.getEvent().add(event);
        premis.getAgent().add(agent);

        LinkingAgentIdentifierComplexType linkingAgentIdentifier = new LinkingAgentIdentifierComplexType();
        linkingAgentIdentifier.setLinkingAgentIdentifierType(getValue(audiodescription, "linkingAgentIdentifierType", 1));
        linkingAgentIdentifier.setLinkingAgentIdentifierValue(getValue(audiodescription, "linkingAgentIdentifierValue", 1));
        linkingAgentIdentifier.getLinkingAgentRole().add(getValue(audiodescription, "linkingAgentRole", 1));
        event.getLinkingAgentIdentifier().add(linkingAgentIdentifier);

        agent.setAgentType(getValue(audiodescription, "agentType", 1));
        agent.getAgentName().add(getValue(audiodescription, "agentName", 1));
        ExtensionComplexType extension = factory.createExtensionComplexType();
        agent.getAgentExtension().add(extension);
        extension.getAny().add(addNkNode(audiodescription));

        JAXBContext jc = JAXBContext.newInstance(PremisComplexType.class);
        return createNode(jaxb, jc, "*[local-name()='premis']");
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

    private Node addNkNode(String[] audioDescription) throws Exception {
        NkComplexType nk = new NkComplexType();
        AudioObjectFactory factory = new AudioObjectFactory();
        JAXBElement<NkComplexType> jaxb = factory.createNk(nk);

        nk.setManufacturer(getValue(audioDescription, "manufacturer", 1));
        nk.setSerialNumber(getValue(audioDescription, "serialNumber", 1));
        nk.setSettings(getValue(audioDescription, "settings", 1));

        JAXBContext jc = JAXBContext.newInstance(NkComplexType.class);
        return createNode(jaxb, jc, "*[local-name()='nk']");
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
