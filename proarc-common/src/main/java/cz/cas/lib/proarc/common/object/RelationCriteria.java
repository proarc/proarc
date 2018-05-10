package cz.cas.lib.proarc.common.object;

import java.io.IOException;
import java.io.StringReader;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import static cz.cas.lib.proarc.common.object.ndk.NdkPlugin.MODEL_PERIODICALISSUE;
import static cz.cas.lib.proarc.common.object.ndk.NdkPlugin.MODEL_PERIODICALVOLUME;

/**
 * Provides object - object relation validation based on hierarchy rules
 *
 * @author Jakub Kremlacek
 */
public class RelationCriteria {

    /**
     * PID - used for simple object-object relation based only on object pids
     * NDK_SUPPLEMENT - parses childrens metadata to check mods:genre value
     */
    public enum Type {
        PID, NDK_SUPPLEMENT
    }

    private static final Logger LOG = Logger.getLogger(RelationCriteria.class.getName());

    private String pid;
    private Type type;

    /**
     * Creates new RelationCriteraia
     *
     * @param pid parent pid
     * @param type criteria type, see RelationCriteria.Type
     */
    public RelationCriteria(String pid, Type type) {
        this.pid = pid;
        this.type = type;
    }

    public boolean isSatisfied(DigitalObjectHandler childHandler, String parentPid, StringBuilder reason) {
        if (pid != parentPid) {
            return false;
        }

        switch (type) {
            case PID:
                return true;
            case NDK_SUPPLEMENT:
                String genre = "";

                try {
                    genre = loadXMLFromString(childHandler.metadata().getMetadataAsXml().getData()).getElementsByTagName("mods:genre").item(0).getTextContent();
                } catch (Exception e) {
                    LOG.log(Level.INFO, "Cannot retrieve mods:genre.");
                }

                if (pid == MODEL_PERIODICALISSUE) {
                    if (genre.equals("issue_supplement")) {
                        return true;
                    } else {
                        reason.append(pid + " must have issue_supplement genre in order to be connected to " + MODEL_PERIODICALISSUE + "<br/>");
                        return false;
                    }
                } else if (pid == MODEL_PERIODICALVOLUME) {
                    if (genre.equals("volume_supplement")) {
                        return true;
                    } else {
                        reason.append(pid + " must have volume_supplement genre in order to be connected to " + MODEL_PERIODICALVOLUME + "<br/>");
                        return false;
                    }
                }

                return false;
            default:
                LOG.log(Level.WARNING, "Unknown RelationCriteria type.");
                return false;
        }
    }

    public static Document loadXMLFromString(String xml) throws ParserConfigurationException, IOException, SAXException {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        InputSource is = new InputSource(new StringReader(xml));
        return builder.parse(is);
    }
}


