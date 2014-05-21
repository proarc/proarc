/*
 * Copyright (C) 2013 Robert Simonovsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cas.lib.proarc.common.export.mets;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Calendar;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathFactory;

import org.apache.commons.io.FileUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import edu.harvard.hul.ois.jhove.App;
import edu.harvard.hul.ois.jhove.JhoveBase;
import edu.harvard.hul.ois.jhove.Module;
import edu.harvard.hul.ois.jhove.OutputHandler;

/**
 * @author eskymo
 *
 *         Utility class for jHove application
 *
 */
public class JhoveUtility {

    private static final Logger LOG = Logger.getLogger(JhoveUtility.class.getName());

    /**
     * Returns a node with MIX info - helper
     *
     * @param node
     * @return
     */
    public static Node getGeneralCaptureInformationRecursive(Node node) {
        if (node.getLocalName() == null) {
            return null;
        }
        if ((node.getLocalName().startsWith("GeneralCaptureInformation"))) {
            return node;
        } else {
            NodeList nl = node.getChildNodes();
            for (int a = 0; a < nl.getLength(); a++) {
                Node mix = getGeneralCaptureInformationRecursive(nl.item(a));
                if (mix != null) {
                    return mix;
                }
            }
        }
        return null;
    }

    /**
     * Returns a node with MIX info - helper
     *
     * @param node
     * @return
     */
    private static Node getMixRecursive(Node node) {
        if ((node.getNodeName().startsWith("mix"))) {
            return node;
        } else {
            NodeList nl = node.getChildNodes();
            for (int a = 0; a < nl.getLength(); a++) {
                Node mix = getMixRecursive(nl.item(a));
                if (mix != null) {
                    return mix;
                }
            }
        }
        return null;
    }

    /**
     *
     * Inits the Jhove app
     *
     * @param metsInfo
     */
    public static void initJhove(MetsContext metsContext) throws MetsExportException {
        Calendar calendar = Calendar.getInstance();

        App app = new App(JhoveUtility.class.getSimpleName(), "1.0", new int[] { calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH), calendar.get(Calendar.DAY_OF_MONTH) }, "jHove", "");
        try {
            JhoveBase jhoveBase = new JhoveBase();
            File jhoveConfigFile = createJhoveConfigurationFile(metsContext);
            jhoveBase.init(jhoveConfigFile.getAbsolutePath(), null);
            metsContext.jhoveBase = jhoveBase;
            metsContext.jhoveApp = app;
            metsContext.jhoveConfig = jhoveConfigFile.getParent();
        } catch (Exception ex) {
            throw new MetsExportException("Error while initialising jHove", false, ex);
        }
    }

    private static void mergeMix(Node source, Document document, Node deviceMix) {
        NodeList nl = deviceMix.getChildNodes();
        for (int a = 0; a < nl.getLength(); a++) {
            Node child = nl.item(a);
            Node adoptedNode = child.cloneNode(true);
            document.adoptNode(adoptedNode);
            source.appendChild(adoptedNode);
        }
    }

    /**
     *
     * Returns a MIX node
     *
     * @param targetFile
     * @param metsinfo
     * @return
     */
    public static JHoveOutput getMixNode(File targetFile, MetsContext metsContext, Node deviceMix, XMLGregorianCalendar dateCreated) throws MetsExportException {
        JHoveOutput jhoveOutput = new JHoveOutput();

        if (targetFile == null || !targetFile.isFile() || !targetFile.exists()) {
            LOG.log(Level.SEVERE, "target file '" + targetFile + "' cannot be found.");
            throw new MetsExportException("target file '" + targetFile + "' cannot be found.", false, null);
        }
        if (metsContext.jhoveBase == null) {
            initJhove(metsContext);
        }
        try {
            File outputFile = File.createTempFile("jhove", "output");
            LOG.log(Level.FINE, "JHOVE output file " + outputFile);
            Module module = metsContext.jhoveBase.getModule(null);
            OutputHandler aboutHandler = metsContext.jhoveBase.getHandler(null);
            OutputHandler xmlHandler = metsContext.jhoveBase.getHandler("XML");
            LOG.log(Level.FINE, "Calling JHOVE dispatch(...) on file " + targetFile);
            metsContext.jhoveBase.dispatch(metsContext.jhoveApp, module, aboutHandler, xmlHandler, outputFile.getAbsolutePath(), new String[] { targetFile.getAbsolutePath() });
            DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
            builderFactory.setNamespaceAware(true);
            DocumentBuilder builder = builderFactory.newDocumentBuilder();
            Document jHoveDoc = builder.parse(outputFile);

            outputFile.delete();
            Node node = getMixRecursive(jHoveDoc);
            if ((deviceMix != null) && (node != null)) {
                mergeMix(node, jHoveDoc, deviceMix);
            }
            if ((dateCreated != null) && (node != null)) {
                Element elm = jHoveDoc.createElementNS("http://www.loc.gov/mix/v20", "dateTimeCreated");
                Node generalInfo = getGeneralCaptureInformationRecursive(node);
                if (generalInfo != null) {
                    generalInfo.appendChild(elm);
                    elm.setTextContent(dateCreated.toXMLFormat());
                }
            }
            jhoveOutput.setMixNode(node);
            XPath xpath =  XPathFactory.newInstance().newXPath();
            String formatVersion = xpath.compile("*[local-name()='jhove']/*[local-name()='repInfo']/*[local-name()='version']").evaluate(jHoveDoc);
            if ((formatVersion == null) || ("0".equals(formatVersion))) {
                formatVersion = "1.0";
            }
            jhoveOutput.setFormatVersion(formatVersion);
            return jhoveOutput;
        } catch (Exception e) {
            metsContext.getMetsExportException().addException("Error inspecting file '" + targetFile + "' - " + e.getMessage(), true, e);
        }
        return jhoveOutput;
    }

    /**
     * Copy the Jhove configuration file to a temporary file.
     *
     * @return the {@link File} where the Jhove configuration was saved.
     *
     */
    private synchronized static File createJhoveConfigurationFile(MetsContext metsContext) throws MetsExportException {
        URL jhoveConf = JhoveUtility.class.getResource("jhove.conf");
        URL jhoveConfXsd = JhoveUtility.class.getResource("jhoveConfig.xsd");
        try {
            File jhoveConfFile = new File(metsContext.getOutputPath() + File.separator + metsContext.getPackageID(), "jhove.conf");
            LOG.log(Level.FINE, "JHOVE configuration file " + jhoveConfFile);
            if (!jhoveConfFile.exists()) {
                FileUtils.copyURLToFile(jhoveConf, jhoveConfFile);
            }
            File xsdFile = new File(jhoveConfFile.getParent(), "jhoveConfig.xsd");
            if (!xsdFile.exists()) {
                FileUtils.copyURLToFile(jhoveConfXsd, xsdFile);
            }
            return jhoveConfFile;
        } catch (IOException ex) {
            throw new MetsExportException("Unable to create jHove config file", false, ex);
        }
    }

    /**
     * Copy the Jhove configuration file to a temporary file.
     *
     * @return the {@link File} where the Jhove configuration was saved.
     *
     */
    public synchronized static void destroyConfigFiles(String configDir) throws MetsExportException {
            File jhoveConfFile = new File(configDir + File.separator + "jhove.conf");
            LOG.log(Level.FINE, "JHOVE configuration file " + jhoveConfFile);
            if (jhoveConfFile.exists()) {
                jhoveConfFile.delete();
            }
            File xsdFile = new File(configDir + File.separator + "jhoveConfig.xsd");
            if (xsdFile.exists()) {
                xsdFile.delete();
        }
    }
}