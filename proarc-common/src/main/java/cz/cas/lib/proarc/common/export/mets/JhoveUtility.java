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

import javax.xml.bind.JAXBElement;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathFactory;

import org.apache.commons.io.FileUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import cz.cas.lib.proarc.mix.BasicDigitalObjectInformationType.Compression;
import cz.cas.lib.proarc.mix.ChangeHistoryType;
import cz.cas.lib.proarc.mix.ChangeHistoryType.ImageProcessing;
import cz.cas.lib.proarc.mix.BasicDigitalObjectInformationType;
import cz.cas.lib.proarc.mix.ImageCaptureMetadataType;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import cz.cas.lib.proarc.mix.StringType;
import cz.cas.lib.proarc.mix.TypeOfDateType;
import edu.harvard.hul.ois.jhove.App;
import edu.harvard.hul.ois.jhove.JhoveBase;
import edu.harvard.hul.ois.jhove.Module;
import edu.harvard.hul.ois.jhove.OutputHandler;

/**
 * @author Robert Simonovsky
 *
 *         Utility class for jHove application
 *
 */
public class JhoveUtility {

    private static final Logger LOG = Logger.getLogger(JhoveUtility.class.getName());

    public static Node getNodeRecursive(Node node, String localName) {
        if ((node.getLocalName() != null) && (node.getLocalName().startsWith(localName))) {
            return node;
        } else {
            NodeList nl = node.getChildNodes();
            if (nl == null) {
                return null;
            }
            for (int a = 0; a < nl.getLength(); a++) {
                Node found = getNodeRecursive(nl.item(a), localName);
                if (found != null) {
                    return found;
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


    /**
     *
     * Returns the MIX data for fiven element
     *
     * @param targetFile
     * @param metsContext
     * @param deviceMix
     * @param dateCreated
     * @param originalFileName
     * @return
     * @throws MetsExportException
     */
    public static JHoveOutput getMix(File targetFile, MetsContext metsContext, Mix deviceMix, XMLGregorianCalendar dateCreated, String originalFileName) throws MetsExportException {
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
            Node node = getNodeRecursive(jHoveDoc, "mix");
            Mix mix = MixUtils.unmarshal(new DOMSource(node), Mix.class);

            XPath xpath = XPathFactory.newInstance().newXPath();
            String formatVersion = xpath.compile("*[local-name()='jhove']/*[local-name()='repInfo']/*[local-name()='version']").evaluate(jHoveDoc);
            if ((formatVersion == null) || ("0".equals(formatVersion)) || (formatVersion.trim().length() == 0)) {
                formatVersion = "1.0";
            }
            String formatName = xpath.compile("*[local-name()='jhove']/*[local-name()='repInfo']/*[local-name()='mimeType']").evaluate(jHoveDoc);
            if ((formatName == null) || (formatName.trim().length() == 0)) {
                formatName = "unknown";
            }
            jhoveOutput.setFormatVersion(formatVersion);
            // merge device and jhove Mix
            if (deviceMix != null) {
                if (deviceMix.getImageCaptureMetadata() != null) {
                    DOMResult domResult = new DOMResult();
                    MixUtils.marshal(domResult, new JAXBElement<ImageCaptureMetadataType>(new QName("uri", "local"), ImageCaptureMetadataType.class, deviceMix.getImageCaptureMetadata()), true);
                    ImageCaptureMetadataType imageCaptureMtd = MixUtils.unmarshal(new DOMSource(domResult.getNode()), ImageCaptureMetadataType.class);
                    mix.setImageCaptureMetadata(imageCaptureMtd);
                }
            }

            // insert date time created
            if ((dateCreated != null) && (mix != null)) {
                TypeOfDateType dateTimeCreated = new TypeOfDateType();
                dateTimeCreated.setValue(dateCreated.toXMLFormat());
                if (mix.getImageCaptureMetadata() == null) {
                    mix.setImageCaptureMetadata(new ImageCaptureMetadataType());
                }
                if (mix.getImageCaptureMetadata().getGeneralCaptureInformation() == null) {
                    mix.getImageCaptureMetadata().setGeneralCaptureInformation(new ImageCaptureMetadataType.GeneralCaptureInformation());
                }
                mix.getImageCaptureMetadata().getGeneralCaptureInformation().setDateTimeCreated(dateTimeCreated);
            }

            // insert ChangeHistory
            if ((dateCreated != null) && (originalFileName != null)) {
                if (mix.getChangeHistory() == null) {
                    mix.setChangeHistory(new ChangeHistoryType());
                }
                ImageProcessing imageProcessing = new ChangeHistoryType.ImageProcessing();
                TypeOfDateType dateTimeProcessed = new TypeOfDateType();
                dateTimeProcessed.setValue(dateCreated.toXMLFormat());
                imageProcessing.setDateTimeProcessed(dateTimeProcessed);
                StringType sourceData = new StringType();
                sourceData.setValue(originalFileName);
                imageProcessing.setSourceData(sourceData);
                mix.getChangeHistory().getImageProcessing().add(imageProcessing);
            }

            // add formatVersion
            if (mix != null) {
                if (mix.getBasicDigitalObjectInformation() == null) {
                    mix.setBasicDigitalObjectInformation(new BasicDigitalObjectInformationType());
                }
                if (mix.getBasicDigitalObjectInformation().getFormatDesignation() == null) {
                    mix.getBasicDigitalObjectInformation().setFormatDesignation(new BasicDigitalObjectInformationType.FormatDesignation());
                }
                StringType formatNameType = new StringType();
                StringType formatVersionType = new StringType();
                formatNameType.setValue(formatName);
                formatVersionType.setValue(formatVersion);
                mix.getBasicDigitalObjectInformation().getFormatDesignation().setFormatName(formatNameType);
                mix.getBasicDigitalObjectInformation().getFormatDesignation().setFormatVersion(formatVersionType);
            }

            // workarround for bug in Jhove - Unknown compression for jpeg2000
            if ("image/jp2".equals(formatName)) {
                if (mix.getBasicDigitalObjectInformation() == null) {
                    mix.setBasicDigitalObjectInformation(new BasicDigitalObjectInformationType());
                }
                mix.getBasicDigitalObjectInformation().getCompression().clear();
                Compression compression = new BasicDigitalObjectInformationType.Compression();
                StringType jpeg2000Type = new StringType();
                jpeg2000Type.setValue("JPEG 2000");
                compression.setCompressionScheme(jpeg2000Type);
                mix.getBasicDigitalObjectInformation().getCompression().add(compression);
            }
            jhoveOutput.setMix(mix);
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