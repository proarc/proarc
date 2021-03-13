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

import cz.cas.lib.proarc.aes57.Aes57Utils;
import cz.cas.lib.proarc.codingHistory.CodingHistoryUtils;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.fedora.AesEditor;
import cz.cas.lib.proarc.common.fedora.CodingHistoryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.MixEditor;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.mix.BasicDigitalObjectInformationType;
import cz.cas.lib.proarc.mix.BasicDigitalObjectInformationType.Compression;
import cz.cas.lib.proarc.mix.BasicDigitalObjectInformationType.ObjectIdentifier;
import cz.cas.lib.proarc.mix.BasicImageInformationType;
import cz.cas.lib.proarc.mix.BasicImageInformationType.BasicImageCharacteristics.PhotometricInterpretation;
import cz.cas.lib.proarc.mix.ChangeHistoryType;
import cz.cas.lib.proarc.mix.ChangeHistoryType.ImageProcessing;
import cz.cas.lib.proarc.mix.ImageCaptureMetadataType;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixType;
import cz.cas.lib.proarc.mix.MixUtils;
import cz.cas.lib.proarc.mix.OrientationType;
import cz.cas.lib.proarc.mix.StringType;
import cz.cas.lib.proarc.mix.TypeOfDateType;
import cz.cas.lib.proarc.mix.TypeOfOrientationType;
import edu.harvard.hul.ois.jhove.App;
import edu.harvard.hul.ois.jhove.JhoveBase;
import edu.harvard.hul.ois.jhove.Module;
import edu.harvard.hul.ois.jhove.OutputHandler;
import edu.harvard.hul.ois.xml.ns.jhove.Property;
import org.aes.audioobject.AudioObject;
import org.aes.audioobject.AudioObjectType;
import org.apache.commons.io.FileUtils;
import org.apache.xerces.dom.DeferredTextImpl;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import javax.xml.bind.JAXBElement;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathFactory;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.net.URL;
import java.util.Calendar;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Robert Simonovsky
 *
 *         Utility class for jHove application
 *
 */
public class JhoveUtility {

    private static final Logger LOG = Logger.getLogger(JhoveUtility.class.getName());
    static final String JHOVE_CONFIG_NAME = "jhove.conf";
    static {
        LOG.setLevel(Level.SEVERE);
    }

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

    public static Node getNodeRecursive(Node node, String localName, String value) {
        if ((node.getLocalName() != null) &&
                (node.getLocalName().startsWith(localName))) {
            System.out.println(localName);
        }
        if ((node.getLocalName() != null) &&
                (node.getLocalName().startsWith(localName)) &&
                (node.getChildNodes() != null) &&
                node.getChildNodes().getLength() > 0 &&
                node.getChildNodes().item(0) != null &&
                (((DeferredTextImpl)node.getChildNodes().item(0)).getData().startsWith(value))) {
            return node;
        } else {
            NodeList nl = node.getChildNodes();
            if (nl == null) {
                return null;
            }
            for (int a = 0; a < nl.getLength(); a++) {
                Node found = getNodeRecursive(nl.item(a), localName, value);
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
     * @param metsContext
     */
    public static void initJhove(MetsContext metsContext) throws MetsExportException {
        if (metsContext.getJhoveContext() == null) {
            File configFolder = new File(metsContext.getOutputPath(), metsContext.getPackageID());
            metsContext.setJhoveContext(createContext(configFolder));
        }
    }

    /**
     *
     * Inits the Jhove app
     *
     * @param metsContext
     * @param config
     */
    private static void initTempJhove(MetsContext metsContext, AppConfiguration config) throws MetsExportException {
        if (metsContext.getJhoveContext() == null) {
            File configFolder = new File(config.getConfigHome(), "temp");
            metsContext.setJhoveContext(createContext(configFolder));
        }
    }

    /**
     * Creates the JHOVE context and stores its configuration in a default temp folder.
     * Use {@link JhoveContext#destroy() } to remove temp folder.
     *
     * @return the context
     * @throws MetsExportException failure
     */
    public static JhoveContext createContext() throws MetsExportException {
        File temp = new File(FileUtils.getTempDirectory(), "jhove" + UUID.randomUUID().toString());
        if (!temp.mkdir()) {
            throw new MetsExportException("Cannot create " + temp.toString());
        }
        temp.deleteOnExit();
        return createContext(temp);
    }

    /**
     * Creates the JHOVE context and stores its configuration in the passed folder.
     * <p>{@link JhoveContext#destroy() } will remove the configuration folder!

     * @param configFolder folder to store configuration files
     * @return the context
     * @throws MetsExportException failure
     * @see #destroyConfigFiles
     */
    public static JhoveContext createContext(File configFolder) throws MetsExportException {
        Calendar calendar = Calendar.getInstance();

        App app = new App(JhoveUtility.class.getSimpleName(), "1.0", new int[] { calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH), calendar.get(Calendar.DAY_OF_MONTH) }, "jHove", "");
        try {
            JhoveBase jhoveBase = new JhoveBase();
            File jhoveConfigFile = createJhoveConfigurationFile(configFolder);
            jhoveBase.init(jhoveConfigFile.getAbsolutePath(), null);
            return new JhoveContext(jhoveBase, configFolder, app);
        } catch (Exception ex) {
            throw new MetsExportException("Error while initialising jHove", false, ex);
        }
    }

    /**
     * Gets MIX of a source image file.
     *
     * @param sourceFile image file to describe with MIX
     * @param tempFolder workspace for JHove
     * @param deviceMix optional device description
     * @param dateCreated optional date of creation of the source
     * @param originalFileName optional image file name
     * @return the MIX description
     * @throws MetsExportException failure
     */
    public static JHoveOutput getMix(File sourceFile, File tempFolder,
            MixType deviceMix, XMLGregorianCalendar dateCreated, String originalFileName
            ) throws MetsExportException {

        JhoveContext ctx = createContext(tempFolder);
        return getMix(sourceFile, ctx, deviceMix, dateCreated, null);
    }

    /**
     * Gets AES of a source image file.
     *
     * @param sourceFile image file to describe with AES
     * @param tempFolder workspace for JHove
     * @param aesDevice optional device description
     * @param dateCreated optional date of creation of the source
     * @param originalFileName optional image file name
     * @return the MIX description
     * @throws MetsExportException failure
     */
    public static JHoveOutput getAes(File sourceFile, File tempFolder,
                                     AudioObjectType aesDevice, XMLGregorianCalendar dateCreated, String originalFileName
    ) throws MetsExportException {

        JhoveContext ctx = createContext(tempFolder);
        return getAes(sourceFile, ctx, aesDevice, dateCreated, null);
    }

    /**
     * Gets Coding History of a source image file.
     *
     * @param sourceFile image file to describe with AES
     * @param tempFolder workspace for JHove
     * @param dateCreated optional date of creation of the source
     * @param originalFileName optional image file name
     * @return the MIX description
     * @throws MetsExportException failure
     */
    public static JHoveOutput getCodingHistory(File sourceFile, File tempFolder,
                                     XMLGregorianCalendar dateCreated, String originalFileName
    ) throws MetsExportException {

        JhoveContext ctx = createContext(tempFolder);
        return getCodingHistory(sourceFile, ctx, dateCreated, null);
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
    public static JHoveOutput getMix(File targetFile, MetsContext metsContext, MixType deviceMix, XMLGregorianCalendar dateCreated, String originalFileName) throws MetsExportException {
        initJhove(metsContext);
        JhoveContext jhoveContext = metsContext.getJhoveContext();
        return getMix(targetFile, jhoveContext, deviceMix, dateCreated, originalFileName);
    }

    /**
     *
     * Returns the AES data for fiven element
     *
     * @param targetFile
     * @param metsContext
     * @param deviceMix
     * @param dateCreated
     * @param originalFileName
     * @return
     * @throws MetsExportException
     */
    public static JHoveOutput getAes(File targetFile, MetsContext metsContext, AudioObjectType aes, XMLGregorianCalendar dateCreated, String originalFileName) throws MetsExportException {
        initJhove(metsContext);
        JhoveContext jhoveContext = metsContext.getJhoveContext();
        return getAes(targetFile, jhoveContext, aes, dateCreated, originalFileName);
    }

    public static JHoveOutput createAes(File targetFile, MetsContext metsContext, AudioObjectType aes, XMLGregorianCalendar dateCreated, String originalFileName, AppConfiguration config) throws MetsExportException {
        initTempJhove(metsContext, config);
        JhoveContext jhoveContext = metsContext.getJhoveContext();
        return getAes(targetFile, jhoveContext, aes, dateCreated, originalFileName);
    }

    /**
     *
     * Returns the AES data for fiven element
     *
     * @param targetFile
     * @param metsContext
     * @param dateCreated
     * @param originalFileName
     * @return
     * @throws MetsExportException
     */
    public static JHoveOutput getCodingHistory(File targetFile, MetsContext metsContext, XMLGregorianCalendar dateCreated, String originalFileName) throws MetsExportException {
        initJhove(metsContext);
        JhoveContext jhoveContext = metsContext.getJhoveContext();
        return getCodingHistory(targetFile, jhoveContext, dateCreated, originalFileName);
    }

    public static JHoveOutput createCodingHistory(File targetFile, MetsContext metsContext, XMLGregorianCalendar dateCreated, String originalFileName, AppConfiguration config) throws MetsExportException {
        initTempJhove(metsContext, config);
        JhoveContext jhoveContext = metsContext.getJhoveContext();
        return getCodingHistory(targetFile, jhoveContext, dateCreated, originalFileName);
    }

    /**
     * Returns the MIX information from the fedoraStream
     *
     * @param metsElement
     * @param streamName
     * @return
     * @throws MetsExportException
     */
    public static JHoveOutput getMixFromFedora(IMetsElement metsElement, String streamName) throws MetsExportException {
//        Document document = null;
        // hotfix of issue 250
        JHoveOutput jhoveOutput = new JHoveOutput();
        MixEditor mixEditor;
        RemoteObject fObj = metsElement.getMetsContext().getRemoteStorage().find(metsElement.getOriginalPid());
        if (MixEditor.RAW_ID.equals(streamName)) {
            mixEditor = MixEditor.raw(fObj);
        } else if (MixEditor.NDK_ARCHIVAL_ID.equals(streamName)) {
            mixEditor = MixEditor.ndkArchival(fObj);
        } else {
            return null;
        }
        Mix mix;
        try {
            mix = mixEditor.readMix();
            if (mix == null) {
                return null;
            }
        } catch (DigitalObjectException ex) {
            throw new MetsExportException(metsElement.getOriginalPid(), ex.getMessage(), false, ex);
        }
//        if (FoxmlUtils.findDatastream(metsElement.getSourceObject(), streamName) != null) {
//            List<Element> streamContent = MetsUtils.getDataStreams(metsElement.getMetsContext().getFedoraClient(), metsElement.getOriginalPid(), streamName);
//            if (streamContent == null) {
//                return null;
//            }
//            document = MetsUtils.getDocumentFromList(streamContent);
//        }
//        if (document == null) {
//            return null;
//        }
//        DOMSource domSource = new DOMSource(document);
//        MixType mix = MixUtils.unmarshal(domSource, MixType.class);
        jhoveOutput.setMix(mix);
        jhoveOutput.setFormatVersion(mix.getBasicDigitalObjectInformation().getFormatDesignation().getFormatName().getValue());
        return jhoveOutput;
    }

    /**
     * Returns the AES information from the fedoraStream
     *
     * @param metsElement
     * @param streamName
     * @return
     * @throws MetsExportException
     */
    public static JHoveOutput getAesFromFedora(IMetsElement metsElement, String streamName) throws MetsExportException {
//        Document document = null;
        // hotfix of issue 250
        JHoveOutput jhoveOutput = new JHoveOutput();
        AesEditor aesEditor;
        RemoteObject fObj = metsElement.getMetsContext().getRemoteStorage().find(metsElement.getOriginalPid());
        if (AesEditor.RAW_ID.equals(streamName)) {
            aesEditor = AesEditor.raw(fObj);
        } else if (AesEditor.NDK_ARCHIVAL_ID.equals(streamName)) {
            aesEditor = AesEditor.ndkArchival(fObj);
        } else {
            return null;
        }
        AudioObject aes;
        try {
            aes = aesEditor.readAes();
            if (aes == null) {
                return null;
            }
        } catch (DigitalObjectException ex) {
            throw new MetsExportException(metsElement.getOriginalPid(), ex.getMessage(), false, ex);
        }
//        if (FoxmlUtils.findDatastream(metsElement.getSourceObject(), streamName) != null) {
//            List<Element> streamContent = MetsUtils.getDataStreams(metsElement.getMetsContext().getFedoraClient(), metsElement.getOriginalPid(), streamName);
//            if (streamContent == null) {
//                return null;
//            }
//            document = MetsUtils.getDocumentFromList(streamContent);
//        }
//        if (document == null) {
//            return null;
//        }
//        DOMSource domSource = new DOMSource(document);
//        MixType mix = MixUtils.unmarshal(domSource, MixType.class);
        jhoveOutput.setAes(aes);
        return jhoveOutput;
    }

    /**
     * Returns the Coding history information from the fedoraStream
     *
     * @param metsElement
     * @param streamName
     * @return
     * @throws MetsExportException
     */
    public static JHoveOutput getCodingHistoryFromFedora(IMetsElement metsElement, String streamName) throws MetsExportException {
//        Document document = null;
        // hotfix of issue 250
        JHoveOutput jhoveOutput = new JHoveOutput();
        CodingHistoryEditor editor;
        RemoteObject fObj = metsElement.getMetsContext().getRemoteStorage().find(metsElement.getOriginalPid());
        if (CodingHistoryEditor.RAW_ID.equals(streamName)) {
            editor = CodingHistoryEditor.raw(fObj);
        } else if (CodingHistoryEditor.NDK_ARCHIVAL_ID.equals(streamName)) {
            editor = CodingHistoryEditor.ndkArchival(fObj);
        } else {
            return null;
        }
        Property codingHistory;
        try {
            codingHistory = editor.readCodingHistory();
            if (codingHistory == null) {
                return null;
            }
        } catch (DigitalObjectException ex) {
            throw new MetsExportException(metsElement.getOriginalPid(), ex.getMessage(), false, ex);
        }
        jhoveOutput.setCodingHistory(codingHistory);
        return jhoveOutput;
    }


    /**
     * Merges the mix from the device and from the image
     *
     * @param source
     * @param deviceMix
     */
    public static void mergeMix(Mix source, MixType deviceMix) {
        if (deviceMix != null) {
            if (deviceMix.getImageCaptureMetadata() != null) {
                DOMResult domResult = new DOMResult();
                MixUtils.marshal(domResult, new JAXBElement<ImageCaptureMetadataType>(new QName("uri", "local"), ImageCaptureMetadataType.class, deviceMix.getImageCaptureMetadata()), true);
                ImageCaptureMetadataType imageCaptureMtd = MixUtils.unmarshal(new DOMSource(domResult.getNode()), ImageCaptureMetadataType.class);
                source.setImageCaptureMetadata(imageCaptureMtd);
            }
        }

    }

    /**
     * Inserts ImageCaptureMetadata into Mix
     *
     * @param mix
     * @param dateCreated
     */
    public static void insertImageCaptureMetadata(Mix mix, XMLGregorianCalendar dateCreated) {
        // inserts DateCreated if missing
        if ((mix.getImageCaptureMetadata() == null) ||
                (mix.getImageCaptureMetadata().getGeneralCaptureInformation() == null) ||
                (mix.getImageCaptureMetadata().getGeneralCaptureInformation().getDateTimeCreated() == null)) {
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
    }

    /**
     * inserts ObjectIdentifier into mix
     *
     * @param mix
     * @param pid
     * @param datastream
     */
    public static void insertObjectIdentifier(Mix mix, String pid, String datastream) {
        mix.getBasicDigitalObjectInformation().getObjectIdentifier().clear();
        ObjectIdentifier identifier = new ObjectIdentifier();
        StringType stringTypeIdentifier = new StringType();
        stringTypeIdentifier.setValue("ProArc_URI");
        StringType stringTypeIdentifierValue = new StringType();
        stringTypeIdentifierValue.setValue(Const.FEDORAPREFIX + pid + "/" + Const.dataStreamToModel.get(datastream));
        identifier.setObjectIdentifierType(stringTypeIdentifier);
        identifier.setObjectIdentifierValue(stringTypeIdentifierValue);
        mix.getBasicDigitalObjectInformation().getObjectIdentifier().add(identifier);
    }

    /**
     * Inserts changeHistory into Mix
     *
     * @param mix
     * @param dateCreated
     * @param originalFileName
     */
    public static void insertChangeHistory(Mix mix, XMLGregorianCalendar dateCreated, String originalFileName) {
        if (mix.getChangeHistory() == null) {
            mix.setChangeHistory(new ChangeHistoryType());
        }
        if (mix.getChangeHistory().getImageProcessing().size() == 0) {
            ImageProcessing imageProcessing = new ChangeHistoryType.ImageProcessing();
            TypeOfDateType dateTimeProcessed = new TypeOfDateType();
            dateTimeProcessed.setValue(dateCreated.toXMLFormat());
            imageProcessing.setDateTimeProcessed(dateTimeProcessed);
            StringType sourceData = new StringType();
            sourceData.setValue(originalFileName);
            imageProcessing.setSourceData(sourceData);
            mix.getChangeHistory().getImageProcessing().add(imageProcessing);
        }
    }

    /**
     * Gets MIX of a source image file.
     *
     * @param sourceFile image file to describe with MIX
     * @param jhoveContext JHove
     * @param deviceMix optional device description
     * @param dateCreated optional date of creation of the source
     * @param originalFileName optional image file name
     * @return the MIX description
     * @throws MetsExportException failure
     */
    public static JHoveOutput getMix(File sourceFile, JhoveContext jhoveContext,
            MixType deviceMix, XMLGregorianCalendar dateCreated, String originalFileName
            ) throws MetsExportException {

        JHoveOutput jhoveOutput = new JHoveOutput();

        if (sourceFile == null || !sourceFile.isFile() || !sourceFile.exists()) {
            LOG.log(Level.SEVERE, "target file '" + sourceFile + "' cannot be found.");
            throw new MetsExportException("target file '" + sourceFile + "' cannot be found.", false, null);
        }
        try {
            JhoveBase jhoveBase = jhoveContext.getJhoveBase();
            File outputFile = File.createTempFile("jhove", "output");
            LOG.log(Level.FINE, "JHOVE output file " + outputFile);
            Module module = jhoveBase.getModule(null);
            OutputHandler aboutHandler = jhoveBase.getHandler(null);
            OutputHandler xmlHandler = jhoveBase.getHandler("XML");
            LOG.log(Level.FINE, "Calling JHOVE dispatch(...) on file " + sourceFile);
            jhoveBase.dispatch(jhoveContext.getJhoveApp(), module, aboutHandler, xmlHandler, outputFile.getAbsolutePath(), new String[] { sourceFile.getAbsolutePath() });
            DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
            builderFactory.setNamespaceAware(true);
            DocumentBuilder builder = builderFactory.newDocumentBuilder();
            Document jHoveDoc = builder.parse(outputFile);

            outputFile.delete();
            Node node = getNodeRecursive(jHoveDoc, "mix");
            if (node == null) {
                return jhoveOutput;
            }
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
            mergeMix(mix, deviceMix);
            // insert date time created
            if ((dateCreated != null) && (mix != null)) {
                insertImageCaptureMetadata(mix, dateCreated);
            }

            // insert ChangeHistory
            if ((dateCreated != null) && (originalFileName != null)) {
                insertChangeHistory(mix, dateCreated, originalFileName);
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
            throw new MetsExportException("Error inspecting file '" + sourceFile + "' - " + e.getMessage(), false, e);
        }
        return jhoveOutput;
    }

    /**
     * Gets Aes of a source image file.
     *
     * @param sourceFile image file to describe with AES
     * @param jhoveContext JHove
     * @param aesDevice optional device description
     * @param dateCreated optional date of creation of the source
     * @param originalFileName optional image file name
     * @return the MIX description
     * @throws MetsExportException failure
     */
    public static JHoveOutput getAes(File sourceFile, JhoveContext jhoveContext,
                                     AudioObjectType  aesDevice, XMLGregorianCalendar dateCreated, String originalFileName
    ) throws MetsExportException {

        JHoveOutput jhoveOutput = new JHoveOutput();

        if (sourceFile == null || !sourceFile.isFile() || !sourceFile.exists()) {
            LOG.log(Level.SEVERE, "target file '" + sourceFile + "' cannot be found.");
            throw new MetsExportException("target file '" + sourceFile + "' cannot be found.", false, null);
        }
        try {
            JhoveBase jhoveBase = jhoveContext.getJhoveBase();
            File outputFile = File.createTempFile("jhove", "output");
            LOG.log(Level.FINE, "JHOVE output file " + outputFile);
            Module module = jhoveBase.getModule(null);
            OutputHandler aboutHandler = jhoveBase.getHandler(null);
            OutputHandler xmlHandler = jhoveBase.getHandler("XML");
            LOG.log(Level.FINE, "Calling JHOVE dispatch(...) on file " + sourceFile);
            jhoveBase.dispatch(jhoveContext.getJhoveApp(), module, aboutHandler, xmlHandler, outputFile.getAbsolutePath(), new String[] { sourceFile.getAbsolutePath() });
            DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
            builderFactory.setNamespaceAware(true);
            DocumentBuilder builder = builderFactory.newDocumentBuilder();
            Document jHoveDoc = builder.parse(outputFile);

            outputFile.delete();
            Node node = getNodeRecursive(jHoveDoc, "audioObject");
            if (node == null) {
                return jhoveOutput;
            }
            AudioObject aes = Aes57Utils.unmarshal(new DOMSource(node), AudioObject.class);

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

            jhoveOutput.setAes(aes);
        } catch (Exception e) {
            throw new MetsExportException("Error inspecting file '" + sourceFile + "' - " + e.getMessage(), false, e);
        }
        return jhoveOutput;
    }

    /**
     * Gets Coding History of a source image file.
     *
     * @param sourceFile image file to describe with AES
     * @param jhoveContext JHove
     * @param dateCreated optional date of creation of the source
     * @param originalFileName optional image file name
     * @return the MIX description
     * @throws MetsExportException failure
     */
    public static JHoveOutput getCodingHistory(File sourceFile, JhoveContext jhoveContext,
                                     XMLGregorianCalendar dateCreated, String originalFileName
    ) throws MetsExportException {

        JHoveOutput jhoveOutput = new JHoveOutput();

        if (sourceFile == null || !sourceFile.isFile() || !sourceFile.exists()) {
            LOG.log(Level.SEVERE, "target file '" + sourceFile + "' cannot be found.");
            throw new MetsExportException("target file '" + sourceFile + "' cannot be found.", false, null);
        }
        try {
            JhoveBase jhoveBase = jhoveContext.getJhoveBase();
            File outputFile = File.createTempFile("jhove", "output");
            LOG.log(Level.FINE, "JHOVE output file " + outputFile);
            Module module = jhoveBase.getModule(null);
            OutputHandler aboutHandler = jhoveBase.getHandler(null);
            OutputHandler xmlHandler = jhoveBase.getHandler("XML");
            LOG.log(Level.FINE, "Calling JHOVE dispatch(...) on file " + sourceFile);
            jhoveBase.dispatch(jhoveContext.getJhoveApp(), module, aboutHandler, xmlHandler, outputFile.getAbsolutePath(), new String[] { sourceFile.getAbsolutePath() });
            DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
            builderFactory.setNamespaceAware(true);
            DocumentBuilder builder = builderFactory.newDocumentBuilder();
            Document jHoveDoc = builder.parse(outputFile);

            outputFile.delete();
            Node nodeExtension = getNodeRecursive(jHoveDoc, "name", "BroadcastAudioExtension");
            if (nodeExtension == null) {
                return jhoveOutput;
            }
            Property codingHistory = CodingHistoryUtils.unmarshal(new DOMSource(nodeExtension.getParentNode()), Property.class);


            XPath xpath = XPathFactory.newInstance().newXPath();
            String formatVersion = xpath.compile("*[local-name()='jhove']/*[local-name()='repInfo']/*[local-name()='version']").evaluate(jHoveDoc);
            if ((formatVersion == null) || ("0".equals(formatVersion)) || (formatVersion.trim().length() == 0)) {
                formatVersion = "1.0";
            }
            jhoveOutput.setFormatVersion(formatVersion);
            jhoveOutput.setCodingHistory(codingHistory);
        } catch (Exception e) {
            throw new MetsExportException("Error inspecting file '" + sourceFile + "' - " + e.getMessage(), false, e);
        }
        return jhoveOutput;
    }

    /**
     * Copy the Jhove configuration file to a temporary file.
     *
     * @return the {@link File} where the Jhove configuration was saved.
     *
     */
    private static File createJhoveConfigurationFile(File configFolder) throws MetsExportException {
        URL jhoveConf = JhoveUtility.class.getResource(JHOVE_CONFIG_NAME);
        URL jhoveConfXsd = JhoveUtility.class.getResource("jhoveConfig.xsd");
        try {
            File jhoveConfFile = new File(configFolder, JHOVE_CONFIG_NAME);
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
     * Removes JHOVE configuration files (not folder) used by the context.
     * It is here not to break {@link cz.cas.lib.proarc.common.export.mets.structure.MetsElementVisitor}.
     * @param ctx context
     */
    public static void destroyConfigFiles(JhoveContext ctx) {
        if (ctx == null) {
            return ;
        }
        File configDir = ctx.getConfigFolder();
        File jhoveConfFile = new File(configDir, JHOVE_CONFIG_NAME);
        LOG.log(Level.FINE, "JHOVE configuration file " + jhoveConfFile);
        if (jhoveConfFile.exists()) {
            jhoveConfFile.delete();
        }
        File xsdFile = new File(configDir, "jhoveConfig.xsd");
        if (xsdFile.exists()) {
            xsdFile.delete();
        }
    }

    /**
     * adds denominator value
     *
     * @param jhoveOutput
     */
    public static void addDenominator(JHoveOutput jhoveOutput) {
        if ((jhoveOutput != null) && (jhoveOutput.getMix() != null)) {
            if ((jhoveOutput.getMix().getImageAssessmentMetadata() != null) && (jhoveOutput.getMix().getImageAssessmentMetadata().getSpatialMetrics() != null)) {
                if ((jhoveOutput.getMix().getImageAssessmentMetadata().getSpatialMetrics().getXSamplingFrequency() != null) && (jhoveOutput.getMix().getImageAssessmentMetadata().getSpatialMetrics().getXSamplingFrequency().getDenominator() == null)) {
                    jhoveOutput.getMix().getImageAssessmentMetadata().getSpatialMetrics().getXSamplingFrequency().setDenominator(BigInteger.ONE);
                }
                if ((jhoveOutput.getMix().getImageAssessmentMetadata().getSpatialMetrics().getXSamplingFrequency() != null) && (jhoveOutput.getMix().getImageAssessmentMetadata().getSpatialMetrics().getYSamplingFrequency().getDenominator() == null)) {
                    jhoveOutput.getMix().getImageAssessmentMetadata().getSpatialMetrics().getYSamplingFrequency().setDenominator(BigInteger.ONE);
                }
            }
        }
    }

    /**
     * Adds the photometric information to the mix
     *
     *
     * @param jhoveOutput
     * @param photometricInterpretation
     */
    public static void addPhotometricInformation(JHoveOutput jhoveOutput, PhotometricInterpretation photometricInterpretation) {
        if (photometricInterpretation != null) {
            if (jhoveOutput.getMix().getBasicImageInformation() == null) {
                jhoveOutput.getMix().setBasicImageInformation(new BasicImageInformationType());
            }
            if (jhoveOutput.getMix().getBasicImageInformation().getBasicImageCharacteristics() == null) {
                jhoveOutput.getMix().getBasicImageInformation().setBasicImageCharacteristics(new BasicImageInformationType.BasicImageCharacteristics());
            }
            if (jhoveOutput.getMix().getBasicImageInformation().getBasicImageCharacteristics().getPhotometricInterpretation() == null) {

                DOMResult photometricResult = new DOMResult();
                MixUtils.marshal(photometricResult, new JAXBElement<PhotometricInterpretation>(new QName("uri", "local"), PhotometricInterpretation.class, photometricInterpretation), true);

                PhotometricInterpretation photometricInterpretationNew = MixUtils.unmarshal(new DOMSource(photometricResult.getNode()), PhotometricInterpretation.class);
                jhoveOutput.getMix().getBasicImageInformation().getBasicImageCharacteristics().setPhotometricInterpretation(photometricInterpretationNew);
            }
        }
    }

    /**
     * Adds an orientation tag to the mix
     *
     * @param jhoveOutput
     */
    public static void addOrientation(JHoveOutput jhoveOutput) {
        if ((jhoveOutput!=null)&&(jhoveOutput.getMix()!=null)) {
            if ((jhoveOutput.getMix().getImageCaptureMetadata() != null) && (jhoveOutput.getMix().getImageCaptureMetadata().getOrientation() == null)) {
                TypeOfOrientationType orientation = new TypeOfOrientationType();
                orientation.setValue(OrientationType.UNKNOWN);
                jhoveOutput.getMix().getImageCaptureMetadata().setOrientation(orientation);
            }
        }
    }
}
