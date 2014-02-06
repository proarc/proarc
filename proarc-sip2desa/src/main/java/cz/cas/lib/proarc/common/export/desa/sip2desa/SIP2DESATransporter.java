/*
 * Copyright (C) 2013 Vladimir Lahoda, Robert Simonovsky
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

package cz.cas.lib.proarc.common.export.desa.sip2desa;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URL;
import java.net.URLConnection;
import java.security.MessageDigest;
import java.util.Date;
import java.util.Enumeration;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.PropertyException;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamReader;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.sax.SAXSource;
import javax.xml.ws.BindingProvider;
import javax.xml.ws.Holder;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import com.google.common.io.ByteStreams;
import com.google.common.io.Files;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;

import cz.cas.lib.proarc.common.export.desa.sip2desa.api.FileHashAlg;
import cz.cas.lib.proarc.common.export.desa.sip2desa.api.NomenclatureListType;
import cz.cas.lib.proarc.common.export.desa.sip2desa.api.SIPSubmission;
import cz.cas.lib.proarc.common.export.desa.sip2desa.api.SIPSubmissionFault;
import cz.cas.lib.proarc.common.export.desa.sip2desa.api.SIPSubmissionService;
import cz.cas.lib.proarc.common.export.desa.sip2desa.nomen.Nomenclatures;
import cz.cas.lib.proarc.common.export.desa.sip2desa.protocol.ObjectFactory;
import cz.cas.lib.proarc.common.export.desa.sip2desa.protocol.PSPSIP;
import cz.cas.lib.proarc.common.export.desa.sip2desa.protocol.ResultType;
import cz.cas.lib.proarc.common.export.desa.sip2desa.protocol.SipType;

/**
 * Main class for import of the SIP packages to DESA repository and for checking
 * the progress of the import.
 */
public class SIP2DESATransporter {

    private static final Logger log = Logger.getLogger(SIP2DESATransporter.class.toString());

    private Marshaller marshaller = null;
    private Unmarshaller unmarshaller = null;

    private Unmarshaller unmarshallerNomen = null;

    private File[] sourceFiles = null;

    private int filesIndex = -1;

    private int mtdpspIndex = -1;

    private String packageid = null;

    private final ObjectFactory resultsFactory = new ObjectFactory();

    private PSPSIP results = resultsFactory.createPSPSIP();

    /**
     * Returns the parsed result from the result file
     *
     * @return
     */
    public PSPSIP getResults() {
        return results;
    }

    private Config config;

    private File desaFolder = null;

    private SIPSubmission desaPort = null;

    private File resultsFolder = null;

    private File sourceFolder = null;

    private String logRoot = "";

    /**
     * The transporter entry point
     *
     * @param args
     *            command line arguments for transport mode : transport
     *            <input-folder> <results-folder> <log-folder> command line
     *            arguments for checkStatus mode : checkStatus <results-file>
     *            <log-folder>
     */
    public static void main(String[] args) {
        try {
            if (args.length == 4 && "transport".equalsIgnoreCase(args[0])) {
                String importRoot = args[1];
                String resultsRoot = args[2];
                String logRoot = args[3];
                new SIP2DESATransporter().transport(importRoot, resultsRoot, logRoot);
            } else if (args.length == 3 && "checkStatus".equalsIgnoreCase(args[0])) {
                String resultsRoot = args[1];
                String logRoot = args[2];
                new SIP2DESATransporter().checkStatus(resultsRoot, logRoot);
            } else if (args.length == 2 && "adminUpload".equalsIgnoreCase(args[0])) {
                String importRoot = args[1];
                new SIP2DESATransporter().adminUpload(importRoot);
            } else {
                System.out.println("SIP to DESA transporter.\n");
                System.out.println("Usage for transport: sip2desa.SIP2DESATransporter transport <input-folder> <results-folder> <log-folder>");
                System.out.println("Usage for checkStatus: sip2desa.SIP2DESATransporter checkStatus <results-file> <log-folder>");
                System.out.println("Usage for adminUpload: sip2desa.SIP2DESATransporter adminUpload <input-folder>");

                System.exit(1);
            }
        } catch (Exception e) {
            log.log(Level.SEVERE, e.getMessage(), e);
        }
    }

    /**
     * Main execution method for the transport mode. Imports the SIP files in
     * the sourceRoot to DESA repository (configured in config property file).
     * The status of the import of each SIP is written in the results XML file
     * named TRANSF_packageid.xml in the resultsRoot folder. The JDK logging
     * output is mirrored in the packageid.log file in the logRoot folder.
     *
     * @param sourceRoot
     * @param resultsRoot
     * @param logRootIn
     */
    public void transport(String sourceRoot, String resultsRoot, String logRootIn) {
        transport(sourceRoot, resultsRoot, logRootIn, null);
    }

    /**
     * Main execution method for the transport mode. Imports the SIP files in
     * the sourceRoot to DESA repository (configured in config property file).
     * The status of the import of each SIP is written in the results XML file
     * named TRANSF_packageid.xml in the resultsRoot folder. The JDK logging
     * output is mirrored in the packageid.log file in the logRoot folder.
     *
     * @param sourceRoot
     * @param resultsRoot
     * @param logRootIn
     * @param customConfig
     */
    public int[] transport(String sourceRoot, String resultsRoot, String logRootIn, Map<String, ?> customConfig) {
        System.setProperty("java.awt.headless", "true");
        long timeStart = System.currentTimeMillis();
        logRoot = logRootIn;
        Handler handler = null;

        initConfig(customConfig);
        try {
            try {
                initJAXB();

                sourceFolder = new File(sourceRoot);

                if (!sourceFolder.exists()) {
                    handler = setupLogHandler(sourceFolder.getName());
                    log.log(Level.SEVERE, "Source folder doesn't exist: " + sourceFolder.getAbsolutePath());
                    throw new IllegalStateException("Source folder doesn't exist: " + sourceFolder.getAbsolutePath());
                }

                sourceFiles = sourceFolder.listFiles(new FileFilter() {
                    @Override
                    public boolean accept(File pathname) {
                        return (pathname.getName().endsWith(".zip"));
                    }
                });
                if (sourceFiles == null || sourceFiles.length == 0) {
                    handler = setupLogHandler(sourceFolder.getName());
                    log.log(Level.SEVERE, "Empty source folder: " + sourceFolder.getAbsolutePath());
                    throw new IllegalStateException("Empty source folder: " + sourceFolder.getAbsolutePath());
                }

                preprocessFiles();

                handler = setupLogHandler(packageid);

                resultsFolder = checkDirectory(resultsRoot);

                log.info("Loaded source folder: " + sourceFolder);
                log.info("Results directory: " + resultsFolder);

            } catch (Exception e) {
                log.log(Level.SEVERE, "Error in transporter initialization.", e);
                throw new IllegalStateException("Error in transporter initialization.", e);
            }

            if (!config.getBoolean("desa.rest")) {
                desaFolder = checkDirectory(config.getString("desa.folder"));
            }
            initializeDesa();

            try {
                for (int i = 0; i < sourceFiles.length; i++) {
                    if (i != filesIndex && i != mtdpspIndex) {
                        uploadFile(sourceFiles[i], SipType.RECORD, true);
                    }
                }
                if (mtdpspIndex > -1) {
                    uploadFile(sourceFiles[mtdpspIndex], SipType.RECORD, true);
                }

                if (filesIndex >= 0) {
                    uploadFile(sourceFiles[filesIndex], SipType.FILE, true);
                }

            } catch (Throwable th) {
                log.log(Level.SEVERE, "Error in file upload: ", th);
                throw new IllegalStateException("Error in file upload: ", th);
            } finally {
                results.setPspResultCode("progress");

                try {
                    marshaller.marshal(results, new File(resultsFolder, "TRANSF_" + packageid + ".xml"));
                } catch (JAXBException e) {
                    log.log(Level.SEVERE, "Error writing results file: ", e);
                    throw new IllegalStateException(e);
                }
            }
            long timeFinish = System.currentTimeMillis();
            log.info("Elapsed time: " + ((timeFinish - timeStart) / 1000.0) + " seconds. " + sourceFiles.length + " SIP packages transported.");
            log.info("RESULT: OK");
        } finally {
            if (handler != null) {
                removeLogHandler(handler);
            }
        }

        return countSip();
    }

    private Handler setupLogHandler(String fileName) {
        Handler handler;
        try {
            checkDirectory(logRoot);
            handler = new FileHandler(logRoot + System.getProperty("file.separator") + fileName + ".txt", 0, 1, true);
            handler.setFormatter(new SimpleFormatter());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        Logger.getLogger("").addHandler(handler);

        return handler;
    }

    private static void removeLogHandler(Handler handler) {
        Logger.getLogger("").removeHandler(handler);
        handler.close();
    }

    public void adminUpload(String sourceRoot) {
        adminUpload(sourceRoot, null);
    }

    public void adminUpload(String sourceRoot, Map<String, ?> customConfig) {
        System.setProperty("java.awt.headless", "true");
        long timeStart = System.currentTimeMillis();

        initConfig(customConfig);

        try {
            initJAXB();

            sourceFolder = new File(sourceRoot);

            if (!sourceFolder.exists()) {
                log.log(Level.SEVERE, "Source folder doesn't exist: " + sourceFolder.getAbsolutePath());
                throw new IllegalStateException("Source folder doesn't exist: " + sourceFolder.getAbsolutePath());
            }

            sourceFiles = sourceFolder.listFiles(new FileFilter() {
                @Override
                public boolean accept(File pathname) {
                    return (pathname.getName().endsWith(".zip"));
                }
            });
            if (sourceFiles == null || sourceFiles.length == 0) {
                log.log(Level.SEVERE, "Empty source folder: " + sourceFolder.getAbsolutePath());
                throw new IllegalStateException("Empty source folder: " + sourceFolder.getAbsolutePath());
            }

            log.info("Loaded source folder: " + sourceFolder);

        } catch (Exception e) {
            log.log(Level.SEVERE, "Error in transporter initialization.", e);
            throw new IllegalStateException("Error in transporter initialization.", e);
        }

        if (!config.getBoolean("desa.rest")) {
            desaFolder = checkDirectory(config.getString("desa.folder"));
        }
        initializeDesa();

        try {
            for (int i = 0; i < sourceFiles.length; i++) {
                uploadFile(sourceFiles[i], null, false);
            }

        } catch (Throwable th) {
            log.log(Level.SEVERE, "Error in file upload: ", th);
            throw new IllegalStateException("Error in file upload: ", th);
        }
        long timeFinish = System.currentTimeMillis();
        log.info("Elapsed time: " + ((timeFinish - timeStart) / 1000.0) + " seconds. " + sourceFiles.length + " SIP packages transported.");
        log.info("RESULT: OK");

    }

    /**
     * Gets nomenclatures from the remote registry.
     * @param customConfig transporter configuration
     * @param nomenclatureAcronyms query acronyms
     * @return nomenclatures
     */
    public Nomenclatures getNomenclatures(Map<String, ?> customConfig, List<String> nomenclatureAcronyms) {
        try {
            Source src = getNomenclaturesSource(customConfig, nomenclatureAcronyms);
            initJAXBNomen();
            return unmarshallerNomen.unmarshal(src, Nomenclatures.class).getValue();
        } catch (JAXBException ex) {
            throw new IllegalStateException("Unable to unmarshall nomenclatures", ex);
        }
    }

    /**
     * Returns the nomenclatures - the list of wanted nomenclatures is in
     * nomenclaturesList
     *
     * @param customConfig
     * @param nomenclatureAcronyms
     * @return
     */
    public Source getNomenclaturesSource(Map<String, ?> customConfig, List<String> nomenclatureAcronyms) {
        System.setProperty("java.awt.headless", "true");
        initConfig(customConfig);
        GregorianCalendar c = new GregorianCalendar();
        c.setTime(new Date());
        XMLGregorianCalendar currentDate;
        try {
            currentDate = DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
        } catch (DatatypeConfigurationException e) {
            throw new RuntimeException(e);
        }

        initializeDesa();
        NomenclatureListType nsType = new NomenclatureListType();
        try {
            for (String nomenclature : nomenclatureAcronyms) {
                nsType.getNomenclatureAcronyme().add(nomenclature);
            }
            byte[] result = desaPort.getNomenclatures(null, config.getString("desa.producer"), config.getString("desa.user"), nsType, currentDate);
            ByteArrayInputStream bis = new ByteArrayInputStream(result);
            return new StreamSource(bis);
        } catch (SIPSubmissionFault e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * Initialize JAXB transformers for Nomenclatures
     *
     * @throws JAXBException
     */
    private void initJAXBNomen() throws JAXBException {
        if (unmarshallerNomen != null) {
            return ;
        }
        JAXBContext jaxbContext = JAXBContext.newInstance(Nomenclatures.class);
        unmarshallerNomen = jaxbContext.createUnmarshaller();
    }

    /**
     * Initialize JAXB transformers
     *
     * @throws JAXBException
     */
    private void initJAXB() throws JAXBException {
        if (marshaller != null) {
            return ;
        }
        JAXBContext jaxbContext = JAXBContext.newInstance(PSPSIP.class);
        marshaller = jaxbContext.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        try {
            marshaller.setProperty("com.sun.xml.internal.bind.namespacePrefixMapper", new NamespacePrefixMapperInternalImpl());
        } catch (PropertyException ex) {
            marshaller.setProperty("com.sun.xml.bind.namespacePrefixMapper", new NamespacePrefixMapperImpl());
        }
        unmarshaller = jaxbContext.createUnmarshaller();
    }

    /**
     * Upload one SIP file from the input folder. First call the DESA API method
     * asyncSubmitPackageStart, then copy the zip file to the mapped DESA target
     * folder and finally call the DESA API method asyncSubmitPackageEnd. Add
     * the corresponding entry into the results file JAXB representation.
     *
     * @param file
     * @param sipType
     * @param writeResults
     */
    private void uploadFile(File file, SipType sipType, boolean writeResults) {
        Holder<String> sipId = new Holder<String>(file.getName().replace(".zip", ""));
        // Holder<String> sipId = new Holder<String>(getSipId(file));
        Holder<String> idSipVersion = new Holder<String>();
        String checksum = getMD5Checksum(file);
        log.info("Transporting file: " + file.getName());

        if (config.getBoolean("desa.rest")) {
            try {
                String operator;
                if (config.hasPath("desa.operator")) {
                    operator = config.getString("desa.operator");
                    if (operator == null) {
                        operator = config.getString("desa.user");
                    }
                } else {
                    operator = config.getString("desa.user");
                }

                URLConnection connection = new URL(config.getString("desa.restapi") + "/submitpackage" + "?userName=" + operator + "&producerCode=" + config.getString("desa.producer") + "&producerSipId=" + sipId.value + "&fileHashAlg=MD5&fileHash=" + checksum).openConnection();
                connection.setDoOutput(true);
                connection.setRequestProperty("Content-Type", "application/octet-stream");
                connection.setRequestProperty("Accept-Language", "cs");
                OutputStream output = connection.getOutputStream();
                InputStream input = new FileInputStream(file);
                try {
                    ByteStreams.copy(input, output);
                } finally {
                    if (output != null)
                        try {
                            output.flush();
                            output.close();
                        } catch (IOException logOrIgnore) {
                        }
                    if (input != null)
                        try {
                            input.close();
                        } catch (IOException logOrIgnore) {
                        }
                }
                idSipVersion.value = connection.getHeaderField("X-DEA-AipVersionId");
                log.info("Received idSipVersion:" + idSipVersion.value);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            if (idSipVersion.value == null || "".equals(idSipVersion.value)) {
                throw new RuntimeException("DESA REST call did not return idSipVersion for file " + file.getName());
            }

        } else {
            try {
                desaPort.asyncSubmitPackageStart(0, config.getString("desa.producer"), config.getString("desa.user"), sipId, (int) file.length(), checksum, FileHashAlg.MD_5, idSipVersion);
            } catch (SIPSubmissionFault sipSubmissionFault) {
                throw new RuntimeException(sipSubmissionFault);
            }
            if (idSipVersion.value == null || "".equals(idSipVersion.value)) {
                throw new RuntimeException("DESA SOAP call did not return idSipVersion for file " + file.getName());
            }
            File target = new File(desaFolder, idSipVersion.value + ".sip");
            try {
                Files.copy(file, target);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            log.info("Received idSipVersion:" + idSipVersion.value);
            try {
                desaPort.asyncSubmitPackageEnd(0, config.getString("desa.producer"), config.getString("desa.user"), idSipVersion.value);
            } catch (SIPSubmissionFault sipSubmissionFault) {
                throw new RuntimeException(sipSubmissionFault);
            }
        }
        if (writeResults) {
            GregorianCalendar c = new GregorianCalendar();
            c.setTime(new Date());
            XMLGregorianCalendar currentDate;
            try {
                currentDate = DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
            } catch (DatatypeConfigurationException e) {
                throw new RuntimeException(e);
            }
            PSPSIP.SIP entry = resultsFactory.createPSPSIPSIP();
            entry.setIdentifier(sipId.value);
            entry.setIdSIPVersion(idSipVersion.value);
            entry.setResultTime(currentDate);
            entry.setResultCode(ResultType.PROGRESS);
            entry.setType(sipType);
            results.getSIP().add(entry);
        }
    }

    private String getSipId(File sipFile) {
        String retval = null;
        try {
            ZipFile file = new ZipFile(sipFile);
            try {
                final Enumeration<? extends ZipEntry> entries = file.entries();
                while (entries.hasMoreElements()) {
                    final ZipEntry entry = entries.nextElement();
                    if ("mets.xml".equalsIgnoreCase(entry.getName())) {
                        retval = parseMetsForSipId(file.getInputStream(entry));
                    }
                }
            } finally {
                file.close();
            }
        } catch (Exception ex) {
            throw new RuntimeException("Cannot read sipId:", ex);
        }
        if (retval == null || "".equals(retval)) {
            throw new RuntimeException("No dc:identifier found in file: " + sipFile.getName());
        }
        return retval;
    }

    private String parseMetsForSipId(InputStream input) throws Exception {
        String retval = null;
        XMLInputFactory f = XMLInputFactory.newInstance();
        XMLStreamReader r = f.createXMLStreamReader(input);
        while (r.hasNext()) {
            r.next();
            if (r.isStartElement()) {
                if ("identifier".equalsIgnoreCase(r.getName().getLocalPart()) && "dc".equalsIgnoreCase(r.getName().getPrefix())) {
                    retval = r.getElementText();
                }

            }
        }
        return retval;
    }

    /**
     * The property key of the user configuration file
     */
    private static final String CONFIG = "transporter.conf";

    /**
     * Initialize the Typesafe Config configuration system
     * (https://github.com/typesafehub/config)
     *
     * @return
     */
    private void initConfig(Map<String, ?> customConfig) {
        Config conf = ConfigFactory.load();
        Config userConf = ConfigFactory.parseFileAnySyntax(new File(conf.getString(CONFIG)));

        if (customConfig != null) {
            Config customConf = ConfigFactory.parseMap(customConfig);
            config = customConf.withFallback(userConf.withFallback(conf));
        } else {
            config = userConf.withFallback(conf);
        }

    }

    /**
     * Check the contents of the input directory and find packageid and FILES
     * and MTDPSP SIP packages (which will be imported last)
     */
    private void preprocessFiles() {
        for (int i = 0; i < sourceFiles.length; i++) {
            if (sourceFiles[i].getName().endsWith("_FILE.zip")) {
                packageid = sourceFiles[i].getName().replace("_FILE.zip", "");
                filesIndex = i;
            } else if (sourceFiles[i].getName().endsWith("_MTDPSP.zip")) {
                mtdpspIndex = i;
            }
        }

        if (packageid == null) {
            for (int i = 0; i < sourceFiles.length; i++) {
                if (sourceFiles[i].getName().endsWith(".zip")) {
                    if (sourceFiles[i].getName().indexOf("_")>0) {
                        packageid = sourceFiles[i].getName().substring(0, sourceFiles[i].getName().indexOf("_"));
                    } else {
                        packageid = sourceFiles[i].getName().replace(".zip", "");
                    }
                    break;
                }
            }
        }

        if (packageid == null) {
            Handler handler = setupLogHandler(sourceFolder.getName());
            log.log(Level.SEVERE, "Invalid source folder contents. Missing FILE.zip.");
            removeLogHandler(handler);
            throw new IllegalStateException("Invalid source folder contents. Missing FILE.zip.");
        }
        if (config.getBoolean("checkMTDPSP") && mtdpspIndex == -1) {
            Handler handler = setupLogHandler(sourceFolder.getName());
            log.log(Level.SEVERE, "Invalid source folder contents. Missing MTDPSP.zip.");
            removeLogHandler(handler);
            throw new IllegalStateException("Invalid source folder contents. Missing MTDPSP.zip.");
        }
        results.setPackageID(packageid);
    }

    /**
     * Initialize DESA WS SOAP interface
     */
    void initializeDesa() {
        Authenticator.setDefault(new Authenticator() {
            @Override
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication(config.getString("desa.user"), config.getString("desa.password").toCharArray());
            }
        });
        SIPSubmissionService desaService = new SIPSubmissionService();
        desaPort = desaService.getSIPSubmissionSOAP();
        final Map<String, Object> context = ((BindingProvider) desaPort).getRequestContext();
        context.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, config.getString("desa.webservice"));
        context.put(BindingProvider.USERNAME_PROPERTY, config.getString("desa.user"));
        context.put(BindingProvider.PASSWORD_PROPERTY, config.getString("desa.password"));
    }

    /**
     * JAXB marshaller compatibility class
     */
    private static class NamespacePrefixMapperInternalImpl extends com.sun.xml.internal.bind.marshaller.NamespacePrefixMapper {

        @Override
        public String getPreferredPrefix(String namespaceUri, String suggestion, boolean requirePrefix) {
            if ("info:fedora/fedora-system:def/foxml#".equals(namespaceUri)) {
                return "foxml";
            }
            if ("http://www.loc.gov/mods/v3".equals(namespaceUri)) {
                return "mods";
            }
            if ("http://purl.org/dc/elements/1.1/".equals(namespaceUri)) {
                return "dc";
            }
            if ("http://www.openarchives.org/OAI/2.0/oai_dc/".equals(namespaceUri)) {
                return "oai_dc";
            }
            if ("info:fedora/fedora-system:def/model#".equals(namespaceUri)) {
                return "fedora-model";
            }
            if ("http://www.w3.org/1999/02/22-rdf-syntax-ns#".equals(namespaceUri)) {
                return "rdf";
            }
            if ("http://www.nsdl.org/ontologies/relationships#".equals(namespaceUri)) {
                return "kramerius";
            }
            if ("http://www.w3.org/1999/xlink".equals(namespaceUri)) {
                return "xlink";
            }
            if ("http://www.loc.gov/METS/".equals(namespaceUri)) {
                return "mets";
            }
            return suggestion;
        }

    }

    /**
     * JAXB marshaller compatibility class
     */
    public static class NamespacePrefixMapperImpl extends com.sun.xml.bind.marshaller.NamespacePrefixMapper {

        @Override
        public String getPreferredPrefix(String namespaceUri, String suggestion, boolean requirePrefix) {
            if ("info:fedora/fedora-system:def/foxml#".equals(namespaceUri)) {
                return "foxml";
            }
            if ("http://www.loc.gov/mods/v3".equals(namespaceUri)) {
                return "mods";
            }
            if ("http://purl.org/dc/elements/1.1/".equals(namespaceUri)) {
                return "dc";
            }
            if ("http://www.openarchives.org/OAI/2.0/oai_dc/".equals(namespaceUri)) {
                return "oai_dc";
            }
            if ("info:fedora/fedora-system:def/model#".equals(namespaceUri)) {
                return "fedora-model";
            }
            if ("http://www.w3.org/1999/02/22-rdf-syntax-ns#".equals(namespaceUri)) {
                return "rdf";
            }
            if ("http://www.nsdl.org/ontologies/relationships#".equals(namespaceUri)) {
                return "kramerius";
            }
            if ("http://www.w3.org/1999/xlink".equals(namespaceUri)) {
                return "xlink";
            }
            if ("http://www.loc.gov/METS/".equals(namespaceUri)) {
                return "mets";
            }
            return suggestion;
        }

    }

    /**
     * Main execution method for the checkStatus mode. The status of the import
     * of each SIP in the results XML file named TRANSF_packageid.xml in the
     * resultsRoot folder is checked by call to the DESA API getPackageStatus
     * method. The results file is then updated accordingly. The JDK logging
     * output is mirrored in the packageid.log file in the logRoot folder.
     *
     * @param resultsFileStr
     * @param logRootIn
     */
    public int[] checkStatus(String resultsFileStr, String logRootIn) {
        return checkStatus(resultsFileStr, logRootIn, null);
    }

    /**
     * Main execution method for the checkStatus mode. The status of the import
     * of each SIP in the results XML file named TRANSF_packageid.xml in the
     * resultsRoot folder is checked by call to the DESA API getPackageStatus
     * method. The results file is then updated accordingly. The JDK logging
     * output is mirrored in the packageid.log file in the logRoot folder.
     *
     * @param resultsFileStr
     * @param logRootIn
     * @param customConfig
     */
    public int[] checkStatus(String resultsFileStr, String logRootIn, Map<String, ?> customConfig) {
        System.setProperty("java.awt.headless", "true");
        long timeStart = System.currentTimeMillis();
        logRoot = logRootIn;
        Handler handler = null;

        initConfig(customConfig);

        try {
            File resultsFile = new File(resultsFileStr);
            if (!resultsFile.exists()) {
                handler = setupLogHandler(resultsFile.getName());
                log.log(Level.SEVERE, "Results file does not exist.");
                throw new IllegalStateException("Results file does not exist.");
            }

            try {
                initJAXB();
                resultsFolder = resultsFile.getParentFile();

                log.info("Loaded results file: " + resultsFile);

                parseResultsFile(resultsFile);

            } catch (Exception e) {
                handler = setupLogHandler(resultsFile.getName());
                log.log(Level.SEVERE, "Error in transporter initialization.", e);
                throw new IllegalStateException("Error in transporter initialization.", e);
            }

            handler = setupLogHandler(packageid);

            int objectCounter = 0;

            try {
                initializeDesa();
                boolean finishedAll = true;
                for (PSPSIP.SIP sip : results.getSIP()) {
                    if (!checkSIP(sip)) {
                        finishedAll = false;
                    }
                    ;
                    objectCounter++;
                }

                if (finishedAll) {
                    results.setPspResultCode("finished");
                } else {
                    results.setPspResultCode("progress");
                }

                marshaller.marshal(results, resultsFile);
            } catch (Exception e) {
                log.log(Level.SEVERE, "Error checking results: ", e);
                throw new IllegalStateException("Error checking results: ", e);
            }

            long timeFinish = System.currentTimeMillis();
            log.info("Elapsed time: " + ((timeFinish - timeStart) / 1000.0) + " seconds. " + objectCounter + " SIP packages checked.");
            log.info("RESULT: OK");

        } finally {
            if (handler != null) {
                removeLogHandler(handler);
            }
        }

        return countSip();
    }

    private int[] countSip() {
        List<PSPSIP.SIP> sipList = results.getSIP();
        int sipCount = sipList.size();
        int sipFinishedCount = 0;
        for (PSPSIP.SIP sip : sipList) {
            if (ResultType.FINISHED.equals(sip.getResultCode())) {
                sipFinishedCount++;
            }
        }

        return new int[] { sipCount, sipFinishedCount };
    }

    /**
     * Convert the existing results file to JAXB representation
     *
     * @param resultsFile
     */
    private void parseResultsFile(File resultsFile) {
        try {
            XMLReader reader = XMLReaderFactory.createXMLReader();
            reader.setEntityResolver(new EntityResolver() {
                @Override
                public InputSource resolveEntity(String publicId, String systemId) throws SAXException, IOException {
                    return new InputSource(new ByteArrayInputStream("<?xml version='1.0' encoding='UTF-8'?>".getBytes()));
                }
            });
            SAXSource saxSource = new SAXSource(reader, new InputSource(new FileInputStream(resultsFile)));
            results = (PSPSIP) unmarshaller.unmarshal(saxSource);
            packageid = results.getPackageID();
        } catch (Exception e) {
            Handler handler = setupLogHandler(resultsFile.getName());
            log.log(Level.SEVERE, "Error in parsing results file.", e);
            removeLogHandler(handler);
            throw new IllegalStateException("Error in parsing results file.", e);
        }
    }

    /**
     * Check the status of the SIP of one entry in the results file and update
     * the entry in the JAXB object.
     *
     * @param sip
     * @return
     */
    private boolean checkSIP(PSPSIP.SIP sip) {
        Holder<String> sipId = new Holder<String>(sip.getIdentifier());
        Holder<String> idSipVersion = new Holder<String>(sip.getIdSIPVersion());
        Holder<String> packageStateCode = new Holder<String>();
        Holder<String> packageStateText = new Holder<String>();
        Holder<String> errorCode = new Holder<String>();
        log.info("Checking file: " + sipId.value);
        /*
         * if(config.getBoolean("desa.rest")){ try { URLConnection connection =
         * new
         * URL(config.getString("desa.restapi")+"/packagestatus"+"?userName="
         * +config.getString("desa.user")
         * +"&producerCode="+config.getString("desa.producer"
         * )+"&producerSipId="+
         * sipId.value+"&idSIPVersion="+idSipVersion.value).openConnection();
         * packageStateCode
         * .value=connection.getHeaderField("X-DEA-packageStateCode");
         * packageStateText
         * .value=connection.getHeaderField("X-DEA-packageStateText");
         * errorCode.value=connection.getHeaderField("X-DEA-errorCode"); } catch
         * (Exception e) { throw new RuntimeException(e); } }else{
         */
        try {
            desaPort.getPackageStatus(0, config.getString("desa.producer"), config.getString("desa.user"), idSipVersion, sipId, packageStateCode, packageStateText, errorCode);
        } catch (SIPSubmissionFault sipSubmissionFault) {
            throw new RuntimeException(sipSubmissionFault);
        } catch (Exception e1) {
            log.log(Level.SEVERE, "DESA exception", e1);
            throw new IllegalStateException("DESA exception", e1);
        }
        /* } */
        log.info("Status: " + packageStateCode.value + " (" + packageStateText.value + ")" + (errorCode.value != null ? (", errorCode:" + errorCode.value) : ""));
        GregorianCalendar c = new GregorianCalendar();
        c.setTime(new Date());
        XMLGregorianCalendar currentDate;
        try {
            currentDate = DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
        } catch (DatatypeConfigurationException e) {
            throw new RuntimeException(e);
        }
        boolean retval = false;
        sip.setResultTime(currentDate);
        if ("AI_ACC_OK".equalsIgnoreCase(packageStateCode.value)) {
            sip.setResultCode(ResultType.FINISHED);
            retval = true;
        } else if ("AI_ERROR".equalsIgnoreCase(packageStateCode.value) || "AI_INVALID".equalsIgnoreCase(packageStateCode.value) || "AI_REJECT".equalsIgnoreCase(packageStateCode.value) || "AI_INFECTED".equalsIgnoreCase(packageStateCode.value) || "AI_QA_ERR".equalsIgnoreCase(packageStateCode.value)) {
            sip.setResultCode(ResultType.ERROR);
        } else {
            sip.setResultCode(ResultType.PROGRESS);
        }
        return retval;
    }

    /**
     * Check if the directory with the given path exists and create it if
     * necessary
     *
     * @param name
     *            The path of the requested directory
     * @return The File representation of the requested directory
     */
    private static File checkDirectory(String name) {
        File directory = new File(name);
        if (!directory.exists() || !directory.isDirectory()) {
            try {
                Files.createParentDirs(directory);
            } catch (IOException e) {
                throw new RuntimeException("Folder doesn't exist and can't be created: " + directory.getAbsolutePath());
            }
            if (!directory.mkdir()) {
                // log.severe("Folder doesn't exist and can't be created: " +
                // directory.getAbsolutePath());
                throw new RuntimeException("Folder doesn't exist and can't be created: " + directory.getAbsolutePath());
            }
        }
        return directory;
    }

    /**
     * Calculate the MD5 checksum of the given file
     *
     * @param file
     * @return
     */
    private static String getMD5Checksum(File file) {
        try {
            InputStream fis = new FileInputStream(file);

            byte[] buffer = new byte[1024];
            MessageDigest complete = MessageDigest.getInstance("MD5");
            int numRead;

            do {
                numRead = fis.read(buffer);
                if (numRead > 0) {
                    complete.update(buffer, 0, numRead);
                }
            } while (numRead != -1);

            fis.close();
            byte[] bytes = complete.digest();
            StringBuilder sb = new StringBuilder(2 * bytes.length);
            for (byte b : bytes) {
                sb.append(hexDigits[(b >> 4) & 0xf]).append(hexDigits[b & 0xf]);
            }
            return sb.toString();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

    }

    private static final char[] hexDigits = "0123456789abcdef".toCharArray();

}
