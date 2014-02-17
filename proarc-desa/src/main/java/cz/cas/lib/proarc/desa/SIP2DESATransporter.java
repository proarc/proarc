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

package cz.cas.lib.proarc.desa;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.sax.SAXSource;
import javax.xml.ws.Holder;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import cz.cas.lib.proarc.desa.pspsip.ObjectFactory;
import cz.cas.lib.proarc.desa.pspsip.PSPSIP;
import cz.cas.lib.proarc.desa.pspsip.ResultType;
import cz.cas.lib.proarc.desa.pspsip.SipType;
import cz.cas.lib.proarc.desa.soap.FileHashAlg;
import cz.cas.lib.proarc.desa.soap.SIPSubmission;
import cz.cas.lib.proarc.desa.soap.SIPSubmissionFault;
import org.apache.commons.io.FileUtils;

/**
 * Main class for import of the SIP packages to DESA repository and for checking
 * the progress of the import.
 */
public final class SIP2DESATransporter {

    private static final Logger log = Logger.getLogger(SIP2DESATransporter.class.toString());
    private static JAXBContext PSPSIP_JAXB;

    private final SIPSubmission desaPort;
    private final DesaClient desaClient;
    private final String desaFolderPath;
    private final String operatorName;
    private final String producerCode;
    private boolean checkMTDPSP = false;
    private final boolean useRest;
    private Marshaller marshaller = null;
    private Unmarshaller unmarshaller = null;

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

    /** used just in case non REST usage! */
    private File desaFolder;

    private String logRoot = "";

//    /**
//     * The transporter entry point
//     *
//     * @param args
//     *            command line arguments for transport mode : transport
//     *            <input-folder> <results-folder> <log-folder> command line
//     *            arguments for checkStatus mode : checkStatus <results-file>
//     *            <log-folder>
//     */
//    public static void main(String[] args) {
//        try {
//            if (args.length == 4 && "transport".equalsIgnoreCase(args[0])) {
//                String importRoot = args[1];
//                String resultsRoot = args[2];
//                String logRoot = args[3];
//                new SIP2DESATransporter().transport(importRoot, resultsRoot, logRoot);
//            } else if (args.length == 3 && "checkStatus".equalsIgnoreCase(args[0])) {
//                String resultsRoot = args[1];
//                String logRoot = args[2];
//                new SIP2DESATransporter().checkStatus(resultsRoot, logRoot);
//            } else if (args.length == 2 && "adminUpload".equalsIgnoreCase(args[0])) {
//                String importRoot = args[1];
//                new SIP2DESATransporter().adminUpload(importRoot);
//            } else {
//                System.out.println("SIP to DESA transporter.\n");
//                System.out.println("Usage for transport: sip2desa.SIP2DESATransporter transport <input-folder> <results-folder> <log-folder>");
//                System.out.println("Usage for checkStatus: sip2desa.SIP2DESATransporter checkStatus <results-file> <log-folder>");
//                System.out.println("Usage for adminUpload: sip2desa.SIP2DESATransporter adminUpload <input-folder>");
//
//                System.exit(1);
//            }
//        } catch (Exception e) {
//            log.log(Level.SEVERE, e.getMessage(), e);
//        }
//    }

    /**
     * Submits SIPs through REST interface.
     */
    SIP2DESATransporter(DesaClient desaClient, String operator, String producerCode) {
        this(desaClient, true, null, operator, producerCode);
    }

    /**
     * Ignores REST interface.
     */
    SIP2DESATransporter(DesaClient desaClient, String desaFolderPath, String operator, String producerCode) {
        this(desaClient, false, desaFolderPath, operator, producerCode);
    }

    private SIP2DESATransporter(DesaClient desaClient, boolean useRest, String desaFolderPath, String operator, String producerCode) {
        this.desaPort = desaClient.getSoapClient();
        this.desaClient = desaClient;
        this.useRest = useRest;
        this.desaFolderPath = desaFolderPath;
        this.operatorName = operator;
        this.producerCode = producerCode;
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
    public int[] transport(String sourceRoot, String resultsRoot, String logRootIn) {
        long timeStart = System.currentTimeMillis();
        logRoot = logRootIn;
        Handler handler = null;
        File[] sourceFiles;
        File resultsFolder;
        try {
            try {
                initJAXB();

                File sourceFolder = new File(sourceRoot);

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

                preprocessFiles(sourceFolder, sourceFiles);

                handler = setupLogHandler(packageid);

                resultsFolder = checkDirectory(resultsRoot);

                log.info("Loaded source folder: " + sourceFolder);
                log.info("Results directory: " + resultsFolder);

            } catch (Exception e) {
                log.log(Level.SEVERE, "Error in transporter initialization.", e);
                throw new IllegalStateException("Error in transporter initialization.", e);
            }

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
        long timeStart = System.currentTimeMillis();
        File[] sourceFiles;

        try {
            initJAXB();

            File sourceFolder = new File(sourceRoot);

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
     * Gets the cached thread safe JAXB context.
     */
    private static JAXBContext getPspipJaxb() throws JAXBException {
        if (PSPSIP_JAXB == null) {
            PSPSIP_JAXB = JAXBContext.newInstance(PSPSIP.class);
        }
        return PSPSIP_JAXB;
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
        JAXBContext jaxbContext = getPspipJaxb();
        marshaller = jaxbContext.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
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
         Holder<String> sipId = new Holder<String>(getSipId(file));
        Holder<String> idSipVersion = new Holder<String>();
        String checksum = getMD5Checksum(file);
        log.info("Transporting file: " + file.getName());

        if (useRest) {
            idSipVersion.value = desaClient.submitPackage(file,
                    operatorName, producerCode, sipId.value,
                    FileHashAlg.MD_5, checksum, "cs");
            log.info("Received idSipVersion:" + idSipVersion.value);
            if (idSipVersion.value == null || "".equals(idSipVersion.value)) {
                throw new RuntimeException("DESA REST call did not return idSipVersion for file " + file.getName());
            }
        } else {
            try {
                desaPort.asyncSubmitPackageStart(0, producerCode, operatorName, sipId, (int) file.length(), checksum, FileHashAlg.MD_5, idSipVersion);
            } catch (SIPSubmissionFault sipSubmissionFault) {
                throw new RuntimeException(sipSubmissionFault);
            }
            if (idSipVersion.value == null || "".equals(idSipVersion.value)) {
                throw new RuntimeException("DESA SOAP call did not return idSipVersion for file " + file.getName());
            }
            File target = new File(getDesaFolder(), idSipVersion.value + ".sip");
            try {
                FileUtils.copyFile(file, target);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            log.info("Received idSipVersion:" + idSipVersion.value);
            try {
                desaPort.asyncSubmitPackageEnd(0, producerCode, operatorName, idSipVersion.value);
            } catch (SIPSubmissionFault sipSubmissionFault) {
                throw new RuntimeException(sipSubmissionFault);
            }
        }
        if (writeResults) {
            XMLGregorianCalendar currentDate = desaClient.getXmlTypes().newXMLGregorianCalendar(new GregorianCalendar());
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
        return sipFile.getName().replace(".zip", "");
    }

    /**
     * Check the contents of the input directory and find packageid and FILES
     * and MTDPSP SIP packages (which will be imported last)
     */
    private void preprocessFiles(File sourceFolder, File[] sourceFiles) {
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
        if (checkMTDPSP && mtdpspIndex == -1) {
            Handler handler = setupLogHandler(sourceFolder.getName());
            log.log(Level.SEVERE, "Invalid source folder contents. Missing MTDPSP.zip.");
            removeLogHandler(handler);
            throw new IllegalStateException("Invalid source folder contents. Missing MTDPSP.zip.");
        }
        results.setPackageID(packageid);
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
        long timeStart = System.currentTimeMillis();
        logRoot = logRootIn;
        Handler handler = null;

        try {
            File resultsFile = new File(resultsFileStr);
            if (!resultsFile.exists()) {
                handler = setupLogHandler(resultsFile.getName());
                log.log(Level.SEVERE, "Results file does not exist.");
                throw new IllegalStateException("Results file does not exist.");
            }

            try {
                initJAXB();

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
            desaPort.getPackageStatus(0, producerCode, operatorName, idSipVersion, sipId, packageStateCode, packageStateText, errorCode);
        } catch (SIPSubmissionFault sipSubmissionFault) {
            throw new RuntimeException(sipSubmissionFault);
        } catch (Exception e1) {
            log.log(Level.SEVERE, "DESA exception", e1);
            throw new IllegalStateException("DESA exception", e1);
        }
        /* } */
        log.info("Status: " + packageStateCode.value + " (" + packageStateText.value + ")" + (errorCode.value != null ? (", errorCode:" + errorCode.value) : ""));
        XMLGregorianCalendar currentDate = desaClient.getXmlTypes().newXMLGregorianCalendar(new GregorianCalendar());;
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

    /** Gets target folder for SOAP submit package. */
    private File getDesaFolder() {
        if (!useRest) {
            if (desaFolder == null) {
                desaFolder = checkDirectory(desaFolderPath);
            }
        }
        return desaFolder;
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
        try {
            FileUtils.forceMkdir(directory);
            return directory;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
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
