/*
 * Copyright (C) 2024 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.external;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.response.FedoraResponse;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.export.DataStreamExport;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraUtils;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import jakarta.xml.bind.JAXBException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.TransformerException;
import org.apache.commons.configuration2.Configuration;

public class PdfaProcess {

    private AppConfiguration config;
    private AkubraConfiguration akubraConfiguration;
    private SearchView search;

    private static final Logger LOG = Logger.getLogger(DataStreamExport.class.getName());

    public PdfaProcess(AppConfiguration config, AkubraConfiguration akubraConfiguration) throws IOException {
        this.config = config;
        this.akubraConfiguration = akubraConfiguration;

        if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
            this.search = FedoraStorage.getInstance(config).getSearch();
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            this.search = AkubraStorage.getInstance(akubraConfiguration).getSearch();
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
        }
    }

    public Result generatePdfA(List<String> pids, File folder) {
        Result result = new Result();
        try {
            if ((pids == null || pids.isEmpty()) && (folder == null || !folder.exists())) {
                throw new IllegalArgumentException("Missing required parameters");
            } else if (pids != null && !pids.isEmpty()) {
                generateSimplePdfA(pids);
            }
        } catch (Exception ex) {
            if (ex.getMessage().contains("Missing 'exec'!")) {
                result.setException(new IOException("Konvertor PdfA není nakonfigurován"));
            } else {
                result.setException(ex);
            }
        } finally {
            return result;
        }
    }

    private void generateSimplePdfA(List<String> pids) throws JAXBException, IOException, TransformerException, FedoraClientException, InterruptedException, DigitalObjectException {
        for (String pid : pids) {
            ProArcObject object = null;
            InputStream inputStream = null;
            String dsId = "RAW";
            if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
                FedoraStorage fedoraStorage = FedoraStorage.getInstance(config);
                object = fedoraStorage.find(pid);
                FedoraResponse response = FedoraClient.getDatastreamDissemination(object.getPid(), dsId)
                        .execute(((FedoraStorage.RemoteObject) object).getClient());
                inputStream = response.getEntityInputStream();
            } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
                inputStream = AkubraUtils.getDatastreamDissemination((AkubraStorage.AkubraObject) object, dsId);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
            }
            if (inputStream == null) {
                throw new IllegalStateException("Nepodařilo se načíst RAW stream pro PID" + pid);
            }

            File pidFolder = prepareFolder(config.getConfigHome(), object.getPid());
            try {
                Thread.sleep(5000);
                File pdfFile = createFile(pidFolder, inputStream);

                if (pdfFile == null) {
                    throw new IllegalStateException("Nepodařilo se vytvořit dočasný soubor pro generování pdfa souboru (pid:" + object.getPid() + ").");
                }
                File generatedPdfFile = new File(pidFolder, createFileName(object.getPid() + "-new", "pdf"));

                PdfAGenerator generator = new PdfAGenerator(config.getPdfAGeneratorProcessor(), pdfFile, generatedPdfFile);
                generator.run();


                if (!(generatedPdfFile.exists())) {
                    throw new IOException("Impossible to generate file with convertor");
                }

                BinaryEditor sourceEditor = BinaryEditor.dissemination(object, BinaryEditor.SOURCE_ID, BinaryEditor.FILE_PDF);
                sourceEditor.write(pdfFile, sourceEditor == null ? 0 : sourceEditor.getLastModified(), null);

                // import pdfa
                BinaryEditor binaryEditor = BinaryEditor.dissemination(object, BinaryEditor.RAW_ID, BinaryEditor.FILE_PDF);
                binaryEditor.write(generatedPdfFile, binaryEditor == null ? 0 : binaryEditor.getLastModified(), null);

                String pdfValidationStatus = "";
                if (generator.isOk() || generator.getFullOutput().contains("PASS")) {
                    pdfValidationStatus = "OK";
                } else {
                    pdfValidationStatus = "FAILED";
                }

                RelationEditor relationEditor = new RelationEditor(object);
                relationEditor.setPdfValidationStatus(pdfValidationStatus);
                relationEditor.write(relationEditor.getLastModified(), "Nahrani stavu zvalidovaneho pdfA");

                object.flush();
                Thread.sleep(5000);
            } finally {
                MetsUtils.deleteFolder(pidFolder);
            }
        }
    }

    private File createFile(File pidFolder, InputStream inputStream) throws IOException, InterruptedException {
        File pdfFile = new File(pidFolder, pidFolder.getName() + ".pdf");
        boolean done = false;
        try {
            FileOutputStream outputStream = new FileOutputStream(pdfFile);
            try {
                MetsUtils.copyStream(inputStream, outputStream);
                Thread.sleep(5000);
                done = true;
            } finally {
                try {
                    outputStream.close();
                } catch (IOException ex) {
                    if (done) {
                        throw ex;
                    } else {
                        LOG.log(Level.SEVERE, pdfFile.toString(), ex);
                        return null;
                    }
                }
            }
        } finally {
            try {
                inputStream.close();
            } catch (IOException ex) {
                if (done) {
                    throw ex;
                } else {
                    LOG.log(Level.SEVERE, pdfFile.toString(), ex);
                    return null;
                }
            }
        }
        return pdfFile.exists() ? pdfFile : null;
    }

    private String createFileName(String pid, String dsId) {
        return FoxmlUtils.pidAsUuid(pid) + '.' + dsId;
    }

    private File prepareFolder(File appHome, String pid) throws IOException {
        if (appHome == null || !appHome.exists()) {
            throw new IOException(String.format("App home does not exists: %s", appHome.getAbsolutePath()));
        } else {
            File tmpFile = new File(appHome, "temp");
            if (!tmpFile.exists()) {
                tmpFile.mkdir();
            }
            if (tmpFile == null || !tmpFile.exists()) {
                throw new IOException(String.format("File does not exists: %s", tmpFile.getAbsolutePath()));
            } else {
                File pdfAFile = new File(tmpFile, "pdfA");
                if (!pdfAFile.exists()) {
                    pdfAFile.mkdir();
                }
                if (pdfAFile == null || !pdfAFile.exists()) {
                    throw new IOException(String.format("File does not exists: %s", tmpFile.getAbsolutePath()));
                } else {
                    File pidFolder = new File(pdfAFile, FoxmlUtils.pidAsUuid(pid));
                    if (pidFolder.exists()) {
                        MetsUtils.deleteFolder(pidFolder);
                    }
                    pidFolder.mkdir();
                    return pidFolder;
                }
            }
        }
    }

    public class PdfAGenerator extends ExternalProcess {

        public static final String ID = "pdfa_generate";
        private final File input;
        private final File output;

        public PdfAGenerator(Configuration conf, File input, File output) {
            super(conf);
            this.input = input;
            this.output = output;
        }

        @Override
        protected List<String> buildCmdLine(Configuration conf) {
            String inputFile = input.getAbsolutePath();
            String outputFile = output.getAbsolutePath();
            List<String> cmdLine = super.buildCmdLine(conf);
            cmdLine.add(inputFile);
            cmdLine.add(outputFile);
            return cmdLine;
        }
    }

    public static class Result {
        private File file;
        private Exception ex;
        private String message;

        public File getFile() {
            return file;
        }

        public void setFile(File file) {
            this.file = file;
        }

        public Exception getException() {
            return ex;
        }

        public void setException(Exception ex) {
            this.ex = ex;
        }

        public String getMessage() {
            return message;
        }

        public void setMessage(String message) {
            this.message = message;
        }
    }


}
