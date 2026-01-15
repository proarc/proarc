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
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.process.export.DataStreamExport;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraUtils;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import jakarta.ws.rs.core.MediaType;
import jakarta.xml.bind.JAXBException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.TransformerException;
import org.json.JSONException;

public class PeroProcess {

    private AppConfiguration config;
    private AkubraConfiguration akubraConfiguration;
    private SearchView search;

    private static final Logger LOG = Logger.getLogger(DataStreamExport.class.getName());

    public PeroProcess(AppConfiguration config, AkubraConfiguration akubraConfiguration) throws IOException {
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

    public Result generateAlto(List<String> pids, File folder, Integer peroOcrEngine) {
        Result result = new Result();
        try {
            if ((pids == null || pids.isEmpty()) && (folder == null || !folder.exists())) {
                throw new IllegalArgumentException("Missing required parameters");
            } else if (folder != null && folder.exists()) {
                generateMultipleAlto(folder, peroOcrEngine);
            } else if (pids != null && !pids.isEmpty()) {
                generateSingleAlto(pids, peroOcrEngine);
            }
        } catch (Exception ex) {
            result.setException(ex);
        } finally {
            return result;
        }
    }

    private void generateMultipleAlto(File folder, Integer peroOcrEngine) throws IOException {
        if (folder == null || !folder.exists() || !folder.canRead() || !folder.canWrite()) {
            throw new IOException("It is not possiblke to access " + (folder == null ? null : folder.getAbsolutePath()));
        }
        for (File file : folder.listFiles()) {
            if (file.getName().endsWith("tiff") || file.getName().endsWith("tif") || file.getName().endsWith("jpg")) {
                File imgFile = file;
                generateOCR(imgFile, peroOcrEngine);
            }
        }
    }

    private void generateSingleAlto(List<String> pids, Integer peroOcrEngine) throws JAXBException, IOException, TransformerException, FedoraClientException, InterruptedException, DigitalObjectException {
        for (String pid : pids) {
            ProArcObject object = null;
            InputStream inputStream = null;
            String dsId = "FULL";
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
            File pidFolder = prepareFolder(config.getConfigHome(), object.getPid(), "jpg");
            Thread.sleep(5000);
            try {
                File jpgFile = new File(pidFolder, createFileName(object.getPid(), "jpg"));
                boolean done = false;
                try {
                    FileOutputStream outputStream = new FileOutputStream(jpgFile);
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
                                LOG.log(Level.SEVERE, jpgFile.toString(), ex);
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
                            LOG.log(Level.SEVERE, jpgFile.toString(), ex);
                        }
                    }
                }
                File ocrFile = new File(pidFolder, createFileName(object.getPid(), "txt"));
                File altoFile = new File(pidFolder, createFileName(object.getPid(), "xml"));
                generateOCR(jpgFile, peroOcrEngine);
                int i = 0;
                while (!altoFile.exists()) {
                    if (i > 25) {
                        break;
                    }
                    i++;
                    Thread.sleep(5000);
                }
                if (!(altoFile.exists() || ocrFile.exists())) {
                    throw new IOException("Impossible to generate file with Pero");
                }
                // import alto
                AltoDatastream.importAlto(object, altoFile.toURI(), null);

                // import ocr
                MediaType mime = MediaType.valueOf(Files.probeContentType(ocrFile.toPath()));
                BinaryEditor editor = BinaryEditor.dissemination(object, StringEditor.OCR_ID, mime);
                if (editor == null) {
                    editor = new BinaryEditor(object, FoxmlUtils.managedProfile(StringEditor.OCR_ID, mime, StringEditor.OCR_LABEL));
                }
                editor.write(ocrFile, editor.getLastModified(), null);

                object.flush();
            } finally {
                MetsUtils.deleteFolder(pidFolder);
            }
        }
    }

    private void generateOCR(File jpgFile, Integer peroOcrEngine) throws IOException {
        PeroOcrProcessor ocrProcessor = new PeroOcrProcessor(config.getImportConfiguration().getOcrGenProcessor(), peroOcrEngine);
        try {
            boolean processed = ocrProcessor.generate(jpgFile, ".txt", ".xml");
            if (processed) {
                LOG.info("OCR GENERATED SUCCESSFULLY for " + jpgFile.getAbsolutePath());
            }
        } catch (JSONException ex) {
            LOG.severe("Generating OCR for " + jpgFile.getName() + " failed.");
            ex.printStackTrace();
            throw new IOException(ex);
        }

//        ExternalProcess process = new OcrGenerator(config.getImportConfiguration().getOcrGenProcessor(), jpgFile, ".txt", ".xml");
//
//        if (process != null) {
//            process.run();
//
//            if (!process.isOk()) {
//                throw new IOException("Generating OCR for " + jpgFile.getName() + " failed. \n " + process.getFullOutput());
//            }
//        }
    }

    private String createFileName(String pid, String dsId) {
        return FoxmlUtils.pidAsUuid(pid) + '.' + dsId;
    }

    private File prepareFolder(File appHome, String pid, String dsId) throws IOException {
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
                File peroFile = new File(tmpFile, "pero");
                if (!peroFile.exists()) {
                    peroFile.mkdir();
                }
                if (peroFile == null || !peroFile.exists()) {
                    throw new IOException(String.format("File does not exists: %s", tmpFile.getAbsolutePath()));
                } else {
                    File pidFolder = new File(peroFile, FoxmlUtils.pidAsUuid(pid));
                    if (pidFolder.exists()) {
                        MetsUtils.deleteFolder(pidFolder);
                    }
                    pidFolder.mkdir();
                    return pidFolder;
                }
            }
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
