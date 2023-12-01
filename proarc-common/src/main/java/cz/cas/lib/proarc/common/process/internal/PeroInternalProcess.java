package cz.cas.lib.proarc.common.process.internal;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.response.FedoraResponse;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.StringEditor;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraUtils;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.process.export.DataStreamExport;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.external.ExternalProcess;
import cz.cas.lib.proarc.common.process.external.OcrGenerator;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.JAXBException;
import javax.xml.transform.TransformerException;

public class PeroInternalProcess {

    private AppConfiguration config;
    private AkubraConfiguration akubraConfiguration;
    private SearchView search;

    private static final Logger LOG = Logger.getLogger(DataStreamExport.class.getName());

    public PeroInternalProcess(AppConfiguration config, AkubraConfiguration akubraConfiguration) throws IOException {
        this.config = config;
        this.akubraConfiguration = akubraConfiguration;

        if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
            this.search = RemoteStorage.getInstance(config).getSearch();
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            this.search = AkubraStorage.getInstance(akubraConfiguration).getSearch();
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
        }
    }

    public Result generateAlto(List<String> pids, File importFolder) {
        Result result = new Result();
        try {
            if ((pids == null || pids.isEmpty()) && (importFolder == null || !importFolder.exists())) {
                throw new IllegalArgumentException("Missing required parameters");
            } else if (pids != null && !pids.isEmpty()) {
                generateSingleAlto(pids);
            } else {
                throw new IllegalStateException("Method is not yet implemented.");
            }
        } catch (Exception ex) {
            result.setException(ex);
        } finally {
            return result;
        }
    }

    private void generateSingleAlto(List<String> pids) throws JAXBException, IOException, TransformerException, FedoraClientException, InterruptedException, DigitalObjectException {
        for (String pid : pids) {
            FedoraObject object = null;
            InputStream inputStream = null;
            String dsId = "FULL";
            if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
                RemoteStorage remoteStorage = RemoteStorage.getInstance(config);
                object = remoteStorage.find(pid);
                FedoraResponse response = FedoraClient.getDatastreamDissemination(object.getPid(), dsId)
                        .execute(((RemoteStorage.RemoteObject) object).getClient());
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
                generateOCR(jpgFile);
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

    private void generateOCR(File jpgFile) throws IOException {

        ExternalProcess process = new OcrGenerator(config.getImportConfiguration().getOcrGenProcessor(), jpgFile, ".txt", ".xml");

        if (process != null) {
            process.run();

            if (!process.isOk()) {
                throw new IOException("Generating OCR for " + jpgFile.getName() + " failed. \n " + process.getFullOutput());
            }
        }
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
