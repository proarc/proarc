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

import static cz.cas.lib.proarc.common.process.imports.ImportProfile.THUMBNAIL_PROCESSOR;

public class ThumbnailPdfProcess {

    private AppConfiguration config;
    private AkubraConfiguration akubraConfiguration;
    private SearchView search;

    private static final Logger LOG = Logger.getLogger(DataStreamExport.class.getName());

    public ThumbnailPdfProcess(AppConfiguration config, AkubraConfiguration akubraConfiguration) throws IOException {
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

    public Result generateThumbnail(List<String> pids, File folder) {
        Result result = new Result();
        try {
            if ((pids == null || pids.isEmpty()) && (folder == null || !folder.exists())) {
                throw new IllegalArgumentException("Missing required parameters");
            } else if (pids != null && !pids.isEmpty()) {
                generateSimpleThumbnail(pids);
            }
        } catch (Exception ex) {
            if (ex.getMessage().contains("Missing 'exec'!")) {
                result.setException(new IOException("Konvertor thumbnail není nakonfigurován"));
            } else {
                result.setException(ex);
            }
        } finally {
            return result;
        }
    }

    private void generateSimpleThumbnail(List<String> pids) throws JAXBException, IOException, TransformerException, FedoraClientException, InterruptedException, DigitalObjectException {
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
                    throw new IllegalStateException("Nepodařilo se vytvořit dočasný soubor pro generování thumbnailu souboru (pid:" + object.getPid() + ").");
                }
                File generatedPdfFile = new File(pidFolder, createFileName(object.getPid() + "-new", "jpg"));

                Configuration thumbConfig = config.getImportConfiguration().getThumbnailProcessor();
                if (thumbConfig == null || thumbConfig.isEmpty()) {
                    throw new IllegalStateException("Missing configuration for " + THUMBNAIL_PROCESSOR);
                }

                GenericExternalProcess thumbProc = new GenericExternalProcess(thumbConfig).addInputFile(pdfFile).addOutputFile(generatedPdfFile);
                thumbProc.run();

                if (!(generatedPdfFile.exists())) {
                    throw new IOException("Impossible to generate file with convertor");
                }

                if (!thumbProc.isOk()) {
                    LOG.severe("Error pri generovani thumbnailu pro pid \"" + pid + "\".");
                    LOG.severe(thumbProc.getFullOutput());
                    throw new IllegalStateException("Impossible to generate file with convertor");
                }

                // import generated thumbnail
                BinaryEditor thumbnailEditor = BinaryEditor.dissemination(object, BinaryEditor.THUMB_ID, BinaryEditor.IMAGE_JPEG);
                thumbnailEditor.write(generatedPdfFile, thumbnailEditor == null ? 0 : thumbnailEditor.getLastModified(), null);

                object.flush();
                Thread.sleep(10000);
            } finally {
                MetsUtils.deleteFolder(pidFolder);
            }
        }
    }

    private File createFile(File pidFolder, InputStream inputStream) throws IOException, InterruptedException {
        File pdfFile = new File(pidFolder, pidFolder.getName()+ ".pdf");
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
                File pdfAFile = new File(tmpFile, "pdfThumbnail");
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
