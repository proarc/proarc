package cz.cas.lib.proarc.common.process.external;

import cz.cas.lib.proarc.common.object.ValueMap;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration2.Configuration;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.json.JSONException;
import org.json.JSONObject;

/**
 *
 * @author alberto and LukasSykora
 */
public class PeroOcrProcessor {

    public static final Logger LOGGER = Logger.getLogger(PeroOcrProcessor.class.getName());

    public static final String PROP_KEY = "key";
    public static final String PROP_URL = "url";

    private final Configuration config;
    private String apiKey;
    private String serverUrl;
    private int peroOcrEngine;

    private String imagePath;
    private String txtPath;
    private String altoPath;


    public PeroOcrProcessor(Configuration configuration, Integer peroOcrEngine) {
        this.config = configuration;
        this.serverUrl = config.getString(PROP_URL);
        if (this.serverUrl == null || this.serverUrl.isEmpty()) {
            this.serverUrl = "https://pero-ocr.fit.vutbr.cz/api/";
        }
        this.apiKey = config.getString(PROP_KEY);
        this.peroOcrEngine = (peroOcrEngine == null || peroOcrEngine < 1) ? 1 : peroOcrEngine;
    }

    public boolean generate(File imageFile, String ocrFileSuffix, String altoFileSuffix) throws JSONException {
        this.imagePath = imageFile.getAbsolutePath();

        File[] outputFiles = getOcrFiles(imageFile, ocrFileSuffix, altoFileSuffix);
        this.txtPath = outputFiles[0].getAbsolutePath();
        this.altoPath = outputFiles[1].getAbsolutePath();

        return process(this.imagePath, this.txtPath, this.altoPath);
    }

    public static File[] getOcrFiles(File imageFile, String plainOcrFileSuffix, String altoFileSuffix) {
        File txt = new File(imageFile.getAbsolutePath().substring(0, imageFile.getAbsolutePath().lastIndexOf('.')) + plainOcrFileSuffix);
        File alto = new File(imageFile.getAbsolutePath().substring(0, imageFile.getAbsolutePath().lastIndexOf('.')) + altoFileSuffix);

        return new File[]{txt, alto};
    }

    public boolean process(String imagePath, String txtPath, String altoPath) throws JSONException {

        String fileExtension = imagePath.substring(imagePath.lastIndexOf('.') + 1);

        String fileName = new File(imagePath).getName().split("\\.")[0];
        String contentType = getContentType(fileExtension);

        JSONObject data = createJson(fileName);
        String txtFormat = "txt";
        String altoFormat = "alto";

        HttpClient httpClient = HttpClients.createDefault();
        String requestId = postProcessingRequest(httpClient, data);
        // LOGGER.log(Level.INFO, "requestId is {0}", requestId);
        try {
            boolean uploaded = uploadImage(httpClient, requestId, fileName, imagePath, contentType);
            if (!uploaded) {
                return false;
            }
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, null, e);
        }

        String processingResult;
        do {
            processingResult = downloadResults(httpClient, txtPath, requestId, fileName, txtFormat);
            if (processingResult.equals("PROCESSED")) {
                downloadResults(httpClient, altoPath, requestId, fileName, altoFormat);
                LOGGER.log(Level.INFO, "Files {0} and {1} created!", new Object[]{txtFormat, altoFormat});
            } else {
                try {
                    TimeUnit.SECONDS.sleep(3);
                } catch (InterruptedException e) {
                    LOGGER.log(Level.SEVERE, "Task interrupted!");
                }

                if (Thread.interrupted()) {
                    LOGGER.log(Level.INFO, "Thread stopped");
                    return false;
                }
            }
        } while (!processingResult.equals("PROCESSED"));
        return true;
    }

    private JSONObject createJson(String fileName) throws JSONException {
        JSONObject outputData = new JSONObject();
        outputData.put(fileName, JSONObject.NULL);
        JSONObject dataDict = new JSONObject();
        dataDict.put("engine", peroOcrEngine);
        dataDict.put("images", outputData);
        return dataDict;
    }

    private String postProcessingRequest(HttpClient httpClient, JSONObject data) {
        String url = this.serverUrl + "post_processing_request";
        HttpPost httpPost = new HttpPost(url);
        httpPost.addHeader("api-key", this.apiKey);
        httpPost.setHeader("Content-Type", "application/json");
        try {
            httpPost.setEntity(new StringEntity(data.toString(), "UTF-8"));
            HttpResponse response = httpClient.execute(httpPost);
            if (response.getStatusLine().getStatusCode() < 400) {
                JSONObject jsonResponse = new JSONObject(EntityUtils.toString(response.getEntity()));
                while (!jsonResponse.getString("status").equals("success")) {
                    TimeUnit.SECONDS.sleep(15);
                    jsonResponse = checkStatus(httpClient, jsonResponse.getString("request_id"));
                }
                return jsonResponse.getString("request_id");
            } else {
                LOGGER.log(Level.SEVERE, "The post processing request ended with status code {0}.", response.getStatusLine().getStatusCode());
            }
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, null, e);

        }
        return null;
    }

    private JSONObject checkStatus(HttpClient httpClient, String requestId) throws IOException, JSONException {
        String url = this.serverUrl + "get_status?request_id=" + requestId;
        HttpGet httpGet = new HttpGet(url);
        httpGet.addHeader("api-key", this.apiKey);

        HttpResponse response = httpClient.execute(httpGet);
        if (response.getStatusLine().getStatusCode() == 200) {
            return new JSONObject(EntityUtils.toString(response.getEntity()));
        } else {
            throw new IOException("Unexpected code " + response.getStatusLine().getStatusCode());
        }
    }

    private JSONObject getEngines(HttpClient httpClient) throws IOException, JSONException {
        String url = this.serverUrl + "get_engines";
        HttpGet httpGet = new HttpGet(url);
        httpGet.addHeader("api-key", this.apiKey);

        HttpResponse response = httpClient.execute(httpGet);
        if (response.getStatusLine().getStatusCode() == 200) {
            return new JSONObject(EntityUtils.toString(response.getEntity()));
        } else {
            throw new IOException("Unexpected code " + response.getStatusLine().getStatusCode());
        }
    }

    private boolean uploadImage(HttpClient httpClient, String requestId, String fileName, String imagePath, String contentType) throws IOException {
        String url = this.serverUrl + "upload_image/" + requestId + "/" + fileName;
        HttpPost httpPost = new HttpPost(url);
        httpPost.addHeader("api-key", this.apiKey);

        File imageFile = new File(imagePath);
        final MultipartEntityBuilder builder = MultipartEntityBuilder.create();
        builder.addBinaryBody("file", imageFile);
        final HttpEntity entity = builder.build();
        httpPost.addHeader("Content-Type", entity.getContentType().getValue());
        httpPost.setEntity(entity);

        HttpResponse response = httpClient.execute(httpPost);
        if (response.getStatusLine().getStatusCode() >= 400) {
            LOGGER.log(Level.INFO, "The image upload of {0} ended with status code {1}. {2}",
                    new Object[]{fileName, response.getStatusLine().getStatusCode(),
                            EntityUtils.toString(response.getEntity())
                    });
            return false;
        } else {
            LOGGER.log(Level.INFO, "The image upload of {0} ended with status code {1}.", new Object[]{fileName, response.getStatusLine().getStatusCode()});
            return true;
        }
    }

    private String downloadResults(HttpClient httpClient, String outputPath, String requestId, String fileName, String resultFormat) {
        String url = this.serverUrl + "download_results/" + requestId + "/" + fileName + "/" + resultFormat;
        HttpGet httpGet = new HttpGet(url);
        httpGet.addHeader("api-key", this.apiKey);

        try {
            HttpResponse response = httpClient.execute(httpGet);
            HttpEntity entity = response.getEntity();

            if (response.getStatusLine().getStatusCode() == 200) {
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(entity.getContent(), StandardCharsets.UTF_8));
                     BufferedWriter writer = new BufferedWriter(
                             new OutputStreamWriter(new FileOutputStream(outputPath), StandardCharsets.UTF_8))) {
                    String line;
                    while ((line = reader.readLine()) != null) {
                        writer.write(line);
                        writer.newLine();
                    }
                    return "PROCESSED";
                }
            } else if (response.getStatusLine().getStatusCode() >= 400) {
                JSONObject jsonResponse = new JSONObject(EntityUtils.toString(entity, "UTF-8"));

//                LOGGER.log(Level.INFO, "The request returned status code {0}. The message is: {1}",
//                        new Object[]{response.getStatusLine().getStatusCode(), jsonResponse.getString("message")});
                if (jsonResponse.getString("message").contains("not processed yet")) {
                    return "UNPROCESSED";
                } else {
                    LOGGER.log(Level.INFO, jsonResponse.getString("message"));
                }
            }
        } catch (JSONException | IOException e) {
            LOGGER.log(Level.SEVERE, null, e);

        }
        return null;
    }

    private String getContentType(String fileExtension) {
        switch (fileExtension.toLowerCase()) {
            case "tiff":
            case "tif":
                return "image/tiff";
            case "jpg":
            case "jpeg":
                return "image/jpeg";
            case "jp2":
                return "image/jp2";
            default:
                LOGGER.log(Level.SEVERE, "Error: the extension {0} is not supported.",
                        fileExtension);
                return null;
        }
    }

    public ValueMap getEnginesList() {
        List<PeroOcrEnginesDescriptor> peroOcrEngines = new ArrayList<>();

        try {
            HttpClient httpClient = HttpClients.createDefault();
            JSONObject response = getEngines(httpClient);

            if (response == null) {
                throw new IOException("Response object is null");
            }

            if (!response.has("engines") || response.isNull("engines")) {
                throw new IOException("Missing required object 'engines' in response");
            }

            JSONObject engines;
            try {
                engines = response.getJSONObject("engines");
            } catch (Exception e) {
                throw new IOException("'engines' is not a valid JSON object", e);
            }

            for (Iterator it = engines.keys(); it.hasNext(); ) {
                String label = it.next().toString();
                JSONObject engine = engines.optJSONObject(label);

                if (engine == null) {
                    throw new IOException("Engine entry '" + label + "' is not a valid JSON object");
                }

                if (!engine.has("id")) {
                    throw new IOException("Missing required field 'id' for engine '" + label + "'");
                }

                int id;
                try {
                    id = engine.getInt("id");
                } catch (Exception e) {
                    throw new IOException("Field 'id' is not a valid integer for engine '" + label + "'", e);
                }

                String description = engine.isNull("description") ? null : engine.getString("description");

                peroOcrEngines.add(new PeroOcrEnginesDescriptor(id, label, description));
            }
        } catch (JSONException | IOException e) {
            LOGGER.severe("Problem s nactenim PERO OCR engines. Mozna neni nakonfigurovane PERO.");
            e.printStackTrace();
        }

        peroOcrEngines.sort(Comparator.comparingInt(PeroOcrEnginesDescriptor::getId));

        return new ValueMap<>("peroOcr.engines", peroOcrEngines);
    }

    class PeroOcrEnginesDescriptor {

        private int id;
        private String label;
        private String description;

        public PeroOcrEnginesDescriptor(int id, String label, String description) {
            this.id = id;
            this.label = label;
            this.description = description;
        }

        public int getId() {
            return id;
        }

        public String getLabel() {
            return label;
        }

        public String getDescription() {
            return description;
        }
    }
}