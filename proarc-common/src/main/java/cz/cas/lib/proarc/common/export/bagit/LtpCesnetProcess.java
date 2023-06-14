package cz.cas.lib.proarc.common.export.bagit;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

import static cz.cas.lib.proarc.common.process.ExternalProcess.PROP_ARG;
import static cz.cas.lib.proarc.common.process.ExternalProcess.PROP_EXEC;

public class LtpCesnetProcess {

    private static final Logger LOG = Logger.getLogger(LtpCesnetProcess.class.getName());

    private File exportFolder;
    private String token;
    private String group;
    private String metadata;
    private Configuration conf;
    private File script;

    public LtpCesnetProcess(Configuration conf, String token, String group, String scriptPath, File exportFolder, String metadata) {
        this.conf = conf;
        this.exportFolder = exportFolder;
        this.token = token;
        this.group = group;
        this.metadata = metadata;
        this.script = new File(scriptPath);
    }

    public void run() throws IOException {

        String exec = this.conf.getString(PROP_EXEC);
        if (exec == null) {
            throw new IllegalStateException("Missing 'exec'!");
        }
        String pythonScriptHome = this.conf.getString(PROP_ARG);
        if (pythonScriptHome == null) {
            throw new IllegalStateException("Missing 'arg'!");
        }
        if (script == null || !script.exists()) {
            throw new IllegalStateException("Script doesnt exists! " + script.getAbsolutePath());
        }

        String [] ltpUploadCmd = new String[14];
        ltpUploadCmd[0] = exec;
        ltpUploadCmd[1] = pythonScriptHome;
        ltpUploadCmd[2] = "-t";
        ltpUploadCmd[3] = token;
        ltpUploadCmd[4] = "-c";
        ltpUploadCmd[5] = group;
        ltpUploadCmd[6] = "-a";
        ltpUploadCmd[7] = "https://ltp.cesnet.cz/api/";
        ltpUploadCmd[8] = "archive";
        ltpUploadCmd[9] = "create";
        ltpUploadCmd[10] = "-d";
        ltpUploadCmd[11] = metadata;
        ltpUploadCmd[12] = "-p";
        ltpUploadCmd[13] = exportFolder.getAbsolutePath();

        LOG.info(String.join(" ", ltpUploadCmd));

        saveToFile(script, ltpUploadCmd);

        Runtime r = Runtime.getRuntime();

        String s;
        Process p = r.exec(script.getAbsolutePath());
        BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));

        StringWriter infoWriter = new StringWriter();
        while(( s = in.readLine()) != null) {
            System.out.println(s);
            infoWriter.append(s).append("\n");
        }
        if (!infoWriter.toString().isEmpty()) {
            LOG.info(infoWriter.toString());
        }

        StringWriter errorWriter = new StringWriter();
        BufferedReader error = new BufferedReader(new InputStreamReader(p.getErrorStream()));
        while(( s = error.readLine()) != null) {
            System.out.println(s);
            errorWriter.append(s).append("\n");
        }
        if (!errorWriter.toString().isEmpty()) {
            LOG.severe(errorWriter.toString());
        }
    }

    private void saveToFile(File script, String[] ltpUploadCmd) {
        try {
            PrintWriter pw = new PrintWriter(new FileOutputStream(script));
            String cmd = String.join(" ", ltpUploadCmd);
            pw.println(cmd);
            LOG.info("Prikaz ulozen do: " + script.getAbsolutePath());
            pw.close();
        } catch (FileNotFoundException var5) {
            LOG.severe("Chyba ukladani do: " + script.getAbsolutePath());
            var5.printStackTrace();
        }
    }
}
