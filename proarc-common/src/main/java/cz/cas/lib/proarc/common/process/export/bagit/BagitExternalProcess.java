package cz.cas.lib.proarc.common.process.export.bagit;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.List;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

public class BagitExternalProcess {

    private static final Logger LOG = Logger.getLogger(BagitExternalProcess.class.getName());

    private File exportFolder;
    private File script;
    private Configuration conf;
    private String output;
//    private int exitCode;

    public BagitExternalProcess(Configuration conf, File script, File exportFolder) {
        this.conf = conf;
        this.script = script;
        this.exportFolder = exportFolder;
//        this.exitCode = 0;
    }

    public void run() throws IOException{
        if (exportFolder == null) {
            throw new IllegalStateException("Unknown export folder :" + exportFolder);
        }
        if (!exportFolder.exists()) {
            throw new IllegalStateException(exportFolder.getAbsolutePath() + "does not exists.");
        }
        BagitProcess bagitProcess = new BagitProcess(conf, exportFolder);
        List<String> bagitCommand = bagitProcess.buildCmdLine(conf);
        String[] bagitCmd = new String[bagitCommand.size()];
        bagitCommand.toArray(bagitCmd);

        LOG.info(String.join(" ", bagitCmd));
        saveToFile(script, bagitCmd);

        Runtime r = Runtime.getRuntime();

        String s;
        StringWriter infoWriter = new StringWriter();
        StringWriter errorWriter = new StringWriter();
        try {
            Process p = r.exec(script.getAbsolutePath());
            LOG.info("Bagit Process finished");

//            BufferedReader info = new BufferedReader(new InputStreamReader(p.getInputStream()));
//
//
//            while ((s = info.readLine()) != null) {
////               System.out.println(s);
//                infoWriter.append(s).append("\n");
//            }
//            if (!infoWriter.toString().isEmpty()) {
//                LOG.info(infoWriter.toString());
//                output += infoWriter.toString();
//            }


            BufferedReader error = new BufferedReader(new InputStreamReader(p.getErrorStream()));
            int i = 0;
            while ((s = error.readLine()) != null) {
//                System.out.println(s);
                errorWriter.append(s).append("\n");
                LOG.fine("Line " + i++ + " " + s);
            }
            if (!errorWriter.toString().isEmpty()) {
                LOG.info(errorWriter.toString());
                output += "\n" + errorWriter.toString();
//                this.exitCode = -1;
            } else {
                LOG.info("No errors in bagit process.");
            }
        } finally {
            infoWriter.close();
            errorWriter.close();
        }
    }

    private void saveToFile(File script, String[] ltpUploadCmd) {
        try {
            PrintWriter pw = new PrintWriter(new FileOutputStream(script));
            String cmd = String.join(" ", ltpUploadCmd);
            pw.println(cmd);
            LOG.info("Prikaz ulozen do: " + script.getAbsolutePath());
            pw.close();
        } catch (FileNotFoundException ex) {
            LOG.severe("Chyba ukladani do: " + script.getAbsolutePath());
            ex.printStackTrace();
        }
    }

    public String getFullOutput() {
        return output;
    }

    public boolean isOk(File folder) {
        File tagManifestMd5 = new File(folder, "tagmanifest-md5.txt");
        LOG.info("Checking process status - file " + tagManifestMd5.getAbsolutePath() + " exists " + (tagManifestMd5 != null && tagManifestMd5.exists()));
        return tagManifestMd5 != null && tagManifestMd5.exists();
    }
}
