package cz.cas.lib.proarc.common.process;

import java.io.File;
import java.util.List;
import org.apache.commons.configuration.Configuration;

/**
 * Created by Jakub Kremlacek on 19.4.17.
 */
public class OcrGenerator extends ExternalProcess {

    public static final String ID = "abbycmd";

    private final File input;
    private final File outputOcr;
    private final File outputAlto;

    /**
     * Returns OCR file links corresponding to image file
     *
     * @param imageFile
     * @param plainOcrFileSuffix
     * @param altoFileSuffix
     * @return array containing plainOcr File at index 0 and alto File at index 1
     */
    public static File[] getOcrFiles(File imageFile, String plainOcrFileSuffix, String altoFileSuffix) {
        File txt = new File(imageFile.getAbsolutePath().substring(0, imageFile.getAbsolutePath().lastIndexOf('.')) + plainOcrFileSuffix);
        File alto = new File(imageFile.getAbsolutePath().substring(0, imageFile.getAbsolutePath().lastIndexOf('.')) + altoFileSuffix);

        return new File[]{txt, alto};
    }

    public OcrGenerator(Configuration conf, File tiff, String plainOcrFileSuffix, String altoFileSuffix) {
        super(conf);

        this.input = tiff;

        File[] outputFiles = getOcrFiles(tiff, plainOcrFileSuffix, altoFileSuffix);

        this.outputOcr = outputFiles[0];
        this.outputAlto = outputFiles[1];
    }

    @Override
    public void run() {
        if (!input.exists()) {
            throw new IllegalStateException(input.getAbsolutePath() + " not exists!");
        }
        if (outputOcr.exists()) {
            throw new IllegalStateException(outputOcr.getAbsolutePath() + " exists!");
        }
        if (outputAlto.exists()) {
            throw new IllegalStateException(outputAlto.getAbsolutePath() + " exists!");
        }
        super.run();
    }

    @Override
    public boolean isOk() {
        return super.isOk() &&
                outputOcr.exists() && outputOcr.length() > 0 &&
                outputAlto.exists() && outputAlto.length() > 0;
    }

    @Override
    protected List<String> buildCmdLine(Configuration conf) {
        String inputFile = input.getAbsolutePath();
        String outputOcrFile = outputOcr.getAbsolutePath();
        String outputAltoFile = outputAlto.getAbsolutePath();
        List<String> cmdLine = super.buildCmdLine(conf);
        cmdLine.add("-i");
        cmdLine.add(inputFile);
        cmdLine.add("-oO");
        cmdLine.add(outputOcrFile);
        cmdLine.add("-oA");
        cmdLine.add(outputAltoFile);
        return cmdLine;
    }
}