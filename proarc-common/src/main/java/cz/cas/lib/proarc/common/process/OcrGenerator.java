/*
 * Copyright (C) 2020 Jakub Kremlacek
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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