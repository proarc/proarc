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
 * Processes K4 and Image export for Kramerius using Imageserver.
 *
 * @author Jakub Kremlacek
 */
public class ExportKWISPostProcess extends ExternalProcess {

    private String imagesPath;
    private String k4Path;
    private String exportPath;

    public ExportKWISPostProcess(Configuration conf, String imagesPath, String k4Path, String exportPath) {
        super(conf);

        this.imagesPath = imagesPath;
        this.k4Path = k4Path;
        this.exportPath = exportPath;
    }

    @Override
    public void run() {
        if (!new File(imagesPath).exists()) {
            throw new IllegalStateException(imagesPath.toString() + " does not exist.");
        }

        if (!new File(k4Path).exists()) {
            throw new IllegalStateException(k4Path.toString() + " does not exist.");
        }

        if (!new File(exportPath).exists()) {
            throw new IllegalStateException(exportPath.toString() + " does not exist.");
        }

        super.run();
    }

    @Override
    protected List<String> buildCmdLine(Configuration conf) {
        String imageInput = new File(imagesPath).getAbsolutePath();
        String k4Input = new File(k4Path).getAbsolutePath();
        String output = new File(exportPath).getAbsolutePath();

        List<String> cmdLine = super.buildCmdLine(conf);
        cmdLine.add("-iI");
        cmdLine.add(imageInput);
        cmdLine.add("-iK");
        cmdLine.add(k4Input);
        cmdLine.add("-o");
        cmdLine.add(output);

        return cmdLine;
    }
}