/*
 * Copyright (C) 2011 Jan Pokorsky
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
package cz.incad.pas.editor.server.imports;

import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import cz.incad.pas.editor.server.imports.ImportProcess.FedoraImportItem;
import cz.incad.pas.editor.server.imports.ImportProcess.ImportContext;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Requires Java Advanced Imaging support.
 * See http://www.oracle.com/technetwork/java/current-142188.html and
 * http://download.java.net/media/jai/builds/release/1_1_3/
 * jai-1_1_3-lib.zip is a platform independent version
 *
 * http://download.java.net/media/jai-imageio/builds/release/1.1/ fo jai_imageio-1.1.jar
 *
 * For maven, try to depend just on com.sun.media.jai_imageio.1.1 as kramerius common.
 * How to properly depend in pom see http://sahits.ch/blog/?p=1038
 *
 * @author Jan Pokorsky
 */
public class TiffImporter {

    private static final Logger LOG = Logger.getLogger(TiffImporter.class.getName());


    public FedoraImportItem consume(File f, String mimetype, ImportContext ctx) throws IOException {
        // check tiff file
        if (!isTiff(f, mimetype)) {
            return null;
        }

        File tempBatchFolder = ctx.getTargetFolder();
        // copy name.tiff.raw
        copyFile(f, new File(tempBatchFolder, f.getName() + ".raw"));
        // create name.jpeg.preview
        copyFile(f, new File(tempBatchFolder, f.getName() + ".preview"));
        // create name.jpeg.thumb
        copyFile(f, new File(tempBatchFolder, f.getName() + ".thumb"));
        // generate OCR name.txt.ocr
        // generate ATM
        // generate FOXML
        File foxml = new File(tempBatchFolder, f.getName() + ".foxml");
        boolean createNewFile = foxml.createNewFile();

        UUID uuid = UUID.randomUUID();
        String pid = "uuid:" + uuid;
        return new FedoraImportItem(foxml, pid);
    }

    private boolean isTiff(File f, String mimetype) {
//        return "image/tiff".equals(mimetype);
        return "image/jpeg".equals(mimetype);
    }

    private void copyFile(File src, File dst) throws IOException {
        FileChannel srcChannel = null;
        FileChannel dstChannel = null;
        FileInputStream srcStream = null;
        FileOutputStream dstStream = null;
        boolean done = false;
        try {
            srcStream = new FileInputStream(src);
            dstStream = new FileOutputStream(dst);
            srcChannel = srcStream.getChannel();
            dstChannel = dstStream.getChannel();
            long count = 0;
            long length = src.length();
            while ((count += dstChannel.transferFrom(srcChannel, count, length - count)) < length) {
                // no op
            }
            done = true;
        } finally {
            closeQuietly(srcChannel, src.toString(), done);
            closeQuietly(srcStream, src.toString(), done);
            closeQuietly(dstChannel, dst.toString(), done);
            closeQuietly(dstStream, dst.toString(), done);
        }
    }

    private static void closeQuietly(Closeable c, String description, boolean notQuietly) throws IOException {
        if (c != null) {
            try {
                c.close();
            } catch (IOException ex) {
                if (notQuietly) {
                    throw ex;
                }
                LOG.log(Level.SEVERE, description, ex);
            }
        }
    }
}
