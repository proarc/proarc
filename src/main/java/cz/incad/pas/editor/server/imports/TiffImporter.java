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

import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.incad.imgsupport.ImageMimeType;
import cz.incad.imgsupport.ImageSupport;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.incad.pas.editor.server.dublincore.DcUtils;
import cz.incad.pas.editor.server.fedora.BinaryEditor;
import cz.incad.pas.editor.server.fedora.DigitalObjectException;
import cz.incad.pas.editor.server.fedora.LocalStorage;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.StringEditor;
import cz.incad.pas.editor.server.fedora.relation.RelationEditor;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import cz.incad.pas.editor.server.imports.ImportProcess.ImportOptions;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
import java.awt.image.BufferedImage;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.stream.FileImageOutputStream;
import javax.ws.rs.core.MediaType;

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
public final class TiffImporter {

    private static final Logger LOG = Logger.getLogger(TiffImporter.class.getName());

    public boolean accept(File f, String mimetype) {
        return isTiff(f, mimetype);
    }
    
    public ImportItem consume(File f, String mimetype, ImportOptions ctx) throws IOException {
        try {
            return consumeImpl(f, mimetype, ctx);
        } catch (DigitalObjectException ex) {
            throw new IOException(ex);
        }
    }

    private ImportItem consumeImpl(File f, String mimetype, ImportOptions ctx) throws IOException, DigitalObjectException {
        // check tiff file
        if (!isTiff(f, mimetype)) {
            return null;
        }

        String originalFilename = getName(f);
        String fedoraModel = ctx.getModel();
        File tempBatchFolder = ctx.getTargetFolder();

        // creates FOXML and metadata
        LocalStorage storage = new LocalStorage();
        File foxml = new File(tempBatchFolder, originalFilename + ".foxml");
        LocalObject localObj = storage.create(foxml);
        localObj.setOwner(ctx.getUsername());
        String pid = localObj.getPid();

        // MODS
        ModsStreamEditor modsEditor = new ModsStreamEditor(localObj);
        String pageIndex = ctx.isGenerateIndices() ? String.valueOf(ctx.getConsumedFileCounter() + 1) : null;
        ModsType mods = modsEditor.createPage(pid, pageIndex, null, null);
        modsEditor.write(mods, 0, null);

        // DC
        DcStreamEditor dcEditor = new DcStreamEditor(localObj);
        dcEditor.write(mods, fedoraModel, 0, null);
        DublinCoreRecord dcr = dcEditor.read();
        localObj.setLabel(DcUtils.getLabel(dcr.getDc()));

        // RELS-EXT
        RelationEditor relEditor = new RelationEditor(localObj);
        relEditor.setModel(fedoraModel);
        relEditor.setDevice(ctx.getDevice());
        relEditor.write(0, null);
        // XXX use fedora-model:downloadFilename in RELS-INT or label of datastream to specify filename

        // Images
        BinaryEditor.dissemination(localObj, BinaryEditor.RAW_ID, BinaryEditor.IMAGE_TIFF)
                .write(f, 0, null);
        createImages(tempBatchFolder, f, originalFilename, localObj);

        // OCR
        StringEditor ocrEditor = StringEditor.ocr(localObj);
        ocrEditor.write("", 0, null);
        
        // XXX generate ATM
        // writes FOXML
        localObj.flush();

        return new ImportItem(foxml, originalFilename, pid);
    }

    private boolean isTiff(File f, String mimetype) {
        return ImageMimeType.TIFF.getMimeType().equals(mimetype);
    }

    private void createImages(File tempBatchFolder, File original, String originalFilename, LocalObject foxml)
            throws IOException, DigitalObjectException {
        
        long start = System.nanoTime();
        BufferedImage tiff = ImageSupport.readImage(original.toURI().toURL(), ImageMimeType.TIFF);
        long endRead = System.nanoTime() - start;
        ImageMimeType imageType = ImageMimeType.JPEG;
        MediaType mediaType = MediaType.valueOf(imageType.getMimeType());

        start = System.nanoTime();
        String targetName = String.format("%s.full.%s", originalFilename, imageType.getDefaultFileExtension());
        File f = writeImage(tiff, tempBatchFolder, targetName, imageType);
        long endFull = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, BinaryEditor.FULL_ID, mediaType).write(f, 0, null);

        start = System.nanoTime();
        targetName = String.format("%s.preview.%s", originalFilename, imageType.getDefaultFileExtension());
        f = writeImage(scale(tiff, null, 1000), tempBatchFolder, targetName, imageType);
        long endPreview = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, BinaryEditor.PREVIEW_ID, mediaType).write(f, 0, null);

        start = System.nanoTime();
        targetName = String.format("%s.thumb.%s", originalFilename, imageType.getDefaultFileExtension());
        f = writeImage(scale(tiff, 120, 128), tempBatchFolder, targetName, imageType);
        long endThumb = System.nanoTime() - start;
        BinaryEditor.dissemination(foxml, BinaryEditor.THUMB_ID, mediaType).write(f, 0, null);

        LOG.info(String.format("file: %s, read: %s, full: %s, preview: %s, thumb: %s",
                originalFilename, endRead / 1000000, endFull / 1000000, endPreview / 1000000, endThumb / 1000000));
    }

    private static File writeImage(BufferedImage image, File folder, String filename, ImageMimeType imageType) throws IOException {
        File imgFile = new File(folder, filename);
        FileImageOutputStream fos = new FileImageOutputStream(imgFile);
        try {
            ImageSupport.writeImageToStream(image, imageType.getDefaultFileExtension(), fos, 1.0f);
            return imgFile;
        } finally {
            fos.close();
        }
    }

    private static BufferedImage scale(BufferedImage tiff, Integer maxWidth, Integer maxHeight) {
        long start = System.nanoTime();
        int height = tiff.getHeight();
        int width = tiff.getWidth();
        int targetWidth = width;
        int targetHeight = height;
        double scale = Double.MAX_VALUE;
        if (maxHeight != null && height > maxHeight) {
            scale = (double) maxHeight / height;
        }
        if (maxWidth != null && width > maxWidth) {
            double scalew = (double) maxWidth / width;
            scale = Math.min(scale, scalew);
        }
        if (scale != Double.MAX_VALUE) {
            targetHeight = (int) (height * scale);
            targetWidth = (int) (width * scale);
        }
        BufferedImage scaled = ImageSupport.scale(tiff, targetWidth, targetHeight);
        LOG.info(String.format("scaled [%s, %s] to [%s, %s], boundary [%s, %s] [w, h], time: %s ms",
                width, height, targetWidth, targetHeight, maxWidth, maxHeight, (System.nanoTime() - start) / 1000000));
        return scaled;
    }

    private static String getName(File f) {
        String fname = f.getName();
        int index = fname.indexOf('.');
        return index > 0 ? fname.substring(0, index) : fname;
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
