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

import com.yourmediashelf.fedora.generated.foxml.ContentLocationType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.StateType;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.incad.imgsupport.ImageMimeType;
import cz.incad.imgsupport.ImageSupport;
import cz.incad.pas.editor.server.fedora.LocalStorage;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import cz.incad.pas.editor.server.imports.ImportProcess.ImportContext;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
import java.awt.image.BufferedImage;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.channels.FileChannel;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.stream.FileImageOutputStream;
import javax.ws.rs.core.MediaType;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

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
    
    private static final String NS_DC = "http://purl.org/dc/elements/1.1/";


    public ImportItem consume(File f, String mimetype, ImportContext ctx) throws IOException {
        // check tiff file
        if (!isTiff(f, mimetype)) {
            return null;
        }

        String originalFilename = getName(f);
        File tempBatchFolder = ctx.getTargetFolder();

        // creates FOXML and metadata
        LocalStorage storage = new LocalStorage();
        File foxml = new File(tempBatchFolder, originalFilename + ".foxml");
        LocalObject localObj = storage.create(foxml);
        DigitalObject digObj = localObj.getDigitalObject();
        String pid = localObj.getPid();
        String uuid = pid.substring("uuid:".length());

        ModsStreamEditor modsEditor = new ModsStreamEditor(localObj);
        ModsType mods = modsEditor.createPage(pid, null, null, null);
        modsEditor.write(mods, 0);

        generateDublinCore(digObj, pid, uuid, ctx);
        createImages(tempBatchFolder, f, originalFilename, digObj, ctx.getXmlNow());

        // XXX generate OCR name.txt.ocr
        // XXX generate ATM
        // writes FOXML
        localObj.flush();

        return new ImportItem(foxml, originalFilename, pid);
    }

    private boolean isTiff(File f, String mimetype) {
        return ImageMimeType.TIFF.getMimeType().equals(mimetype);
    }

    private DigitalObject generateDublinCore(DigitalObject digObj, String uuid, String filename, ImportContext ctx) {
        DatastreamType dsDc = createDublinCore(uuid, filename, ctx);
        digObj.getDatastream().add(dsDc);
        return digObj;
    }

    private DatastreamType createDublinCore(String uuid, String filename, ImportContext ctx) {
        DatastreamType ds = createDatastream("DC", "X", false, StateType.A);

        DatastreamVersionType streamVersion = new DatastreamVersionType();
        streamVersion.setID(ds.getID() + ".0");
        streamVersion.setLABEL("Dublin Core description");
        streamVersion.setMIMETYPE(MediaType.TEXT_XML);
        streamVersion.setFORMATURI("http://www.openarchives.org/OAI/2.0/oai_dc/");
        // do not set created; it is assigned by fedora
        streamVersion.setCREATED(ctx.getXmlNow());
        ds.getDatastreamVersion().add(streamVersion);

        // create DC
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        try {
            Document d = factory.newDocumentBuilder().newDocument();
            Element root = d.createElementNS("http://www.openarchives.org/OAI/2.0/oai_dc/", "oai_dc:dc");
            root.setAttribute("xmlns:dc", NS_DC);
            d.appendChild(root);

            Element title = d.createElementNS(NS_DC, "dc:title");
            title.setTextContent(filename);
            root.appendChild(title);

            Element identifier = d.createElementNS(NS_DC, "dc:identifier");
            identifier.setTextContent("uuid:" + uuid);
            root.appendChild(identifier);

            Element type = d.createElementNS(NS_DC, "dc:type");
            type.setTextContent("model:page");
            root.appendChild(type);

            XmlContentType xml = new XmlContentType();
            xml.getAny().add(root);
            streamVersion.setXmlContent(xml);
            return ds;
        } catch (ParserConfigurationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private static DatastreamType createDatastream(String id, String controlGroup, boolean versionable, StateType state) {
        DatastreamType ds = new DatastreamType();
        ds.setID(id);
        ds.setCONTROLGROUP(controlGroup);
        ds.setVERSIONABLE(versionable);
        ds.setSTATE(state);
        return ds;
    }

    private void createOcr() {

    }

    private void createRelsExt() {

    }

    private void createImages(File tempBatchFolder, File original, String originalFilename, DigitalObject foxml, XMLGregorianCalendar xmlNow) throws IOException {
        long start = System.nanoTime();
        BufferedImage tiff = ImageSupport.readImage(original.toURI().toURL(), ImageMimeType.TIFF);
        long endRead = System.nanoTime() - start;
        ImageMimeType imageType = ImageMimeType.JPEG;

        start = System.nanoTime();
        String targetName = String.format("%s.full.%s", originalFilename, imageType.getDefaultFileExtension());
        File f = writeImage(tiff, tempBatchFolder, targetName, imageType);
        long endFull = System.nanoTime() - start;
        foxml.getDatastream().add(createImageStream("IMG_FULL", imageType.getMimeType(), f.toURI(), xmlNow));

        start = System.nanoTime();
        targetName = String.format("%s.preview.%s", originalFilename, imageType.getDefaultFileExtension());
        f = writeImage(scale(tiff, 800, 600), tempBatchFolder, targetName, imageType);
        long endPreview = System.nanoTime() - start;
        foxml.getDatastream().add(createImageStream("IMG_PREVIEW", imageType.getMimeType(), f.toURI(), xmlNow));

        start = System.nanoTime();
        targetName = String.format("%s.thumb.%s", originalFilename, imageType.getDefaultFileExtension());
        f = writeImage(scale(tiff, 120, 128), tempBatchFolder, targetName, imageType);
        long endThumb = System.nanoTime() - start;
        foxml.getDatastream().add(createImageStream("IMG_THUMB", imageType.getMimeType(), f.toURI(), xmlNow));

        LOG.info(String.format("file: %s, read: %s, full: %s, preview: %s, thumb: %s",
                originalFilename, endRead / 1000000, endFull / 1000000, endPreview / 1000000, endThumb / 1000000));
    }

    private static DatastreamType createImageStream(String id, String mime, URI ref, XMLGregorianCalendar xmlNow) {
        DatastreamType stream = createDatastream(id, "M", false, StateType.A);

        ContentLocationType location = new ContentLocationType();
        location.setTYPE("URL");
        location.setREF(ref.toASCIIString());

        DatastreamVersionType version = new DatastreamVersionType();
        version.setMIMETYPE(mime);
        version.setID(stream.getID() + ".0");
        version.setContentLocation(location);
        version.setCREATED(xmlNow);

        stream.getDatastreamVersion().add(version);
        return stream;
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

    private static BufferedImage scale(BufferedImage tiff, int maxWidth, int maxHeight) {
        long start = System.nanoTime();
        int height = tiff.getHeight();
        int width = tiff.getWidth();
        int targetWidth = width;
        int targetHeight = height;
        if (height > maxHeight || width > maxWidth) {
            double scaleh = (double) maxHeight / height;
            double scalew = (double) maxWidth / width;
            double scale = Math.min(scaleh, scalew);
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
