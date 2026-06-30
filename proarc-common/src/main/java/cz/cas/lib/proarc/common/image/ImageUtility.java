package cz.cas.lib.proarc.common.image;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageReader;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.stream.FileImageOutputStream;

public class ImageUtility {

    /**
     * Načte obrázek z URL a převede ho na BufferedImage typu INT_RGB vhodný pro JPG.
     * Podporuje formáty s Java-native podporou nebo TIFF přes TwelveMonkeys.
     */
    public static BufferedImage readImage(URL url, ImageMimeType type) throws IOException {
        if (type == null) {
            return null;
        }

        // Všechny formáty podporované TwelveMonkeys lze načíst přes ImageIO.read
        BufferedImage img = null;
        try {
            img = ImageIO.read(url);
        } catch (IOException ex) {
            File originalFile = new File(url.getFile());
            ImageReader reader = ImageIO.getImageReaders(ImageIO.createImageInputStream(originalFile)).next();
            reader.setInput(ImageIO.createImageInputStream(originalFile), true, true);

            ImageReadParam param = reader.getDefaultReadParam();

            // dynamický výpočet subsamplingu
            int width = reader.getWidth(0);
            int height = reader.getHeight(0);

            // cílový max počet pixelů (např. pro preview)
            long maxPixels = 50_000_000; // ~50 MPx

            long totalPixels = (long) width * height;
            int subsampling = (int) Math.ceil(Math.sqrt((double) totalPixels / maxPixels));

            param.setSourceSubsampling(subsampling, subsampling, 0, 0);

            img = reader.read(0, param);
        }

        if (img == null) {
            throw new IllegalArgumentException("Unsupported or corrupted image: " + type.getMimeType());
        }

        // Pokud typ obrázku není standardní RGB, převedeme na TYPE_INT_RGB (pro JPG)
        if (img.getType() != BufferedImage.TYPE_INT_RGB) {
            BufferedImage converted = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_RGB);
            Graphics2D g2 = converted.createGraphics();
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            // vyplníme bílým pozadím (ochrana proti alpha)
            g2.setColor(Color.WHITE);
            g2.fillRect(0, 0, img.getWidth(), img.getHeight());

            g2.drawImage(img, 0, 0, null);
            g2.dispose();
            return converted;
        }

        return img;
    }

    public static BufferedImage scale(BufferedImage img, int targetWidth, int targetHeight, ScalingMethod method, boolean higherQuality) {
        switch (method) {
            case BILINEAR:
                return getScaledInstanceJava2D(img, targetWidth, targetHeight, RenderingHints.VALUE_INTERPOLATION_BILINEAR, higherQuality);
            case BICUBIC:
                return getScaledInstanceJava2D(img, targetWidth, targetHeight, RenderingHints.VALUE_INTERPOLATION_BICUBIC, higherQuality);
            case NEAREST_NEIGHBOR:
                return getScaledInstanceJava2D(img, targetWidth, targetHeight, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR, higherQuality);
            case BILINEAR_STEPPED:
                return getScaledInstanceJava2D(img, targetWidth, targetHeight, RenderingHints.VALUE_INTERPOLATION_BILINEAR, higherQuality);
            case BICUBIC_STEPPED:
                return getScaledInstanceJava2D(img, targetWidth, targetHeight, RenderingHints.VALUE_INTERPOLATION_BICUBIC, higherQuality);
            case NEAREST_NEIGHBOR_STEPPED:
                return getScaledInstanceJava2D(img, targetWidth, targetHeight, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR, higherQuality);
            default:
                return null;
        }
    }

    private static BufferedImage getScaledInstanceJava2D(
            BufferedImage img,
            int targetWidth,
            int targetHeight,
            Object hint,
            boolean higherQuality) {

        int type = BufferedImage.TYPE_INT_RGB;
        BufferedImage ret = img;

        int w = higherQuality ? img.getWidth() : targetWidth;
        int h = higherQuality ? img.getHeight() : targetHeight;

        do {
            if (w > targetWidth) {
                w = Math.max(w / 2, targetWidth);
            }

            if (h > targetHeight) {
                h = Math.max(h / 2, targetHeight);
            }

            BufferedImage tmp = new BufferedImage(w, h, type);
            Graphics2D g2 = tmp.createGraphics();

            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, hint);
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            g2.setColor(Color.WHITE);
            g2.fillRect(0, 0, w, h);

            g2.drawImage(ret, 0, 0, w, h, null);
            g2.dispose();

            ret = tmp;

        } while (w > targetWidth || h > targetHeight);

        return ret;
    }

    public static void writeImageToStream(BufferedImage scaledImage, String javaFormat, FileImageOutputStream os, float quality) throws IOException {
        Iterator<ImageWriter> iter = ImageIO.getImageWritersByFormatName(javaFormat);
        if (iter.hasNext()) {
            ImageWriter writer = (ImageWriter)iter.next();
            try {
                ImageWriteParam iwp = writer.getDefaultWriteParam();

                // nastav kompresi pro JPEG
                if (iwp.canWriteCompressed()) {
                    iwp.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
                    iwp.setCompressionQuality(quality); // 0.0f - 1.0f
                }

                writer.setOutput(os);
                IIOImage image = new IIOImage(scaledImage, (List) null, (IIOMetadata) null);
                writer.write((IIOMetadata) null, image, iwp);
            } finally {
                writer.dispose();
            }
        } else {
            throw new IOException("No writer for format '" + javaFormat + "'");
        }
    }

    public static enum ScalingMethod {
        REPLICATE,
        AREA_AVERAGING,
        BILINEAR,
        BICUBIC,
        NEAREST_NEIGHBOR,
        BILINEAR_STEPPED,
        BICUBIC_STEPPED,
        NEAREST_NEIGHBOR_STEPPED;
    }
}
