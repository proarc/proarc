/*
 * Copyright (C) 2013 Robert Simonovsky
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

package cz.cas.lib.proarc.common.export.mets;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.io.IOUtils;
import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import cz.cas.lib.proarc.common.export.mets.structure.MetsInfo;
import edu.harvard.hul.ois.jhove.App;
import edu.harvard.hul.ois.jhove.JhoveBase;
import edu.harvard.hul.ois.jhove.Module;
import edu.harvard.hul.ois.jhove.OutputHandler;

/**
 * @author eskymo
 * 
 *         Utility class for jHove application
 * 
 */
public class JhoveUtility {

    private static Logger logger = Logger.getLogger(JhoveUtility.class);

    private static Node getMixRecursive(Node node) {
        if ((node.getNodeName().startsWith("mix"))) {
            return node;
        } else {
            NodeList nl = node.getChildNodes();
            for (int a = 0; a < nl.getLength(); a++) {
                Node mix = getMixRecursive(nl.item(a));
                if (mix != null) {
                    return mix;
                }
            }
        }
        return null;
    }

    public static void initJhove(MetsInfo metsInfo) {
        Calendar calendar = Calendar.getInstance();

        App app = new App(JhoveUtility.class.getSimpleName(), "1.0", new int[] { calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH), calendar.get(Calendar.DAY_OF_MONTH) }, "jHove", "");
        try {

            JhoveBase jhoveBase = new JhoveBase();
            File jhoveConfigFile = createJhoveConfigurationFile();
            jhoveBase.init(jhoveConfigFile.getAbsolutePath(), null);
            metsInfo.jhoveBase = jhoveBase;
            metsInfo.jhoveApp = app;
        } catch (Exception ex) {
            logger.error(ex.getLocalizedMessage());
            throw new IllegalStateException(ex);
        }
    }

    public static Node getMixNode(File targetFile, MetsInfo metsinfo) {
        if (targetFile == null || !targetFile.isFile() || !targetFile.exists()) {
            logger.warn("target file '" + targetFile + "' cannot be found.");
            throw new RuntimeException("target file '" + targetFile + "' cannot be found.");
        }
        if (metsinfo.jhoveBase == null) {
            initJhove(metsinfo);
        }
        try {
            File outputFile = File.createTempFile("jhove", "output");
            logger.debug("JHOVE output file " + outputFile);
            Module module = metsinfo.jhoveBase.getModule(null);
            OutputHandler aboutHandler = metsinfo.jhoveBase.getHandler(null);
            OutputHandler xmlHandler = metsinfo.jhoveBase.getHandler("XML");
            logger.debug("Calling JHOVE dispatch(...) on file " + targetFile);
            metsinfo.jhoveBase.dispatch(metsinfo.jhoveApp, module, aboutHandler, xmlHandler, outputFile.getAbsolutePath(), new String[] { targetFile.getAbsolutePath() });
            DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = builderFactory.newDocumentBuilder();
            Document jHoveDoc = builder.parse(outputFile);
            outputFile.delete();
            return getMixRecursive(jHoveDoc);
        } catch (Exception e) {
            logger.warn("Error inspecting file '" + targetFile + "' - " + e.getMessage(), e);
        }
        return null;
    }

    /**
     * Copy the Jhove configuration file to a temporary file.
     * 
     * @return the {@link File} where the Jhove configuration was saved.
     * 
     * @throws IOException
     */
    private static File createJhoveConfigurationFile() throws IOException {
        InputStream jhoveConfInputStream = JhoveUtility.class.getResourceAsStream("jhove.conf");
        File jhoveConfFile = File.createTempFile("jhove", "conf");
        FileOutputStream jhoveConfOutputStream = new FileOutputStream(jhoveConfFile);
        IOUtils.copy(jhoveConfInputStream, jhoveConfOutputStream);
        jhoveConfInputStream.close();
        jhoveConfOutputStream.close();
        logger.debug("JHOVE configuration file " + jhoveConfFile);
        return jhoveConfFile;
    }
}
