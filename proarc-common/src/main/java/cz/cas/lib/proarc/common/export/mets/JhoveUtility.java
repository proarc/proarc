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
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.apache.commons.io.IOUtils;
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

    private static final Logger LOG = Logger.getLogger(JhoveUtility.class.getName());

    /**
     * Returns a node with MIX info - helper
     *
     * @param node
     * @return
     */
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

    /**
     *
     * Inits the Jhove app
     *
     * @param metsInfo
     */
    public static void initJhove(MetsInfo metsInfo) throws MetsExportException {
        Calendar calendar = Calendar.getInstance();

        App app = new App(JhoveUtility.class.getSimpleName(), "1.0", new int[] { calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH), calendar.get(Calendar.DAY_OF_MONTH) }, "jHove", "");
        try {

            JhoveBase jhoveBase = new JhoveBase();
            File jhoveConfigFile = createJhoveConfigurationFile();
            jhoveBase.init(jhoveConfigFile.getAbsolutePath(), null);
            metsInfo.jhoveBase = jhoveBase;
            metsInfo.jhoveApp = app;
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Error while initialising jHove", ex);
            metsInfo.metsExportException.addException("Error while initialising jHove", false, ex);
            throw metsInfo.metsExportException;
        }
    }

    /**
     *
     * Returns a MIX node
     *
     * @param targetFile
     * @param metsinfo
     * @return
     */
    public static Node getMixNode(File targetFile, MetsInfo metsinfo) throws MetsExportException {
        if (targetFile == null || !targetFile.isFile() || !targetFile.exists()) {
            LOG.log(Level.SEVERE, "target file '" + targetFile + "' cannot be found.");
            throw new MetsExportException("target file '" + targetFile + "' cannot be found.", false, null);
        }
        if (metsinfo.jhoveBase == null) {
            initJhove(metsinfo);
        }
        try {
            File outputFile = File.createTempFile("jhove", "output");
            LOG.log(Level.FINE, "JHOVE output file " + outputFile);
            Module module = metsinfo.jhoveBase.getModule(null);
            OutputHandler aboutHandler = metsinfo.jhoveBase.getHandler(null);
            OutputHandler xmlHandler = metsinfo.jhoveBase.getHandler("XML");
            LOG.log(Level.FINE, "Calling JHOVE dispatch(...) on file " + targetFile);
            metsinfo.jhoveBase.dispatch(metsinfo.jhoveApp, module, aboutHandler, xmlHandler, outputFile.getAbsolutePath(), new String[] { targetFile.getAbsolutePath() });
            DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = builderFactory.newDocumentBuilder();
            Document jHoveDoc = builder.parse(outputFile);
            outputFile.delete();
            return getMixRecursive(jHoveDoc);
        } catch (Exception e) {
            LOG.log(Level.SEVERE, "Error inspecting file '" + targetFile + "' - " + e.getMessage(), e);
            metsinfo.metsExportException.addException("Error inspecting file '" + targetFile + "' - " + e.getMessage(), true, e);
        }
        return null;
    }

    /**
     * Copy the Jhove configuration file to a temporary file.
     *
     * @return the {@link File} where the Jhove configuration was saved.
     *
     */

    private static File createJhoveConfigurationFile() throws MetsExportException {
        InputStream jhoveConfInputStream = JhoveUtility.class.getResourceAsStream("jhove.conf");
        InputStream jhoveConfXSDInputStream = JhoveUtility.class.getResourceAsStream("jhoveConfig.xsd");
        try {
            File jhoveConfFile = File.createTempFile("jhove", "conf");
            LOG.log(Level.FINE, "JHOVE configuration file " + jhoveConfFile);
            FileOutputStream jhoveConfOutputStream = new FileOutputStream(jhoveConfFile);
            IOUtils.copy(jhoveConfInputStream, jhoveConfOutputStream);
            File xsdFile = new File(jhoveConfFile.getParent() + File.separator + "jhoveConfig.xsd");
            if (!xsdFile.exists()) {
                FileOutputStream jhoveXSDOutputStream = new FileOutputStream(xsdFile);
                IOUtils.copy(jhoveConfXSDInputStream, jhoveXSDOutputStream);
                jhoveConfXSDInputStream.close();
                jhoveXSDOutputStream.close();
            }
            jhoveConfInputStream.close();
            jhoveConfOutputStream.close();
            return jhoveConfFile;
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, "Unable to create jHove config file", ex);
            throw new MetsExportException("Unable to create jHove config file", false, ex);
        }
    }
}