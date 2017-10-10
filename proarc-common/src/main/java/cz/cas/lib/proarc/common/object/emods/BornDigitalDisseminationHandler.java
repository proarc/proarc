/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.object.emods;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.imports.InputUtils;
import cz.cas.lib.proarc.common.object.DefaultDisseminationHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DisseminationHandler;
import cz.cas.lib.proarc.common.object.DisseminationInput;
import cz.cas.lib.proarc.common.process.GenericExternalProcess;
import java.io.File;
import java.io.IOException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import org.apache.commons.configuration.Configuration;

/**
 * Processes uploaded contents (PDF), validates, creates thumbnail, preview and others.
 *
 * @author Jan Pokorsky
 */
public class BornDigitalDisseminationHandler implements DisseminationHandler {

    private final String dsId;
    private final DigitalObjectHandler objHandler;
    private final DefaultDisseminationHandler ddh;

    public BornDigitalDisseminationHandler(DefaultDisseminationHandler ddh) {
        this.dsId = ddh.getDsId();
        this.objHandler = ddh.getHandler();
        this.ddh = ddh;
    }

    @Override
    public Response getDissemination(Request httpRequest) throws DigitalObjectException {
        return ddh.getDissemination(httpRequest);
    }

    @Override
    public void setDissemination(DisseminationInput input, String message) throws DigitalObjectException {
//        MediaType mime = input.getMime();
//        if (!"application".equalsIgnoreCase(mime.getType()) || !"pdf".equalsIgnoreCase(mime.getSubtype())) {
//            throw new DigitalObjectException(handler.getFedoraObject().getPid(), null, dsId,
//                    "Unsupported MIME " + mime, null);
//        }
        File inputFile = input.getFile();
        if (!isPdf(inputFile)) {
            throw new DigitalObjectException(objHandler.getFedoraObject().getPid(), null, dsId,
                    "Not PDF content " + inputFile + ", exists: " + inputFile.exists() + ", size: " + inputFile.length(), null);
        }
        MediaType mime = new MediaType("application", "pdf");
        if (BinaryEditor.RAW_ID.equals(dsId)) {
            // XXX use importer
            ddh.setRawDissemination(inputFile, input.getFilename(), mime, message);
            ddh.setIconAsDissemination(BinaryEditor.PREVIEW_ID, mime, BinaryEditor.PREVIEW_LABEL, message);
            createThumbnail(inputFile, message);
        } else {
            throw new DigitalObjectException(objHandler.getFedoraObject().getPid(), null, dsId,
                    "Unsupported datastream ID!", null);
        }
    }

    @Override
    public void deleteDissemination(String message) throws DigitalObjectException {
        objHandler.getFedoraObject().purgeDatastream(BinaryEditor.RAW_ID, message);
        objHandler.getFedoraObject().purgeDatastream(BinaryEditor.PREVIEW_ID, message);
        objHandler.getFedoraObject().purgeDatastream(BinaryEditor.THUMB_ID, message);
    }

    private void createThumbnail(File inputFile, String message) throws DigitalObjectException {
        Configuration thumbConf = getConfig().getImportConfiguration().getThumbnailProcessor();
        if (thumbConf != null && !thumbConf.isEmpty()) {
            GenericExternalProcess thumbProc = new GenericExternalProcess(thumbConf)
                    .addInputFile(inputFile)
                    .addOutputFile(new File(inputFile.getAbsolutePath() + ".jpg"));
            thumbProc.run();
            if (thumbProc.isOk()) {
                ddh.setDsDissemination(BinaryEditor.THUMB_ID, thumbProc.getOutputFile(),
                        BinaryEditor.THUMB_LABEL, BinaryEditor.IMAGE_JPEG, message);
            }
        }
    }

    private AppConfiguration getConfig() throws DigitalObjectException {
        try {
            return AppConfigurationFactory.getInstance().defaultInstance();
        } catch (AppConfigurationException ex) {
            throw new DigitalObjectException(objHandler.getFedoraObject().getPid(), null, dsId,
                    "Broken configuration! ", ex);
        }
    }

    private boolean isPdf(File f) throws DigitalObjectException {
        try {
            return InputUtils.isPdf(f);
        } catch (IOException ex) {
            throw new DigitalObjectException(objHandler.getFedoraObject().getPid(),
                    null, dsId, f.toString(), ex);
        }
    }

}
