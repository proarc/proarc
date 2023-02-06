/*
 * Copyright (C) 2023 Lukas Sykora
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
package cz.cas.lib.proarc.common.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.common.kramerius.KUtils.findHandler;
import static cz.cas.lib.proarc.common.kramerius.KUtils.getExportFile;
import static cz.cas.lib.proarc.common.kramerius.KUtils.getFile;

public class KDataHandler {

    private static final Logger LOG = Logger.getLogger(KDataHandler.class.getName());

    private final AppConfiguration appConfig;

    public KDataHandler(AppConfiguration appConfig) {
        this.appConfig = appConfig;
    }

    public DescriptionMetadata<String> getDescriptionMetadata(String pid, String instanceId) throws DigitalObjectException {
        DigitalObjectHandler handler = findHandler(pid, instanceId);
        if (handler != null) {
            MetadataHandler<?> metadataHandler = handler.metadata();
            if (metadataHandler != null) {
                DescriptionMetadata<String> metadataAsXml = metadataHandler.getMetadataAsXml();
                return metadataAsXml;
            }
        }
        return null;
    }

    public boolean setDescriptionMetadataToProArc(String pid, DescriptionMetadata<String> metadata, String instanceId) throws DigitalObjectException {
        DigitalObjectHandler handler = findHandler(pid, null);
        if (handler == null) {
            return false;
        }
        MetadataHandler<?> mHandler = handler.metadata();
        if (mHandler == null) {
            return false;
        }
        DescriptionMetadata<String> descriptionMetadata = new DescriptionMetadata<>();
        descriptionMetadata.setPid(pid);
        descriptionMetadata.setKrameriusInstanceId(instanceId);
        descriptionMetadata.setTimestamp(mHandler.getMetadataAsXml().getTimestamp());
        descriptionMetadata.setData(metadata.getData());

        descriptionMetadata.setIgnoreValidation(true);
        mHandler.setMetadataAsXml(descriptionMetadata, "Update Metadata from editK7 foxml.", NdkMetadataHandler.OPERATION_UPDATE);
        handler.commit();

        return true;
    }

    public File getSourceFile(String pid, String krameriusInstanceId) throws IOException {
        File pidFoxml = getFile(appConfig, krameriusInstanceId, pid);
        return pidFoxml;
    }

    public File getDestinationFile(String pid, KrameriusOptions.KrameriusInstance instance) throws IOException {
        File pidFoxml = getExportFile(appConfig, instance, pid);
        if (!pidFoxml.createNewFile()) {
            LOG.severe("Can not create file " + pidFoxml.getAbsolutePath());
            throw new IOException("Can not create file " + pidFoxml.getAbsolutePath());
        }
        return pidFoxml;
    }
}
