/*
 * Copyright (C) 2026 Lukas Sykora
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
package cz.cas.lib.proarc.common.externalApp.ltpCesnet;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import java.io.File;
import java.io.IOException;
import org.json.JSONException;
import org.json.JSONObject;

/** Builds archive metadata and uploads a completed archive to CESNET. */
public final class CesnetUploader {
    private final AppConfiguration appConfiguration;
    private final Integer credentialOwnerId;

    public CesnetUploader(AppConfiguration appConfiguration) {
        this(appConfiguration, null);
    }

    public CesnetUploader(AppConfiguration appConfiguration, Integer credentialOwnerId) {
        this.appConfiguration = appConfiguration;
        this.credentialOwnerId = credentialOwnerId;
    }

    public void upload(File archive, String operationId, String pid) throws IOException, DigitalObjectException, InterruptedException {
        upload(archive, operationId, pid, false);
    }

    public void upload(File archive, String operationId, String pid, boolean confirmed) throws IOException, DigitalObjectException, InterruptedException {
        if (archive != null && archive.exists()) {
            CesnetTokenStore.getInstance().validate(appConfiguration, credentialOwnerId, null);
            String metadata = createMetadataJson(archive.getName(), pid);
            if (!isValid(metadata)) {
                throw new IOException("Invalid json:" + metadata);
            }
            var options = appConfiguration.getLtpCesnetConfiguration();
            var record = new cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo();
            record.setProvider(LtpCesnetClient.ID);
            record.setEndpoint(options.getString("apiUrl", "https://ltp.cesnet.cz/api").replaceAll("/+$", ""));
            record.setGroup(options.getString("groupToken"));
            record.setCatalogId(options.getString("catalogId"));
            record.setCredentialOwnerId(credentialOwnerId);
            record.setOperationId(operationId);
            cz.cas.lib.proarc.common.externalApp.ltp.PreservationService.cesnet(appConfiguration, credentialOwnerId)
                    .submit(pid, archive, new JSONObject(metadata), record, confirmed);
        } else {
            throw new IOException("CESNET archive does not exist: " + archive);
        }
    }

    private boolean isValid(String json) {
        try {
            new JSONObject(json);
        } catch (JSONException ex) {
            return false;
        }
        return true;
    }

    private String toPid(String folderName) {
        return folderName;
    }

    private String createMetadataJson(String folderName, String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fo = dom.find(pid, null);
        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
        NdkMapper.Context context = new NdkMapper.Context(handler);
        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();


        String urnNbn = getUrnNbn(mods);
        String title = getTitle(mods);

        return createJson(pid, folderName, urnNbn, title);
    }

    private String getTitle(ModsDefinition mods) {
        StringBuilder builder = new StringBuilder();
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            if (titleInfo.getTitle().size() > 0) {
                builder.append(titleInfo.getTitle().get(0).getValue());
            }
            if (titleInfo.getSubTitle().size() > 0) {
                builder.append(": ");
                builder.append(titleInfo.getSubTitle().get(0).getValue());
            }
            if (titleInfo.getPartName().size() > 0) {
                builder.append(" - ");
                builder.append(titleInfo.getPartName().get(0).getValue());
            }
            if (titleInfo.getPartNumber().size() > 0) {
                builder.append(" / ");
                builder.append(titleInfo.getPartNumber().get(0).getValue());
            }
        }
        return builder.toString();
    }

    private String getUrnNbn(ModsDefinition mods) {
        for (IdentifierDefinition identifier : mods.getIdentifier()) {
            if ("urnnbn".equals(identifier.getType()) && identifier.getInvalid() == null) {
                return identifier.getValue();
            }
        }
        return null;
    }

    private String createJson(String pid, String folderName, String urnNbn, String title) {
        JSONObject metadata = new JSONObject();
        if (folderName != null && !folderName.isEmpty()) metadata.put("folder", folderName);
        if (urnNbn != null && !urnNbn.isEmpty()) metadata.put("urn:nbn", urnNbn);
        if (title != null && !title.isEmpty()) metadata.put("title", title);
        JSONObject archive = new JSONObject().put("name", pid);
        if (!metadata.isEmpty()) archive.put("user_metadata", metadata);
        return archive.toString();
    }
}
