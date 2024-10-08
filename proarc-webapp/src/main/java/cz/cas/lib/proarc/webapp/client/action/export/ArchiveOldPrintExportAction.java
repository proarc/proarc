/*
 * Copyright (C) 2021 Lukas Sykora
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

package cz.cas.lib.proarc.webapp.client.action.export;

import com.smartgwt.client.data.Record;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;

/**
 * The Archive OldPrint export action.
 *
 * @author Lukas Sykora
 */
public class ArchiveOldPrintExportAction extends ArchiveExportAction {

    public ArchiveOldPrintExportAction (ClientMessages i18n){
        super(i18n, i18n.ArchiveOldPrintExportAction_Title(), null, i18n.ArchiveOldPrintExportAction_Hint());
    }

    @Override
    protected boolean isAcceptableModel(String modelId) {
        return modelId != null && isOldPrintModel(modelId);
    }

    protected boolean isOldPrintModel(String modelId) {
        if (modelId.equals(OldPrintPlugin.MODEL_MONOGRAPHTITLE) ||
                modelId.equals(OldPrintPlugin.MODEL_MONOGRAPHUNIT) ||
                modelId.equals(OldPrintPlugin.MODEL_MONOGRAPHVOLUME) ||
                modelId.equals(OldPrintPlugin.MODEL_SUPPLEMENT) ||
                modelId.equals(OldPrintPlugin.MODEL_CHAPTER) ||
                modelId.equals(OldPrintPlugin.MODEL_SHEETMUSIC) ||
                modelId.equals(OldPrintPlugin.MODEL_CARTOGRAPHIC) ||
                modelId.equals(OldPrintPlugin.MODEL_GRAPHICS)) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected void setAttributes(Record export, String[] pids) {
        export.setAttribute(ExportResourceApi.ARCHIVE_PID_PARAM, pids);
        export.setAttribute(ExportResourceApi.NDK_PACKAGE, ExportResourceApi.Package.STT.name());
    }
}
