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
package cz.cas.lib.proarc.common.export.archive;

import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import java.util.List;

/**
 * It selects digital object to archive oldprint.
 *
 * @author Lukas Sykora
 */
public class ArchiveOldPrintObjectSelector extends ArchiveObjectSelector {


    public ArchiveOldPrintObjectSelector(DigitalObjectCrawler crawler) {
        super(crawler);
    }

    @Override
    protected void searchPath(List<DigitalObjectElement> entryPath) throws DigitalObjectException {
        DigitalObjectElement entry = entryPath.get(0);
        if (processedPids.contains(entry.getPid())) {
            return ;
        }
        processedPids.add(entry.getPid());
        String modelId = entry.getModelId();
        if (OldPrintPlugin.MODEL_VOLUME.equals(modelId)
                || OldPrintPlugin.MODEL_CARTOGRAPHIC.equals(modelId)
                || OldPrintPlugin.MODEL_SHEETMUSIC.equals(modelId)
                || OldPrintPlugin.MODEL_GRAPHICS.equals(modelId)
                || OldPrintPlugin.MODEL_SUPPLEMENT.equals(modelId)) {
            addSelection(entryPath);
        } else if (OldPrintPlugin.MODEL_MONOGRAPHTITLE.equals(modelId)
                || OldPrintPlugin.MODEL_OMNIBUSVOLUME.equals(modelId)) {
            searchChildren(entry, entryPath);
        } else if (OldPrintPlugin.MODEL_CHAPTER.equals(modelId)
                || OldPrintPlugin.MODEL_PAGE.equals(modelId)) {
            if (entryPath.size() == 1) {
                throw new DigitalObjectException(entry.getPid(), "Unexpected hierarchy: " + entryPath);
            } else {
                searchPath(entryPath.subList(1, entryPath.size()));
            }
        } else {
            throw new DigitalObjectException(entry.getPid(), "Unexpected model: " + entry);
        }
    }
}
