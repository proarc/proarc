/*
 * Copyright (C) 2025 Jan Pokorsky
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
package cz.cas.lib.proarc.common.workflow;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.ModsDataHandler;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;

public class WorkflowMetadataHandler {

    private final String modelId;
    private final String pid;
    private final Job parentJob;

    public WorkflowMetadataHandler(String modelId, String pid, Job parentJob) {
        this.modelId = modelId;
        this.pid = pid;
        this.parentJob = parentJob;
    }

    protected static ModsDefinition setRules(ModsDefinition mods) {
        try {
            AppConfiguration config = AppConfigurationFactory.getInstance().defaultInstance();
            StringPlusLanguagePlusAuthority descriptionStandard = new StringPlusLanguagePlusAuthority();
            String rules = config.getRules();
            descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR.equalsIgnoreCase(rules)? ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR : ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA);
            RecordInfoDefinition recordInfo = new RecordInfoDefinition();
            recordInfo.getDescriptionStandard().add(0, descriptionStandard);
            mods.getRecordInfo().add(0, recordInfo);
        } catch (AppConfigurationException ex) {
            ex.printStackTrace();
        }
        return mods;
    }

    public ModsDefinition createDefaultMods() throws DigitalObjectException, AppConfigurationException {
        ModsDataHandler modsDataHandler = new ModsDataHandler(AppConfigurationFactory.getInstance().defaultInstance());
        ModsDefinition defaultMods = modsDataHandler.createDefaultMetadata(pid, modelId, null, parentJob);
        return defaultMods;
    }
}
