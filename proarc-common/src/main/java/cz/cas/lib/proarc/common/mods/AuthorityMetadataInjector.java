/*
 * Copyright (C) 2018 Martin Rumanek
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

package cz.cas.lib.proarc.common.mods;

import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.mods.ModsCollectionDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;

public class AuthorityMetadataInjector implements MetadataInjector {
    MetadataHandler metadataHandler;

    public AuthorityMetadataInjector(MetadataHandler metadataHandler) {
        this.metadataHandler = metadataHandler;
    }

    @Override
    public void addMetadata(DescriptionMetadata<String> authorityJson) throws DigitalObjectException {
        DescriptionMetadata<ModsDefinition> authority = new DescriptionMetadata<>();
        ModsCollectionDefinition authorityCollectionMods = ModsUtils.unmarshal(authorityJson.getData(),
                ModsCollectionDefinition.class);
        authorityCollectionMods.getMods();
        authority.setData(authorityCollectionMods.getMods().get(0));
        DescriptionMetadata<ModsDefinition> metadata = metadataHandler.getMetadata();
        metadata = insertAuthority(metadata, authority);
        metadataHandler.setMetadata(metadata, "authority added", "update");
    }

    private DescriptionMetadata<ModsDefinition> insertAuthority(DescriptionMetadata<ModsDefinition> metadata, DescriptionMetadata<ModsDefinition> authority) {
        if (metadata != null && metadata.getData() != null) {
            metadata = insertName(metadata, authority);
            metadata = insertSubject(metadata, authority);
        }
        return metadata;
    }

    private DescriptionMetadata<ModsDefinition> insertSubject(DescriptionMetadata<ModsDefinition> metadata, DescriptionMetadata<ModsDefinition> authority) {
        if (authority != null && authority.getData() != null && authority.getData().getSubject() != null && authority.getData().getSubject().size() > 0) {
            metadata.getData().getSubject().addAll(authority.getData().getSubject());
        }
        return metadata;
    }

    private DescriptionMetadata<ModsDefinition> insertName(DescriptionMetadata<ModsDefinition> metadata, DescriptionMetadata<ModsDefinition> authority) {
        if (authority != null && authority.getData() != null && authority.getData().getName() != null && authority.getData().getName().size() > 0) {
            for (NameDefinition name : authority.getData().getName()) {
                name.setUsage(null);
            }
            metadata.getData().getName().addAll(authority.getData().getName());
        }
        return metadata;
    }
}
