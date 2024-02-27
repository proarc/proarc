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

import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.mods.custom.NameMapperTest;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.io.IOUtils;
import org.junit.Test;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class AuthorityMetadataInjectorTest {

    @Mocked
    MetadataHandler metadataHandler;


    @Test
    public void addMetadataTest() throws DigitalObjectException, IOException {
        DescriptionMetadata<ModsDefinition> data = new DescriptionMetadata<>();
        ModsDefinition mods = ModsUtils.unmarshal(IOUtils.toString(
                NameMapperTest.class.getResource("monograph_mods.xml"), StandardCharsets.UTF_8),
                ModsDefinition.class);
        data.setData(mods);





        new Expectations() {{
            //metadataHandler.setMetadataAsXml((DescriptionMetadata<String>)any, anyString);
            metadataHandler.getMetadata(); result=data;
        }};



        MetadataInjector metadataInjector = new AuthorityMetadataInjector(metadataHandler);

        DescriptionMetadata<String> authority = new DescriptionMetadata<>();
        authority.setData(IOUtils.toString(
                NameMapperTest.class.getResource("authority_author.xml"), StandardCharsets.UTF_8));


        metadataInjector.addMetadata(authority);

        new Verifications() {{
            DescriptionMetadata<ModsDefinition> metadata;
            metadataHandler.setMetadata(metadata = withCapture(), anyString, NdkMetadataHandler.OPERATION_UPDATE);
            minTimes=0;
            assertNotNull(metadata.getData());
            ModsDefinition mods = metadata.getData();
            assertEquals("", 6, mods.getName().size());

            List<String> listNames = mods.getName().stream()
                    .flatMap(nameDefinition -> nameDefinition.getNamePart().stream()).map(namePartDefinition -> namePartDefinition.getValue())
                    .collect(Collectors.toList());
            assertTrue(listNames.contains("Hásek"));
        }};
    }
}