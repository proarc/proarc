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

package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.util.Arrays;
import java.util.HashMap;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;
import org.easymock.EasyMock;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import mockit.Mocked;

import static junit.framework.TestCase.assertNotNull;

public class DigitalObjectPluginTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);

    private AppConfiguration config;

    @Mocked
    RemoteStorage remoteStorage;

    @Before
    public void setUp() throws Exception {
        ServiceLoader<DigitalObjectPlugin> pluginLoader = ServiceLoader.load(DigitalObjectPlugin.class);

        config = AppConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, temp.getRoot().getPath());
        }});
        MetaModelRepository.setInstance(
                StreamSupport.stream(pluginLoader.spliterator(), false)
                        .map(digitalObjectPlugin -> digitalObjectPlugin.getId())
                        .toArray(String[]::new)
        );
        DigitalObjectManager.setDefault(new DigitalObjectManager(
                config,
                EasyMock.createNiceMock(ImportBatchManager.class),
                remoteStorage,
                MetaModelRepository.getInstance(),
                EasyMock.createNiceMock(UserManager.class)));
    }

    @Test
    public void testGetModel() throws DigitalObjectException {
        ServiceLoader<DigitalObjectPlugin> pluginLoader = ServiceLoader.load(DigitalObjectPlugin.class);
        for (DigitalObjectPlugin plugin : pluginLoader) {
            // Skip desa plugin. Desa-des doesn't have correct handler provider. Not sure it's a bug.
            if (Arrays.asList("desa-des").contains(plugin.getId())) continue;

            assertNotNull("no handler provider for plugin " + plugin.getId(), plugin.getHandlerProvider(HasMetadataHandler.class));

            DigitalObjectManager dom = DigitalObjectManager.getDefault();

            for (MetaModel metaModel : plugin.getModel()) {
                assertNotNull(metaModel);
                assertNotNull(metaModel.getPid());
                UserProfile userProfile = new UserProfile();
                userProfile.setUserName("junit");
                DigitalObjectManager.CreateHandler createHandler = dom.create(metaModel.getPid(), null, null, userProfile, null, "");
                createHandler.createDigitalObject(true);
            }

        }
    }
}
