/*
 * Copyright (C) 2018 Martin Rumanek
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cas.lib.proarc.common.export.sip;

import java.util.Collections;

import com.yourmediashelf.fedora.client.FedoraClient;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.export.ExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.NdkExport;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.Mockito;



public class NdkSipExportTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void export() throws AppConfigurationException, ExportException {
        AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        RemoteStorage remoteStorage = Mockito.mock(RemoteStorage.class);
        RemoteStorage.RemoteObject object = Mockito.mock(RemoteStorage.RemoteObject.class);

        FedoraClient client = Mockito.mock(FedoraClient.class);

        Mockito.when(object.getClient()).thenReturn(client);
        Mockito.when(object.getPid()).thenReturn("uuid:ebfd7bf2-169d-476e-a230-0cc39f01764c");


        Mockito.when(remoteStorage.find("uuid:ebfd7bf2-169d-476e-a230-0cc39f01764c")).thenReturn(object);

        NdkExport export = new NdkSipExport(remoteStorage, appConfig.getNdkExportOptions(), new MetsUtils() {


        });

        export.export(folder.getRoot(), Collections.singletonList("uuid:ebfd7bf2-169d-476e-a230-0cc39f01764c"),
        true, true, null);
    }
}