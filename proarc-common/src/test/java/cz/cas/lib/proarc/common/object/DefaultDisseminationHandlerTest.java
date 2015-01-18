/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.FedoraTestSupport;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import javax.ws.rs.core.Response;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Rule;
import org.junit.rules.TestName;

/**
 *
 * @author Jan Pokorsky
 */
public class DefaultDisseminationHandlerTest {

    @Rule
    public TestName test = new TestName();

    private FedoraTestSupport fedora;

    public DefaultDisseminationHandlerTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
        fedora = new FedoraTestSupport();
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testGetDissemination_RemoteNotFound() throws Exception {
        LocalObject object = new LocalStorage().create();
        String label = test.getMethodName();
        object.setLabel(label);
        RemoteStorage rStorage = fedora.getRemoteStorage();
        rStorage.ingest(object, fedora.getTestUser());
        RemoteObject robject = rStorage.find(object.getPid());
        final DigitalObjectHandler pageObject = new DigitalObjectHandler(robject, null);

        DefaultDisseminationHandler handler = new DefaultDisseminationHandler("unknownDatastreamId", pageObject);
        try {
            Response response = handler.getDissemination(null);
            fail(robject.getPid());
        } catch (DigitalObjectNotFoundException ex) {
            assertEquals(robject.getPid(), ex.getPid());
        }
    }

}