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

import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.FedoraTestSupport;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.process.imports.TiffImporterTest;
import java.util.logging.Logger;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assume;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

/**
 *
 * @author Jan Pokorsky
 */
public class DefaultDisseminationHandlerTest {

    private static final Logger LOG = Logger.getLogger(DefaultDisseminationHandlerTest.class.getName());
    private static RemoteObject robject;
    private static FedoraTestSupport fedora;

    @Rule
    public TestName test = new TestName();

    public DefaultDisseminationHandlerTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
        fedora = new FedoraTestSupport();
        fedora.cleanUp();
        LocalObject object = new LocalStorage().create();
        object.setLabel(DefaultDisseminationHandlerTest.class.getSimpleName());
        FedoraStorage rStorage = fedora.getRemoteStorage();
        rStorage.ingest(object, fedora.getTestUser());
        robject = rStorage.find(object.getPid());
        LOG.info(robject.getPid());
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testGetDissemination_RemoteNotFound() throws Exception {
        final DigitalObjectHandler pageObject = new DigitalObjectHandler(robject, null);

        DefaultDisseminationHandler handler = new DefaultDisseminationHandler("unknownDatastreamId", pageObject);
        try {
            Response response = handler.getDissemination(null);
            fail(robject.getPid());
        } catch (DigitalObjectNotFoundException ex) {
            assertEquals(robject.getPid(), ex.getPid());
        }
    }

    @Test
    public void testSetIconAsDissemination() throws Exception {
        final MediaType mime = new MediaType("application", "pdf");
        final String dsId = BinaryEditor.PREVIEW_ID;
        assumeIcon(mime, dsId);
        final DigitalObjectHandler pageObject = new DigitalObjectHandler(robject, null);
        DefaultDisseminationHandler handler = new DefaultDisseminationHandler(dsId, pageObject);
        String t = test.getMethodName();
//        fedora.getClient().debug(true);
        handler.setIconAsDissemination(dsId, mime, BinaryEditor.PREVIEW_LABEL, Storage.FEDORA, t);
        pageObject.commit();

        Response response = handler.getDissemination(null);
        assertNotNull(robject.getPid(), response);
        assertEquals(robject.getPid(), Status.OK.getStatusCode(), response.getStatus());
    }

    @Test
    public void testSetIconAsDissemination_Update() throws Exception {
//        fedora.getClient().debug(true);
        String testName = test.getMethodName();
        final MediaType mime = new MediaType("application", "pdf");
        final String dsId = BinaryEditor.RAW_ID;
        assumeIcon(mime, dsId);
        BinaryEditor beditor = BinaryEditor.dissemination(robject, dsId, mime);
        beditor.write(TiffImporterTest.class.getResource("pdfa_test.pdf").toURI(), beditor.getLastModified(), testName);
        robject.flush();

        final DigitalObjectHandler pageObject = new DigitalObjectHandler(robject, null);
        DefaultDisseminationHandler handler = new DefaultDisseminationHandler(dsId, pageObject);

        handler.setIconAsDissemination(mime, BinaryEditor.PREVIEW_LABEL, Storage.FEDORA, testName);
        pageObject.commit();

        Response response = handler.getDissemination(null);
        System.out.println(response.getMetadata());
        assertNotNull(robject.getPid(), response);
        assertEquals(robject.getPid(), Status.OK.getStatusCode(), response.getStatus());
        assertEquals(response.getMetadata().toString(), BinaryEditor.IMAGE_JPEG,
                response.getMetadata().getFirst("Content-Type"));
    }

    /**
     * Runs test just in case an icon for given MIME and stream ID exists
     */
    private void assumeIcon(final MediaType mime, final String dsId) throws DigitalObjectException {
        RemoteObject icon = fedora.getRemoteStorage().find(DefaultDisseminationHandler.mime2iconPid(mime));
        try {
            DatastreamProfile iconDs = DefaultDisseminationHandler.findProfile(dsId, icon.getDatastreams());
            if (iconDs != null) {
                return ;
            }
            DatastreamProfile iconDefaultDs = DefaultDisseminationHandler.findProfile(
                    BinaryEditor.THUMB_ID, icon.getDatastreams());
            Assume.assumeNotNull(iconDefaultDs);
        } catch (DigitalObjectNotFoundException ex) {
            Assume.assumeNoException(ex);
        }
    }

}
