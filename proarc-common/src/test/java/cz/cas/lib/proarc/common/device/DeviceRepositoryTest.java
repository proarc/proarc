/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.device;

import cz.cas.lib.proarc.common.fedora.FedoraTestSupport;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import java.util.List;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class DeviceRepositoryTest {

    private FedoraTestSupport fedora;
    private RemoteStorage storage;
    private DeviceRepository repository;

    public DeviceRepositoryTest() {
    }

    @Before
    public void setUp() {
        fedora = new FedoraTestSupport();
        storage = fedora.getRemoteStorage();
        repository = new DeviceRepository(storage);
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testFindAll() throws Exception {
        Device expected = addTestDevice("testFindAll");
        List<Device> result = repository.find(null);

        String dump = String.valueOf(result);
        assertFalse(dump, result.isEmpty());
        int matches = 0;
        // repo might contain other devices
        for (Device device : result) {
            if (expected.getId().equals(device.getId())) {
                ++matches;
            }
        }
        assertEquals(dump, 1, matches);
    }

    @Test
    public void testFind() throws Exception {
        Device expected = addTestDevice("testFind");
        List<Device> result = repository.find(expected.getId());
        assertDeviceEquals(expected, result);
    }

    @Test
    public void testAddDevice() throws Exception {
//        fedora.cleanUp();
        String owner = "junit";
        String log = "testAddDevice_log";
        Device expected = new Device();
        expected.setLabel("testAddDevice");
        Device result = repository.addDevice(owner, expected.getLabel(), log);

        // test add result
        expected.setId(result.getId());
        assertDeviceEquals(expected, result);
        assertNotNull(result.getId());

        // test storage content
        List<Device> found = repository.find(result.getId());
        assertDeviceEquals(expected, found);
    }

    @Test
    public void testDeleteDevice() throws Exception {
        Device delete = addTestDevice("testDeleteDevice");
        assertTrue(repository.deleteDevice(delete.getId(), "testDeleteDevice"));
        List<Device> result = repository.find(delete.getId());
        assertTrue(String.valueOf(result), result.isEmpty());
    }

    @Test
    public void testDeleteUnknownDevice() throws Exception {
        String[] expected = {"device:testDeleteUnknownDevice"};
        try {
            repository.deleteDevice(expected[0], "testDeleteDevice");
            fail("Expected " + DeviceNotFoundException.class.getName());
        } catch (DeviceNotFoundException ex) {
            assertArrayEquals(expected, ex.getIds());
        }
    }

    @Test
    public void testUpdate() throws Exception {
//        fedora.cleanUp();
        Device device = addTestDevice("testUpdate");
        String expectedLabel = "updated label";
        Device update = repository.update(device.getId(), expectedLabel, "testUpdate");

        // test update result
        device.setLabel(expectedLabel);
        assertDeviceEquals(device, update);

        // test storage content
        List<Device> found = repository.find(update.getId());
        assertDeviceEquals(update, found);
    }

    private Device addTestDevice(String label) throws Exception {
        Device result = repository.addDevice("junit", label, "addDevice");
        return result;
    }

    /**
     * Check whether the result contains the expected device and nothing all.
     */
    public static void assertDeviceEquals(Device expected, List<Device> result) {
        assertNotNull(expected);
        String resDump = String.valueOf(result);
        assertNotNull(result);
        assertEquals(resDump, 1, result.size());
        assertDeviceEquals(expected, result.get(0));
    }

    public static void assertDeviceEquals(Device expected, Device result) {
        if (expected == null) {
            assertNull(result);
            return ;
        }
        assertEquals(expected.getId(), result.getId());
        assertEquals(expected.getLabel(), result.getLabel());
    }

}