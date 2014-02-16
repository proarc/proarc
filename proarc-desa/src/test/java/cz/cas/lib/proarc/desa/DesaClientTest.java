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
package cz.cas.lib.proarc.desa;

import cz.cas.lib.proarc.desa.nomenclature.Nomenclatures;
import cz.cas.lib.proarc.desa.soap.FileHashAlg;
import java.io.File;
import java.util.Arrays;
import java.util.List;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Assume;

/**
 *
 * @author Jan Pokorsky
 */
public class DesaClientTest {

    private DesaClient client;

    public DesaClientTest() {
    }

    @Before
    public void setUp() {
        String soapUrl = System.getProperty("proarc-desa.DesaClient.soapUrl");
        String restUrl = System.getProperty("proarc-desa.DesaClient.restUrl");
        String user = System.getProperty("proarc-desa.DesaClient.user");
        String passwd = System.getProperty("proarc-desa.DesaClient.passwd");
        Assume.assumeNotNull(soapUrl, restUrl, user, passwd);
        client = new DesaClient(soapUrl, restUrl, user, passwd);
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testGetNomenclatures() {
        String producerCode = "PROARC";
        List<String> nomenclatureAcronyms = Arrays.asList("RecCl");
        Nomenclatures result = client.getNomenclatures(producerCode, nomenclatureAcronyms);
        assertNotNull(result);
    }

//    @Test
    public void testGetNomenclaturesSource() throws Exception {
        String producerCode = "PROARC";
        List<String> nomenclatureAcronyms = Arrays.asList("RecCl");
        Source result = client.getNomenclaturesSource(producerCode, nomenclatureAcronyms);
        assertNotNull(result);
        TransformerFactory.newInstance().newTransformer().transform(result, new StreamResult(System.out));
    }

//    @Test
    public void testSubmitPackage() {
        System.out.println("submitPackage");
        File file = null;
        String operator = "";
        String producerCode = "PROARC";
        String producerSipId = "";
        FileHashAlg fileHashAlg = FileHashAlg.MD_5;
        String fileHash = "";
        String lang = "";
        String expResult = "";
        String result = client.submitPackage(file, operator, producerCode, producerSipId, fileHashAlg, fileHash, lang);
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

  }