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
package cz.cas.lib.proarc.nsesss2;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class NsesssUtilsTest {

    public NsesssUtilsTest() {
    }

    @Test
    public void testDefaultJaxbContext() throws Exception {
        JAXBContext result = NsesssUtils.defaultJaxbContext();
        assertNotNull(result);
    }

    @Test
    public void testDefaultMarshaller() throws Exception {
        Marshaller result = NsesssUtils.defaultMarshaller(true);
        assertNotNull(result);
    }

    @Test
    public void testDefaultUnmarshaller() throws Exception {
        Unmarshaller result = NsesssUtils.defaultUnmarshaller();
        assertNotNull(result);
    }

    @Test
    public void testSpisToXml() {
        Spis object = new Spis();
        String result = NsesssUtils.toXml(object, true);
        System.out.println(result);
        assertNotNull(result);
    }

    @Test
    public void testDefaultSpis() {
        Spis spis = NsesssUtils.defaultSpis();
        assertNotNull(spis);
        assertEquals("ERMS", spis.getEvidencniUdaje().getEvidence().getNazevEvidenceDokumentu());
        assertEquals(TZpusobVyrizeni.VYŘÍZENÍ_DOKUMENTEM, spis.getEvidencniUdaje().getVyrizeniUzavreni().getZpusob());
        assertEquals("Vyplývá ze spisového plánu organizace",
                spis.getEvidencniUdaje().getVyrazovani().getSkartacniRezim().getOduvodneni());
        assertEquals("Uzavření spisu",
                spis.getEvidencniUdaje().getVyrazovani().getSkartacniRezim().getSpousteciUdalost());
        assertEquals(TLogicky.ANO, spis.getEvidencniUdaje().getManipulace().getAnalogovyDokument().ANO);
//        System.out.println(NsesssUtils.toXml(spis, true));
    }

    @Test
    public void testDefaultInternalDokument() {
        Dokument result = NsesssUtils.defaultInternalDokument();
        assertNotNull(result);
        assertEquals("ERMS", result.getEvidencniUdaje().getEvidence().getNazevEvidenceDokumentu());
        assertEquals("Vyplývá ze spisového plánu organizace",
                result.getEvidencniUdaje().getVyrazovani().getSkartacniRezim().getOduvodneni());
        assertEquals("Uzavření spisu",
                result.getEvidencniUdaje().getVyrazovani().getSkartacniRezim().getSpousteciUdalost());
        assertEquals(TLogicky.ANO, result.getEvidencniUdaje().getManipulace().getAnalogovyDokument().ANO);
        System.out.println(NsesssUtils.toXml(result, true));
    }

}