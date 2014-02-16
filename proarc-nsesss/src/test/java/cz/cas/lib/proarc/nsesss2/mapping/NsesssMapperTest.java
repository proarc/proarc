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
package cz.cas.lib.proarc.nsesss2.mapping;

import cz.cas.lib.proarc.nsesss2.mapping.NsesssMapper;
import cz.cas.lib.proarc.nsesss2.NsesssConstants;
import cz.cas.lib.proarc.nsesss2.Spis;
import cz.cas.lib.proarc.nsesss2.TDatum;
import cz.cas.lib.proarc.nsesss2.TEvidencniUdajeSpisu;
import cz.cas.lib.proarc.nsesss2.TIdentifikator;
import cz.cas.lib.proarc.nsesss2.TManipulaceSeskupeni;
import cz.cas.lib.proarc.nsesss2.TOsobyExterni;
import cz.cas.lib.proarc.nsesss2.TSkartacniRezim;
import cz.cas.lib.proarc.nsesss2.TSubjektExterni;
import cz.cas.lib.proarc.nsesss2.mapping.NsesssMapper.SubjektExterni;
import java.util.List;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class NsesssMapperTest {

    public NsesssMapperTest() {
    }

    @Test
    public void testToJson() {
        TOsobyExterni osobyExterni = new TOsobyExterni();
        List<TSubjektExterni> subjects = osobyExterni.getSubjekt();

        TSubjektExterni pravnickaOsoba = new TSubjektExterni();
        pravnickaOsoba.setIdentifikatorOrganizace(createTIdentifikator("po_io_value", "po_io_zdroj"));
        pravnickaOsoba.setNazevOrganizace("po_NazevOrganizace");
        pravnickaOsoba.setIdentifikatorFyzickeOsoby(createTIdentifikator("po_ifo_value", "po_ifo_zdroj"));
        pravnickaOsoba.setNazevFyzickeOsoby("po_NazevFyzickeOsoby");
        pravnickaOsoba.setOrganizacniUtvar("po_OrganizacniUtvar");
        pravnickaOsoba.setPracovniPozice("po_PracovniPozice");
        pravnickaOsoba.setSidloOrganizace("po_SidloOrganizace");
        pravnickaOsoba.setElektronickyKontakt("po_email");
        subjects.add(pravnickaOsoba);

        TSubjektExterni fyzickaOsoba = new TSubjektExterni();
        fyzickaOsoba.setIdentifikatorFyzickeOsoby(createTIdentifikator("fo_ifo_value", "fo_ifo_zdroj"));
        fyzickaOsoba.setNazevFyzickeOsoby("fo_NazevFyzickeOsoby");
        fyzickaOsoba.setPostovniAdresa("fo_PostovniAdresa");
        fyzickaOsoba.setElektronickyKontakt("po_email");

        subjects.add(fyzickaOsoba);

        List<TSubjektExterni> input = osobyExterni.getSubjekt();
        assertEquals(2, input.size());
        NsesssMapper instance = new NsesssMapper();
        TOsobyExterni result = instance.replaceTSubjektExterni(osobyExterni);
        List<TSubjektExterni> jsonResult = result.getSubjekt();
        assertEquals(2, jsonResult.size());

        assertEquals(SubjektExterni.class, jsonResult.get(0).getClass());
        SubjektExterni se = (SubjektExterni) jsonResult.get(0);
        assertEquals(NsesssConstants.DOKUMENT_PRIJEMCE_PRAVNICKA_OSOBA, se.getSubjectType());
        assertEquals(pravnickaOsoba.getElektronickyKontakt(), se.getElektronickyKontakt());
        assertEquals(pravnickaOsoba.getIdentifikatorFyzickeOsoby(), se.getIdentifikatorFyzickeOsoby());
        assertEquals(pravnickaOsoba.getIdentifikatorOrganizace(), se.getIdentifikatorOrganizace());
        assertEquals(pravnickaOsoba.getNazevFyzickeOsoby(), se.getNazevFyzickeOsoby());
        assertEquals(pravnickaOsoba.getNazevOrganizace(), se.getNazevOrganizace());
        assertEquals(pravnickaOsoba.getOrganizacniUtvar(), se.getOrganizacniUtvar());
        assertEquals(pravnickaOsoba.getPracovniPozice(), se.getPracovniPozice());
        assertEquals(pravnickaOsoba.getSidloOrganizace(), se.getSidloOrganizace());

        assertEquals(SubjektExterni.class, jsonResult.get(1).getClass());
        se = (SubjektExterni) jsonResult.get(1);
        assertEquals(NsesssConstants.DOKUMENT_PRIJEMCE_FYZICKA_OSOBA, se.getSubjectType());
        assertEquals(fyzickaOsoba.getElektronickyKontakt(), se.getElektronickyKontakt());
        assertEquals(fyzickaOsoba.getIdentifikatorFyzickeOsoby(), se.getIdentifikatorFyzickeOsoby());
        assertEquals(fyzickaOsoba.getNazevFyzickeOsoby(), se.getNazevFyzickeOsoby());
        assertEquals(fyzickaOsoba.getPostovniAdresa(), se.getPostovniAdresa());
    }

    private static TIdentifikator createTIdentifikator(String value, String zdroj) {
        TIdentifikator i = new TIdentifikator();
        i.setValue(value);
        i.setZdroj(zdroj);
        return i;
    }

    @Test
    public void testfillDisposalDate() throws Exception {
        NsesssMapper mapper = new NsesssMapper();
        Spis s = mapper.fillDisposalDate(new Spis());
        assertNull(s.getEvidencniUdaje());

        TEvidencniUdajeSpisu eu = new TEvidencniUdajeSpisu();
        TManipulaceSeskupeni ms = new TManipulaceSeskupeni();
        TDatum datumUzavreni = new TDatum();
        datumUzavreni.setValue(mapper.getXmlTypes().newXMLGregorianCalendar("2012-02-02"));
        ms.setDatumUzavreni(datumUzavreni);
        eu.setManipulace(ms);
        s.setEvidencniUdaje(eu);

        // test fill new dates without period
        mapper.fillDisposalDate(s);
        assertEquals("2013-02-02", eu.getVyrazovani().getDataceVyrazeni().getRokSkartacniOperace().toXMLFormat());
        assertEquals("2012-02-02", eu.getVyrazovani().getDataceVyrazeni().getRokSpousteciUdalosti().toXMLFormat());

        // test update of new date with period 5
        TSkartacniRezim skartacniRezim = new TSkartacniRezim();
        skartacniRezim.setSkartacniLhuta(5);
        eu.getVyrazovani().setSkartacniRezim(skartacniRezim);
        datumUzavreni.setValue(mapper.getXmlTypes().newXMLGregorianCalendar("2000-02-02"));
        mapper.fillDisposalDate(s);
        assertEquals("2006-02-02", eu.getVyrazovani().getDataceVyrazeni().getRokSkartacniOperace().toXMLFormat());
        assertEquals("2000-02-02", eu.getVyrazovani().getDataceVyrazeni().getRokSpousteciUdalosti().toXMLFormat());

        // test update with missing date
        datumUzavreni.setValue(null);
        mapper.fillDisposalDate(s);
        assertNull(eu.getVyrazovani().getDataceVyrazeni().getRokSkartacniOperace());
        assertNull(eu.getVyrazovani().getDataceVyrazeni().getRokSpousteciUdalosti());
//        TransformerFactory.newInstance().newTransformer().transform(
//                new JAXBSource(JAXBContext.newInstance(Spis.class), s),
//                new StreamResult(System.out));
//        System.out.println("");
    }

}