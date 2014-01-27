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
package cz.cas.lib.proarc.desa.nsesss2.mapping;

import cz.cas.lib.proarc.desa.nsesss2.NsesssConstants;
import cz.cas.lib.proarc.desa.nsesss2.TIdentifikator;
import cz.cas.lib.proarc.desa.nsesss2.TOsobyExterni;
import cz.cas.lib.proarc.desa.nsesss2.TSubjektExterni;
import cz.cas.lib.proarc.desa.nsesss2.mapping.PrijemceMapping.SubjektExterni;
import java.util.List;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class PrijemceMappingTest {

    public PrijemceMappingTest() {
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
        PrijemceMapping instance = new PrijemceMapping();
        TOsobyExterni result = instance.toJson(osobyExterni);
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

}