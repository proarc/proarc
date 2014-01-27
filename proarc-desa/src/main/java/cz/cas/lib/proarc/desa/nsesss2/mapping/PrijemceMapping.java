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

import cz.cas.lib.proarc.desa.nsesss2.Dokument;
import cz.cas.lib.proarc.desa.nsesss2.NsesssConstants;
import cz.cas.lib.proarc.desa.nsesss2.TEvidencniUdajeDokumentu;
import cz.cas.lib.proarc.desa.nsesss2.TOsobyExterni;
import cz.cas.lib.proarc.desa.nsesss2.TSubjektExterni;
import cz.cas.lib.proarc.desa.nsesss2.TVyrizeniEntity;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * Maps TSubjektExterni to SubjektExterni to allow to recognize
 * the type of TSubjektExterni in JSON. TSubjektExterni is extended with subjectType
 * with possible values ("FyzickaOsoba", "PravnickaOsoba"). The synthetic subjectType
 * is discarded on JAXB serialization.
 *
 * <p>It should be applied to {@code /Dokument/EvidencniUdaje/Vyrizeni/Prijemce/Subjekt}
 *
 * @author Jan Pokorsky
 */
public final class PrijemceMapping {

    public Dokument toJson(Dokument d) {
        TEvidencniUdajeDokumentu eu = d.getEvidencniUdaje();
        if (eu != null) {
            TVyrizeniEntity vyrizeni = eu.getVyrizeni();
            if (vyrizeni != null) {
                toJson(vyrizeni.getPrijemce());
            }
        }
        return d;
    }

    TOsobyExterni toJson(TOsobyExterni obj) {
        if (obj == null) {
            return obj;
        }
        List<TSubjektExterni> in = obj.getSubjekt();
        List<TSubjektExterni> mapped = toJson(in);
        in.clear();
        in.addAll(mapped);
        return obj;
    }

    List<TSubjektExterni> toJson(List<TSubjektExterni> subjects) {
        List<TSubjektExterni> mappedSubjects = new ArrayList<TSubjektExterni>(subjects.size());
        for (TSubjektExterni subject : subjects) {
            TSubjektExterni mapped = toJson(subject);
            mappedSubjects.add(mapped);
        }
        return mappedSubjects;
    }

    private TSubjektExterni toJson(TSubjektExterni subject) {
        return SubjektExterni.fromSubjektExterni(subject);
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class SubjektExterni extends TSubjektExterni {

        private String subjectType;

        public SubjektExterni() {
        }

        public String getSubjectType() {
            return subjectType;
        }

        public void setSubjectType(String subjectType) {
            this.subjectType = subjectType;
        }

        public static SubjektExterni fromSubjektExterni(TSubjektExterni se) {
            SubjektExterni o = new SubjektExterni();
            o.subjectType = se.getPostovniAdresa() != null
                    ? NsesssConstants.DOKUMENT_PRIJEMCE_FYZICKA_OSOBA: NsesssConstants.DOKUMENT_PRIJEMCE_PRAVNICKA_OSOBA;
            o.setElektronickyKontakt(se.getElektronickyKontakt());
            o.setIdentifikatorFyzickeOsoby(se.getIdentifikatorFyzickeOsoby());
            o.setIdentifikatorOrganizace(se.getIdentifikatorOrganizace());
            o.setNazevFyzickeOsoby(se.getNazevFyzickeOsoby());
            o.setNazevOrganizace(se.getNazevOrganizace());
            o.setOrganizacniUtvar(se.getOrganizacniUtvar());
            o.setPostovniAdresa(se.getPostovniAdresa());
            o.setPracovniPozice(se.getPracovniPozice());
            o.setSidloOrganizace(se.getSidloOrganizace());
            return o;
        }

    }

}
