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
import cz.cas.lib.proarc.desa.nsesss2.Spis;
import cz.cas.lib.proarc.desa.nsesss2.TDorucenyDokument;
import cz.cas.lib.proarc.desa.nsesss2.TEvidence;
import cz.cas.lib.proarc.desa.nsesss2.TEvidencniUdajeDokumentu;
import cz.cas.lib.proarc.desa.nsesss2.TEvidencniUdajeSpisu;
import cz.cas.lib.proarc.desa.nsesss2.TIdentifikace;
import cz.cas.lib.proarc.desa.nsesss2.TIdentifikator;
import cz.cas.lib.proarc.desa.nsesss2.TOsobaExterni;
import cz.cas.lib.proarc.desa.nsesss2.TOsobyExterni;
import cz.cas.lib.proarc.desa.nsesss2.TOsobyInterni;
import cz.cas.lib.proarc.desa.nsesss2.TPuvodDokumentu;
import cz.cas.lib.proarc.desa.nsesss2.TSkartacniRezim;
import cz.cas.lib.proarc.desa.nsesss2.TSubjektExterni;
import cz.cas.lib.proarc.desa.nsesss2.TSubjektInterni;
import cz.cas.lib.proarc.desa.nsesss2.TTrideniDokumentu;
import cz.cas.lib.proarc.desa.nsesss2.TTypDokumentu;
import cz.cas.lib.proarc.desa.nsesss2.TVlastniDokument;
import cz.cas.lib.proarc.desa.nsesss2.TVyrazovani;
import cz.cas.lib.proarc.desa.nsesss2.TVyrizeniEntity;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * Maps NSESSS object values.
 *
 * @author Jan Pokorsky
 */
public final class NsesssMapper {

    /**
     * Replaces {@link TSubjektExterni} to {@link SubjektExterni} to allow to recognize
     * the type of TSubjektExterni in JSON. TSubjektExterni is extended with subjectType
     * with possible values ("FyzickaOsoba", "PravnickaOsoba"). The synthetic subjectType
     * is discarded on JAXB serialization.
     *
     * <p>It should be applied to {@code /Dokument/EvidencniUdaje/Vyrizeni/Prijemce/Subjekt}
     */
    public Dokument replaceTSubjektExterni(Dokument d) {
        TEvidencniUdajeDokumentu eu = d.getEvidencniUdaje();
        if (eu != null) {
            TVyrizeniEntity vyrizeni = eu.getVyrizeni();
            if (vyrizeni != null) {
                toJson(vyrizeni.getPrijemce());
            }
            TPuvodDokumentu puvod = eu.getPuvod();
            if (puvod != null) {
                TDorucenyDokument dorucenyDokument = puvod.getDorucenyDokument();
                if (dorucenyDokument != null) {
                    TOsobaExterni odesilatel = dorucenyDokument.getOdesilatel();
                    if (odesilatel != null) {
                        TSubjektExterni synthetic = toJson(odesilatel.getSubjekt());
                        odesilatel.setSubjekt(synthetic);
                    }
                }
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

    /**
     * Fills {@code zdroj} attribute of all {@link TIdentifikator} elements
     * with {@code /Spis/EvidencniUdaje/Evidence/NazevEvidenceDokumentu} value.
     * @param s
     * @return updated object
     */
    public Spis fillZdroj(Spis s) {
        TEvidencniUdajeSpisu eu = s.getEvidencniUdaje();
        if (eu != null) {
            TEvidence evidence = eu.getEvidence();
            if (evidence != null) {
                String evidenceId = evidence.getNazevEvidenceDokumentu();
                TIdentifikace identifikace = eu.getIdentifikace();
                if (identifikace != null) {
                    for (TIdentifikator id : identifikace.getIdentifikator()) {
                        fillZdroj(id, evidenceId);
                    }
                }
                TVyrizeniEntity vyrizeniUzavreni = eu.getVyrizeniUzavreni();
                if (vyrizeniUzavreni != null) {
                    fillZdroj(vyrizeniUzavreni.getZpracovatel(), evidenceId);
                }
                TVyrazovani vyrazovani = eu.getVyrazovani();
                if (vyrazovani != null) {
                    TSkartacniRezim sr = vyrazovani.getSkartacniRezim();
                    if (sr != null) {
                        fillZdroj(sr.getIdentifikator(), evidenceId);
                    }
                }
            }
        }
        return s;
    }

    /**
     * Fills {@code zdroj} attribute of all {@link TIdentifikator} elements
     * with {@code /Document/EvidencniUdaje/Evidence/NazevEvidenceDokumentu} value.
     * @param s
     * @return updated object
     */
    public Dokument fillZdroj(Dokument d) {
        TEvidencniUdajeDokumentu eu = d.getEvidencniUdaje();
        if (eu != null) {
            TEvidence evidence = eu.getEvidence();
            if (evidence != null) {
                String evidenceId = evidence.getNazevEvidenceDokumentu();
                TIdentifikace identifikace = eu.getIdentifikace();
                if (identifikace != null) {
                    for (TIdentifikator id : identifikace.getIdentifikator()) {
                        fillZdroj(id, evidenceId);
                    }
                }
                TTrideniDokumentu trideni = eu.getTrideni();
                if (trideni != null) {
                    TTypDokumentu td = trideni.getTypDokumentu();
                    if (td != null) {
                        fillZdroj(td.getIdentifikator(), evidenceId);
                    }
                }
                TVyrizeniEntity vyrizeni = eu.getVyrizeni();
                if (vyrizeni != null) {
                    fillZdroj(vyrizeni.getZpracovatel(), evidenceId);
                    TOsobyExterni prijemce = vyrizeni.getPrijemce();
                    if (prijemce != null) {
                        for (TSubjektExterni sbj : prijemce.getSubjekt()) {
                            fillZdroj(sbj, evidenceId);
                        }
                    }
                }
                TVyrazovani vyrazovani = eu.getVyrazovani();
                if (vyrazovani != null) {
                    TSkartacniRezim sr = vyrazovani.getSkartacniRezim();
                    if (sr != null) {
                        fillZdroj(sr.getIdentifikator(), evidenceId);
                    }
                }
                TPuvodDokumentu puvod = eu.getPuvod();
                if (puvod != null) {
                    TDorucenyDokument dorucenyDokument = puvod.getDorucenyDokument();
                    if (dorucenyDokument != null) {
                        TOsobaExterni odesilatel = dorucenyDokument.getOdesilatel();
                        if (odesilatel != null) {
                            fillZdroj(odesilatel.getSubjekt(), evidenceId);
                        }
                    }
                    TVlastniDokument vlastniDokument = puvod.getVlastniDokument();
                    if (vlastniDokument != null) {
                        fillZdroj(vlastniDokument.getAutor(), evidenceId);
                    }
                }
            }
        }
        return d;
    }

    void fillZdroj(TIdentifikator id, String zdroj) {
        if (id != null) {
            id.setZdroj(zdroj);
        }
    }

    void fillZdroj(TSubjektExterni sbj, String evidenceId) {
        fillZdroj(sbj.getIdentifikatorFyzickeOsoby(), evidenceId);
        fillZdroj(sbj.getIdentifikatorOrganizace(), evidenceId);
    }

    void fillZdroj(TOsobyInterni internal, String evidenceId) {
        if (internal != null) {
            for (TSubjektInterni sbj : internal.getSubjekt()) {
                fillZdroj(sbj.getIdentifikatorFyzickeOsoby(), evidenceId);
                fillZdroj(sbj.getIdentifikatorOrganizace(), evidenceId);
            }
        }
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
