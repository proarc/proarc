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
import cz.cas.lib.proarc.desa.nsesss2.TDataceVyrazeni;
import cz.cas.lib.proarc.desa.nsesss2.TDatum;
import cz.cas.lib.proarc.desa.nsesss2.TDorucenyDokument;
import cz.cas.lib.proarc.desa.nsesss2.TEvidence;
import cz.cas.lib.proarc.desa.nsesss2.TEvidencniUdajeDokumentu;
import cz.cas.lib.proarc.desa.nsesss2.TEvidencniUdajeSpisu;
import cz.cas.lib.proarc.desa.nsesss2.TIdentifikace;
import cz.cas.lib.proarc.desa.nsesss2.TIdentifikator;
import cz.cas.lib.proarc.desa.nsesss2.TManipulaceSeskupeni;
import cz.cas.lib.proarc.desa.nsesss2.TOsobaExterni;
import cz.cas.lib.proarc.desa.nsesss2.TOsobyExterni;
import cz.cas.lib.proarc.desa.nsesss2.TOsobyInterni;
import cz.cas.lib.proarc.desa.nsesss2.TPuvodDokumentu;
import cz.cas.lib.proarc.desa.nsesss2.TPuvodSeskupeni;
import cz.cas.lib.proarc.desa.nsesss2.TSkartacniRezim;
import cz.cas.lib.proarc.desa.nsesss2.TSubjektExterni;
import cz.cas.lib.proarc.desa.nsesss2.TSubjektInterni;
import cz.cas.lib.proarc.desa.nsesss2.TTrideniDokumentu;
import cz.cas.lib.proarc.desa.nsesss2.TTypDokumentu;
import cz.cas.lib.proarc.desa.nsesss2.TUrceneCasoveObdobi;
import cz.cas.lib.proarc.desa.nsesss2.TVlastniDokument;
import cz.cas.lib.proarc.desa.nsesss2.TVyrazovani;
import cz.cas.lib.proarc.desa.nsesss2.TVyrizeniEntity;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.Duration;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * Maps NSESSS object values.
 *
 * @author Jan Pokorsky
 */
public final class NsesssMapper {

    private DatatypeFactory xmlTypes;

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
                replaceTSubjektExterni(vyrizeni.getPrijemce());
            }
            TPuvodDokumentu puvod = eu.getPuvod();
            if (puvod != null) {
                TDorucenyDokument dorucenyDokument = puvod.getDorucenyDokument();
                if (dorucenyDokument != null) {
                    TOsobaExterni odesilatel = dorucenyDokument.getOdesilatel();
                    if (odesilatel != null) {
                        TSubjektExterni synthetic = replaceTSubjektExterni(odesilatel.getSubjekt());
                        odesilatel.setSubjekt(synthetic);
                    }
                }
            }
        }
        return d;
    }

    TOsobyExterni replaceTSubjektExterni(TOsobyExterni obj) {
        if (obj == null) {
            return obj;
        }
        List<TSubjektExterni> in = obj.getSubjekt();
        List<TSubjektExterni> mapped = replaceTSubjektExterni(in);
        in.clear();
        in.addAll(mapped);
        return obj;
    }

    List<TSubjektExterni> replaceTSubjektExterni(List<TSubjektExterni> subjects) {
        List<TSubjektExterni> mappedSubjects = new ArrayList<TSubjektExterni>(subjects.size());
        for (TSubjektExterni subject : subjects) {
            TSubjektExterni mapped = replaceTSubjektExterni(subject);
            mappedSubjects.add(mapped);
        }
        return mappedSubjects;
    }

    private TSubjektExterni replaceTSubjektExterni(TSubjektExterni subject) {
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

    /**
     * Fills elements {@code Dokument/EvidencniUdaje/Vyrazovani/SkartacniRezim/DataceVyrazeni/RokSpousteciUdalosti+RokSpousteciUdalosti}
     * with combination of {@code Dokument/EvidencniUdaje/Vyrizeni/Datum}
     * and {@code Dokument/EvidencniUdaje/Vyrazovani/SkartacniRezim/SkartacniLhuta}.
     */
    public void fillDisposalDate(Dokument d) {
        TEvidencniUdajeDokumentu eu = d.getEvidencniUdaje();
        if (eu != null) {
            XMLGregorianCalendar closeDate = null;
            TVyrizeniEntity vyrizeni = eu.getVyrizeni();
            if (vyrizeni != null) {
                TDatum datum = vyrizeni.getDatum();
                if (datum != null) {
                    closeDate = datum.getValue();
                }
            }
            TVyrazovani vyrazovani = eu.getVyrazovani();
            if (vyrazovani == null && closeDate != null) {
                vyrazovani = new TVyrazovani();
                eu.setVyrazovani(vyrazovani);
            }
            fillDisposalDate(vyrazovani, closeDate);
        }
    }

    /**
     * Fills elements {@code Spis/EvidencniUdaje/Vyrazovani/SkartacniRezim/DataceVyrazeni/RokSpousteciUdalosti+RokSpousteciUdalosti}
     * with combination of {@code Spis/EvidencniUdaje/Manipulace/DatumUzavreni}
     * and {@code Spis/EvidencniUdaje/Vyrazovani/SkartacniRezim/SkartacniLhuta}.
     */
    public Spis fillDisposalDate(Spis s) {
        TEvidencniUdajeSpisu eu = s.getEvidencniUdaje();
        if (eu != null) {
            XMLGregorianCalendar closeDate = null;
            TManipulaceSeskupeni manipulace = eu.getManipulace();
            if (manipulace != null) {
                TDatum datumUzavreni = manipulace.getDatumUzavreni();
                if (datumUzavreni != null) {
                    closeDate = datumUzavreni.getValue();
                }
            }
            TVyrazovani vyrazovani = eu.getVyrazovani();
            if (vyrazovani == null && closeDate != null) {
                vyrazovani = new TVyrazovani();
                eu.setVyrazovani(vyrazovani);
            }
            fillDisposalDate(vyrazovani, closeDate);
        }
        return s;
    }

    private void fillDisposalDate(TVyrazovani vyrazovani, XMLGregorianCalendar closeDate) {
        if (vyrazovani != null) {
            int disposalPeriod = 0;
            TSkartacniRezim skartacniRezim = vyrazovani.getSkartacniRezim();
            if (skartacniRezim != null) {
                disposalPeriod = skartacniRezim.getSkartacniLhuta();
            }
            TDataceVyrazeni dataceVyrazeni = vyrazovani.getDataceVyrazeni();
            if (dataceVyrazeni == null && closeDate != null) {
                dataceVyrazeni = new TDataceVyrazeni();
                vyrazovani.setDataceVyrazeni(dataceVyrazeni);
            }
            if (dataceVyrazeni != null) {
                XMLGregorianCalendar disposalDate = null;
                if (closeDate != null) {
                    Duration duration = getXmlTypes().newDurationYearMonth(true, 1 + disposalPeriod, 0);
                    disposalDate = (XMLGregorianCalendar) closeDate.clone();
                    disposalDate.add(duration);
                }
                dataceVyrazeni.setRokSkartacniOperace(disposalDate);
                dataceVyrazeni.setRokSpousteciUdalosti(closeDate);
            }
        }
    }

    /**
     * Fills default values. ID, current date, ...
     * @param id unique ID with NCName syntax
     */
    public Spis fillDefaults(Spis s, String id) {
        s.setID(id);
        XMLGregorianCalendar now = getXmlTypes().newXMLGregorianCalendar(new GregorianCalendar());
        TEvidencniUdajeSpisu eu = s.getEvidencniUdaje();
        if (eu == null) {
            eu = new TEvidencniUdajeSpisu();
            s.setEvidencniUdaje(eu);
        }
        TPuvodSeskupeni puvod = eu.getPuvod();
        if (puvod == null) {
            puvod = new TPuvodSeskupeni();
            eu.setPuvod(puvod);
        }
        TDatum datumVytvoreni = puvod.getDatumVytvoreni();
        if (datumVytvoreni == null) {
            datumVytvoreni = new TDatum();
            puvod.setDatumVytvoreni(datumVytvoreni);
        }
        datumVytvoreni.setValue(now);

        TEvidence evidence = eu.getEvidence();
        if (evidence == null) {
            evidence = new TEvidence();
            eu.setEvidence(evidence);
        }
        TUrceneCasoveObdobi urceneCasoveObdobi = evidence.getUrceneCasoveObdobi();
        if (urceneCasoveObdobi == null) {
            urceneCasoveObdobi = new TUrceneCasoveObdobi();
            evidence.setUrceneCasoveObdobi(urceneCasoveObdobi);
        }
        urceneCasoveObdobi.setRok(now);
        return s;
    }

    /**
     * Fills default values. ID, current date, ...
     * @param id unique ID with NCName syntax
     */
    public Dokument fillDefaults(Dokument d, boolean intenalDocument, String id) {
        d.setID(id);
        XMLGregorianCalendar now = getXmlTypes().newXMLGregorianCalendar(new GregorianCalendar());
        TEvidencniUdajeDokumentu eu = d.getEvidencniUdaje();
        if (eu == null) {
            eu = new TEvidencniUdajeDokumentu();
            d.setEvidencniUdaje(eu);
        }
        TPuvodDokumentu puvod = eu.getPuvod();
        if (puvod == null) {
            puvod = new TPuvodDokumentu();
            eu.setPuvod(puvod);
        }
        if (intenalDocument) {
            TVlastniDokument vlastniDokument = puvod.getVlastniDokument();
            if (vlastniDokument == null) {
                vlastniDokument = new TVlastniDokument();
                puvod.setVlastniDokument(vlastniDokument);
            }
            TDatum datumVytvoreni = vlastniDokument.getDatumVytvoreni();
            if (datumVytvoreni == null) {
                datumVytvoreni = new TDatum();
                vlastniDokument.setDatumVytvoreni(datumVytvoreni);
            }
            datumVytvoreni.setValue(now);
        }
        TEvidence evidence = eu.getEvidence();
        if (evidence == null) {
            evidence = new TEvidence();
            eu.setEvidence(evidence);
        }
        TUrceneCasoveObdobi urceneCasoveObdobi = evidence.getUrceneCasoveObdobi();
        if (urceneCasoveObdobi == null) {
            urceneCasoveObdobi = new TUrceneCasoveObdobi();
            evidence.setUrceneCasoveObdobi(urceneCasoveObdobi);
        }
        urceneCasoveObdobi.setRok(now);
        return d;
    }

    DatatypeFactory getXmlTypes() {
        try {
            if (xmlTypes == null) {
                xmlTypes = DatatypeFactory.newInstance();
            }
            return xmlTypes;
        } catch (DatatypeConfigurationException ex) {
            throw new IllegalStateException(ex);
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
