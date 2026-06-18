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

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Sada elementů pro identifikaci právnické nebo fyzické osoby a pro uvedení
 * její poštovní adresy (adresy pro doručování) nebo elektronického kontaktu.
 * Tato osoba není identická s organizací, která tato metadata vytváří, nebo
 * není v zaměstnaneckém poměru k této organizaci.
 *
 * <p>Java class for tSubjektExterni complex type.
 *
 * <p>The following schema fragment specifies the expected content contained
 * within this class.
 *
 * <pre>
 * &lt;complexType name="tSubjektExterni">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;choice>
 *         &lt;sequence>
 *           &lt;element name="IdentifikatorOrganizace" type="{http://www.mvcr.cz/nsesss/v2}tIdentifikator"/>
 *           &lt;element name="NazevOrganizace" type="{http://www.mvcr.cz/nsesss/v2}tNazev"/>
 *           &lt;element name="IdentifikatorFyzickeOsoby" type="{http://www.mvcr.cz/nsesss/v2}tIdentifikator" minOccurs="0"/>
 *           &lt;element name="NazevFyzickeOsoby" type="{http://www.mvcr.cz/nsesss/v2}tNazev" minOccurs="0"/>
 *           &lt;element name="OrganizacniUtvar" type="{http://www.mvcr.cz/nsesss/v2}tText" minOccurs="0"/>
 *           &lt;element name="PracovniPozice" type="{http://www.mvcr.cz/nsesss/v2}tText" minOccurs="0"/>
 *           &lt;element name="SidloOrganizace" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *           &lt;element name="ElektronickyKontakt" type="{http://www.mvcr.cz/nsesss/v2}tText" minOccurs="0"/>
 *         &lt;/sequence>
 *         &lt;sequence>
 *           &lt;element name="IdentifikatorFyzickeOsoby" type="{http://www.mvcr.cz/nsesss/v2}tIdentifikator" minOccurs="0"/>
 *           &lt;element name="NazevFyzickeOsoby" type="{http://www.mvcr.cz/nsesss/v2}tNazev"/>
 *           &lt;element name="PostovniAdresa" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *           &lt;element name="ElektronickyKontakt" type="{http://www.mvcr.cz/nsesss/v2}tText"/>
 *         &lt;/sequence>
 *       &lt;/choice>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tSubjektExterni", namespace = NsesssConstants.NS)
public class TSubjektExterni {

    @XmlElement(name = "IdentifikatorOrganizace", namespace = NsesssConstants.NS)
    private TIdentifikator identifikatorOrganizace;
    @XmlElement(name = "NazevOrganizace", namespace = NsesssConstants.NS)
    private String nazevOrganizace;
    @XmlElement(name = "IdentifikatorFyzickeOsoby", namespace = NsesssConstants.NS)
    private TIdentifikator IdentifikatorFyzickeOsoby;
    @XmlElement(name = "NazevFyzickeOsoby", namespace = NsesssConstants.NS)
    private String nazevFyzickeOsoby;
    @XmlElement(name = "OrganizacniUtvar", namespace = NsesssConstants.NS)
    private String organizacniUtvar;
    @XmlElement(name = "PracovniPozice", namespace = NsesssConstants.NS)
    private String pracovniPozice;
    @XmlElement(name = "SidloOrganizace", namespace = NsesssConstants.NS)
    private String sidloOrganizace;
    @XmlElement(name = "PostovniAdresa", namespace = NsesssConstants.NS)
    private String postovniAdresa;
    @XmlElement(name = "ElektronickyKontakt", namespace = NsesssConstants.NS)
    private String elektronickyKontakt;

    public TIdentifikator getIdentifikatorOrganizace() {
        return identifikatorOrganizace;
    }

    public void setIdentifikatorOrganizace(TIdentifikator IdentifikatorOrganizace) {
        this.identifikatorOrganizace = IdentifikatorOrganizace;
    }

    public String getNazevOrganizace() {
        return nazevOrganizace;
    }

    public void setNazevOrganizace(String NazevOrganizace) {
        this.nazevOrganizace = NazevOrganizace;
    }

    public TIdentifikator getIdentifikatorFyzickeOsoby() {
        return IdentifikatorFyzickeOsoby;
    }

    public void setIdentifikatorFyzickeOsoby(TIdentifikator IdentifikatorFyzickeOsoby) {
        this.IdentifikatorFyzickeOsoby = IdentifikatorFyzickeOsoby;
    }

    public String getNazevFyzickeOsoby() {
        return nazevFyzickeOsoby;
    }

    public void setNazevFyzickeOsoby(String NazevFyzickeOsoby) {
        this.nazevFyzickeOsoby = NazevFyzickeOsoby;
    }

    public String getOrganizacniUtvar() {
        return organizacniUtvar;
    }

    public void setOrganizacniUtvar(String OrganizacniUtvar) {
        this.organizacniUtvar = OrganizacniUtvar;
    }

    public String getPracovniPozice() {
        return pracovniPozice;
    }

    public void setPracovniPozice(String PracovniPozice) {
        this.pracovniPozice = PracovniPozice;
    }

    public String getSidloOrganizace() {
        return sidloOrganizace;
    }

    public void setSidloOrganizace(String SidloOrganizace) {
        this.sidloOrganizace = SidloOrganizace;
    }

    public String getPostovniAdresa() {
        return postovniAdresa;
    }

    public void setPostovniAdresa(String PostovniAdresa) {
        this.postovniAdresa = PostovniAdresa;
    }

    public String getElektronickyKontakt() {
        return elektronickyKontakt;
    }

    public void setElektronickyKontakt(String ElektronickyKontakt) {
        this.elektronickyKontakt = ElektronickyKontakt;
    }
}
