/*
 * Copyright (C) 2019 Lukas Sykora
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

package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;

@XmlRootElement(name = "Kroniky")
@XmlAccessorType(value = XmlAccessType.FIELD)
public class ImportCatalog {

    @XmlElement(name = "Kronika")
    private List<Kronika> kronika;

    public List<Kronika> getKronika() {
        if (kronika == null) {
            kronika = new ArrayList<>();
        }
        return  kronika;
    }

    @XmlType(namespace = ProarcXmlUtils.NS_IMPORT)
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Kronika {


        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String Id;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String LocalId;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String Nazev;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String MistoUlozeni;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String Revers;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String PocetSnimku;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String Prijeti;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String Vraceni;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String PredaniDat;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String Poznamka;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String Obdobi;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String Uloziste;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String CestaKomplet;

        @XmlElement(namespace = ProarcXmlUtils.NS_IMPORT)
        private String ZalohaCD;


        public String getId() {
            return Id;
        }

        public void setId(String id) {
            Id = id;
        }

        public String getLocalId() {
            return LocalId;
        }

        public void setLocalId(String localId) {
            LocalId = localId;
        }

        public String getNazev() {
            return Nazev;
        }

        public void setNazev(String nazev) {
            Nazev = nazev;
        }

        public String getMistoUlozeni() {
            return MistoUlozeni;
        }

        public void setMistoUlozeni(String mistoUlozeni) {
            MistoUlozeni = mistoUlozeni;
        }

        public String getRevers() {
            return Revers;
        }

        public void setRevers(String revers) {
            Revers = revers;
        }

        public String getPocetSnimku() {
            return PocetSnimku;
        }

        public void setPocetSnimku(String pocetSnimku) {
            PocetSnimku = pocetSnimku;
        }

        public String getPrijeti() {
            return Prijeti;
        }

        public void setPrijeti(String prijeti) {
            Prijeti = prijeti;
        }

        public String getVraceni() {
            return Vraceni;
        }

        public void setVraceni(String vraceni) {
            Vraceni = vraceni;
        }

        public String getPredaniDat() {
            return PredaniDat;
        }

        public void setPredaniDat(String predaniDat) {
            PredaniDat = predaniDat;
        }

        public String getPoznamka() {
            return Poznamka;
        }

        public void setPoznamka(String poznamka) {
            Poznamka = poznamka;
        }

        public String getObdobi() {
            return Obdobi;
        }

        public void setObdobi(String obdobi) {
            Obdobi = obdobi;
        }

        public String getUloziste() {
            return Uloziste;
        }

        public void setUloziste(String uloziste) {
            Uloziste = uloziste;
        }

        public String getCestaKomplet() {
            return CestaKomplet;
        }

        public void setCestaKomplet(String cestaKomplet) {
            CestaKomplet = cestaKomplet;
        }

        public String getZalohaCD() {
            return ZalohaCD;
        }

        public void setZalohaCD(String zalohaCD) {
            ZalohaCD = zalohaCD;
        }
    }

}
