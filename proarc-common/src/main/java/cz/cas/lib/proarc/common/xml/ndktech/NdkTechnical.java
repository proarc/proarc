/*
 * Copyright (C) 2024 Lukas Sykora
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
package cz.cas.lib.proarc.common.xml.ndktech;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * The object that has ndktech schema
 *
 * @author Lukas Sykora
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "ndktech", namespace = NdkTechnicalUtils.NS)
public class NdkTechnical {

    @XmlElement(name = "mediaTypes", namespace = NdkTechnicalUtils.NS)
    protected MediaTypesType mediatypes;

    @XmlElement(name = "entries", namespace = NdkTechnicalUtils.NS)
    protected EntriesComplexType entries;

    @XmlElement(name = "filter", namespace = NdkTechnicalUtils.NS)
    protected FiltersType filters;

    @XmlElement(name = "profiles", namespace = NdkTechnicalUtils.NS)
    protected ProfilesType profiles;

    @XmlElement(name = "colorspaces", namespace = NdkTechnicalUtils.NS)
    protected ColorSpacesType colorspaces;

    @XmlElement(name = "imageCount", namespace = NdkTechnicalUtils.NS)
    protected BigInteger imagesCount;

    @XmlElement(name = "indirectObjectsNumber", namespace = NdkTechnicalUtils.NS)
    protected BigInteger indirectObjectsNumber;

    public MediaTypesType getMediatypes() {
        return mediatypes;
    }

    public void setMediatypes(MediaTypesType mediatypes) {
        this.mediatypes = mediatypes;
    }

    public EntriesComplexType getEntries() {
        return entries;
    }

    public void setEntries(EntriesComplexType entries) {
        this.entries = entries;
    }

    public FiltersType getFilters() {
        return filters;
    }

    public void setFilters(FiltersType filters) {
        this.filters = filters;
    }

    public ProfilesType getProfiles() {
        return profiles;
    }

    public void setProfiles(ProfilesType profiles) {
        this.profiles = profiles;
    }

    public ColorSpacesType getColorspaces() {
        return colorspaces;
    }

    public void setColorspaces(ColorSpacesType colorspaces) {
        this.colorspaces = colorspaces;
    }

    public BigInteger getImagesCount() {
        return imagesCount;
    }

    public void setImagesCount(BigInteger imagesCount) {
        this.imagesCount = imagesCount;
    }

    public BigInteger getIndirectObjectsNumber() {
        return indirectObjectsNumber;
    }

    public void setIndirectObjectsNumber(BigInteger indirectObjectsNumber) {
        this.indirectObjectsNumber = indirectObjectsNumber;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "MediaTypesType", namespace = NdkTechnicalUtils.NS)
    public static class MediaTypesType {

        @XmlElement(name = "mediaType", namespace = NdkTechnicalUtils.NS)
        protected List<String> mediatype;

        public List<String> getMediatype() {
            if (mediatype == null) {
                mediatype = new ArrayList<>();
            }
            return mediatype;
        }

        public void setMediatype(List<String> mediatype) {
            this.mediatype = mediatype;
        }
    }


    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "EntriesComplexType", namespace = NdkTechnicalUtils.NS)
    public static class EntriesComplexType {

        @XmlElement(name = "entry", namespace = NdkTechnicalUtils.NS)
        protected List<String> entry;

        public List<String> getEntry() {
            if (entry == null) {
                entry = new ArrayList<>();
            }
            return entry;
        }

        public void setEntry(List<String> entry) {
            this.entry = entry;
        }
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "FiltersType", namespace = NdkTechnicalUtils.NS)
    public static class FiltersType {

        @XmlElement(name = "filter", namespace = NdkTechnicalUtils.NS)
        protected List<String> filter;

        public List<String> getFilter() {
            if (filter == null) {
                filter = new ArrayList<>();
            }
            return filter;
        }

        public void setFilter(List<String> filter) {
            this.filter = filter;
        }
    }


    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ProfilesType", namespace = NdkTechnicalUtils.NS)
    public static class ProfilesType {

        @XmlElement(name = "profile", namespace = NdkTechnicalUtils.NS)
        protected List<String> profile;

        public List<String> getProfile() {
            if (profile == null) {
                profile = new ArrayList<>();
            }
            return profile;
        }

        public void setProfile(List<String> profile) {
            this.profile = profile;
        }
    }


    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ColorSpacesType", namespace = NdkTechnicalUtils.NS)
    public static class ColorSpacesType {

        @XmlElement(name = "colorspace", namespace = NdkTechnicalUtils.NS)
        protected List<String> colorspace;

        @XmlElement(name = "iccprofile", namespace = NdkTechnicalUtils.NS)
        protected List<IccProfileType> iccprofile;

        public List<String> getColorspace() {
            if (colorspace == null) {
                colorspace = new ArrayList<>();
            }
            return colorspace;
        }

        public void setColorspace(List<String> colorspace) {
            this.colorspace = colorspace;
        }

        public List<IccProfileType> getIccprofile() {
            if (iccprofile == null) {
                iccprofile = new ArrayList<>();
            }
            return iccprofile;
        }

        public void setIccprofile(List<IccProfileType> iccprofile) {
            this.iccprofile = iccprofile;
        }
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "IccProfileType", namespace = NdkTechnicalUtils.NS)
    public static class IccProfileType {

        @XmlElement(name = "iccprofilename", namespace = NdkTechnicalUtils.NS)
        protected List<String> iccprofilename;

        @XmlElement(name = "iccprofileversion", namespace = NdkTechnicalUtils.NS)
        protected List<String> iccprofileversion;

        public List<String> getIccprofilename() {
            if (iccprofilename == null) {
                iccprofilename = new ArrayList<>();
            }
            return iccprofilename;
        }

        public void setIccprofilename(List<String> iccprofilename) {
            this.iccprofilename = iccprofilename;
        }

        public List<String> getIccprofileversion() {
            if (iccprofileversion == null) {
                iccprofileversion = new ArrayList<>();
            }
            return iccprofileversion;
        }

        public void setIccprofileversion(List<String> iccprofileversion) {
            this.iccprofileversion = iccprofileversion;
        }
    }
}
