/*
 * Copyright (C) 2012 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.rest;

import cz.incad.pas.editor.server.catalog.AlephXServer;
import java.io.IOException;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.transform.TransformerException;

/**
 * Permits to query metadata from remote catalogs like Aleph, registrdigitalizace.cz, ...
 *
 * @author Jan Pokorsky
 */
@Path("/metadatacatalog")
public class MetadataCatalogResource {

    private static final Logger LOG = Logger.getLogger(MetadataCatalogResource.class.getName());

    private final HttpHeaders httpHeaders;

    public MetadataCatalogResource(
            @Context HttpHeaders httpHeaders
            ) {

        this.httpHeaders = httpHeaders;
    }

    /**
     * Finds metadata in remote catalog.
     *
     * @param catalog catalog descriptor
     * @param fieldName issn|isbn|ccnb
     * @param value value to query
     * @return list of metadata records
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public MetadataList find(
            @QueryParam("catalog") String catalog,
            @QueryParam("fieldName") String fieldName,
            @QueryParam("value") String value

            ) throws TransformerException, IOException {

        List<Locale> acceptableLanguages = httpHeaders.getAcceptableLanguages();
        Locale locale = acceptableLanguages.isEmpty() ? null : acceptableLanguages.get(0);
        AlephXServer alephXServer = new AlephXServer();
        List<MetadataItem> result = alephXServer.find(fieldName, value, locale);
        return new MetadataList(result);
    }

    public enum CATALOG {
        ALEPH, REGISTRCZ
    }

    /**
     * JAXB helper to list items.
     */
    @XmlRootElement(name = "metadataCatalogEntries")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class MetadataList {

        @XmlElement(name="entry")
        List<MetadataItem> list;

        public MetadataList() {
        }

        public MetadataList(List<MetadataItem> list) {
            this.list = list;
        }

    }

//    @XmlRootElement
//    @XmlAccessorType(XmlAccessType.FIELD)
//    public static class Criterion {
//
//        private String operator;
//        private String fieldName;
//        private String value;
//
//        public Criterion() {
//        }
//
//        public String getFieldName() {
//            return fieldName;
//        }
//
//        public String getValue() {
//            return value;
//        }
//
//        @Override
//        public String toString() {
//            return String.format("Criterion[operator: %s, fieldName: %s, value: %s]",
//                    operator, fieldName, value);
//        }
//
//    }

    /**
     * Describes metadata item fetched from catalog.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class MetadataItem {
        
        private int id;
        /** MODS XML */
        private String mods;
        /** displayable mods; HTML is permitted */
        private String preview;
        /** short descriptor of the item; used in ListGrid */
        private String title;

        public MetadataItem() {
        }

        public MetadataItem(int id, String mods, String preview, String title) {
            this.id = id;
            this.mods = mods;
            this.preview = preview;
            this.title = title;
        }

        public int getId() {
            return id;
        }

        public String getMods() {
            return mods;
        }

        public String getPreview() {
            return preview;
        }

        public String getTitle() {
            return title;
        }
    }

}
