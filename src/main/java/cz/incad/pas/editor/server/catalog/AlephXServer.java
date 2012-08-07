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
package cz.incad.pas.editor.server.catalog;

import cz.incad.pas.editor.server.mods.ModsUtils;
import cz.incad.pas.editor.server.rest.MetadataCatalogResource.MetadataItem;
import cz.incad.pas.editor.server.xml.Transformers;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Element;

/**
 * Aleph X Server metadata provider
 *
 * @author Jan Pokorsky
 */
public final class AlephXServer {

    private static final Logger LOG = Logger.getLogger(AlephXServer.class.getName());
    private Transformers transformers = new Transformers();

    public List<MetadataItem> find(String fieldName, String value) throws TransformerException, IOException {
        return find(fieldName, value, null);
    }

    public List<MetadataItem> find(String fieldName, String value, Locale locale) throws TransformerException, IOException {
        Criteria criteria = Criteria.get(fieldName, value);
        if (value == null) {
            return Collections.emptyList();
        }
        InputStream is = fetchEntries(criteria);
        FindResponse found = createFindResponse(is);
        if (found == null || found.getEntryCount() < 1) {
            return Collections.emptyList();
        }

        is = fetchDetails(found);
        return createDetailResponse(is, locale);
    }

    List<MetadataItem> createDetailResponse(InputStream is, Locale locale) throws TransformerException {
        try {
            StreamSource fixedOaiMarc = (StreamSource) transformers.transform(new StreamSource(is), Transformers.Format.AlephOaiMarcFix);
//            StringBuilder sb = new StringBuilder();
//            fixedOaiMarc = (StreamSource) transformers.dump(fixedOaiMarc, sb);
//            String toString = sb.toString();
//            fixedOaiMarc = (StreamSource) transformers.dump2Temp(fixedOaiMarc, "1AlephOaiMarcFix.xml");
            
            DetailResponse details = JAXB.unmarshal(fixedOaiMarc.getInputStream(), DetailResponse.class);
            if (details == null) {
                return Collections.emptyList();
            }
            List<MetadataItem> result = new ArrayList<MetadataItem>();
            for (DetailResponse.Record record : details.getRecords()) {
                Element oaiMarc = record.getOaiMarc();
                DOMSource domSource = new DOMSource(oaiMarc);
                MetadataItem item;
                try {
                    item = createResponse(record.getEntry(), domSource, locale);
                    result.add(item);
                } catch (UnsupportedEncodingException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }
            return result;
        } finally {
            try {
                is.close();
            } catch (IOException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
    }

    FindResponse createFindResponse(InputStream is) {
        try {
            return JAXB.unmarshal(is, AlephXServer.FindResponse.class);
        } finally {
            try {
                is.close();
            } catch (IOException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
    }
    
    private MetadataItem createResponse(int entryIdx, Source source, Locale locale)
            throws TransformerException, UnsupportedEncodingException {

//        StringBuilder sb = new StringBuilder();
//        source = transformers.dump(source, sb);
//        String toString = sb.toString();
//        source = transformers.dump2Temp(source, "2AlephOaiMarcFixedElement.xml");
        

        Source marcxmlSrc = transformers.transform(source, Transformers.Format.OaimarcAsMarc21slim);
//        marcxmlSrc = transformers.dump2Temp(marcxmlSrc, "3OaimarcAsMarc21slim.xml");
        byte[] modsBytes = transformers.transformAsBytes(
                marcxmlSrc, Transformers.Format.MarcxmlAsMods34);
//        try {
//            FileOutputStream tmp = new FileOutputStream("/tmp/aleph/4mods.xml");
//            tmp.write(modsBytes);
//            tmp.close();
//        } catch (Exception ex) {
//            Logger.getLogger(AlephXServer.class.getName()).log(Level.SEVERE, null, ex);
//        }
        byte[] modsHtmlBytes = modsAsHtmlBytes(new StreamSource(new ByteArrayInputStream(modsBytes)), locale);
        byte[] modsTitleBytes = transformers.transformAsBytes(
                new StreamSource(new ByteArrayInputStream(modsBytes)),
                Transformers.Format.ModsAsTitle);
//        try {
//            FileOutputStream tmp = new FileOutputStream("/tmp/aleph/5title.txt");
//            tmp.write(modsTitleBytes);
//            tmp.close();
//        } catch (Exception ex) {
//            Logger.getLogger(AlephXServer.class.getName()).log(Level.SEVERE, null, ex);
//        }
        return new MetadataItem(entryIdx, new String(modsBytes, "UTF-8"),
                new String(modsHtmlBytes, "UTF-8"), new String(modsTitleBytes, "UTF-8"));
    }

    private byte[] modsAsHtmlBytes(Source source, Locale locale) throws TransformerException {
        byte[] modsHtmlBytes = transformers.transformAsBytes(
                source, Transformers.Format.ModsAsHtml, ModsUtils.modsAsHtmlParameters(locale));
        return modsHtmlBytes;
    }

    private InputStream fetchEntries(Criteria criteria) throws MalformedURLException, IOException {
        URL alephFind = criteria.toFindUrl();
        return alephFind.openStream();
    }

    private InputStream fetchDetails(FindResponse found) throws MalformedURLException, IOException {
        String number = found.getNumber();
        int entryCount = found.getEntryCount();
        entryCount = Math.min(10, entryCount);
        String entries = (entryCount == 1) ? "1" : "1-" + entryCount;
        String path = String.format("http://aleph.nkp.cz/X?op=present&set_number=%s&set_entry=%s", number, entries);
        URL alephDetails = new URL(path);
        return new BufferedInputStream(alephDetails.openStream());
    }

    private static final class Criteria {

        private enum Field {
            ISSN("ssn"), ISBN("sbn"), CCNB("cnb");

            private String keyword;

            private Field(String keyword) {
                this.keyword = keyword;
            }

            public String getKeyword() {
                return keyword;
            }

        }

        private String value;
        private Criteria.Field field;

        public Criteria(String value, Criteria.Field field) {
            this.value = value;
            this.field = field;
        }

        public URL toFindUrl() throws MalformedURLException {
            String url = String.format("http://aleph.nkp.cz/X?op=find&request=%s=%s&base=nkc",
                    field.getKeyword(), value);
            return new URL(url);
        }

        private static Criteria get(String fieldName, String value) {
            if (value == null  || value.trim().length() == 0) {
                return null;
            }
            Criteria result = null;
            if ("issn".equalsIgnoreCase(fieldName)) {
                result = new Criteria(value, Criteria.Field.ISSN);
            } else if ("isbn".equalsIgnoreCase(fieldName)) {
                result = new Criteria(value, Criteria.Field.ISBN);
            } else if ("ccnb".equalsIgnoreCase(fieldName)) {
                result = new Criteria(value, Criteria.Field.CCNB);
            }
            return result;
        }
    }


    @XmlRootElement(name = "present")
    public static class DetailResponse {

        @XmlElement
        private List<Record> record;

        public DetailResponse() {
        }

        public List<Record> getRecords() {
            return record;
        }


        public static class Record {

            @XmlElement(name = "record_header")
            private Header header;

            @XmlElement
            private Metadata metadata;

            public Record() {
            }

            public Header getHeader() {
                return header;
            }

            public Metadata getMetadata() {
                return metadata;
            }

            public int getEntry() {
                return header.getEntry();
            }

            public Element getOaiMarc() {
                return metadata.getOaiMarc();
            }


            public static class Header {

                @XmlElement(name = "set_entry")
                private int entry;

                public Header() {
                }

                public int getEntry() {
                    return entry;
                }
            }
        }

        public static class Metadata {

//            @XmlElement(name = "oai_marc")
            @XmlAnyElement(lax=false)
            private Element oaiMarc;

            public Metadata() {
            }

            public Element getOaiMarc() {
                return oaiMarc;
            }
        }
    }

    @XmlRootElement(name = "find")
    public static class FindResponse {

        @XmlElement(name = "set_number")
        private String number;
        @XmlElement(name = "no_records")
        private int recordCount;
        @XmlElement(name = "no_entries")
        private int entryCount;

        public FindResponse() {
        }

        public int getEntryCount() {
            return entryCount;
        }

        public String getNumber() {
            return number;
        }

        public int getRecordCount() {
            return recordCount;
        }
    }
}
