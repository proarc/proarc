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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.z3950;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.dom.DOMResult;
import org.jzkit.a2j.codec.util.OIDRegister;
import org.jzkit.a2j.gen.AsnUseful.EXTERNAL_type;
import org.jzkit.search.util.QueryModel.InvalidQueryException;
import org.jzkit.search.util.QueryModel.PrefixString.PrefixString;
import org.jzkit.search.util.QueryModel.QueryModel;
import org.jzkit.z3950.Z3950Exception;
import org.jzkit.z3950.client.SynchronousOriginBean;
import org.jzkit.z3950.client.ZClient;
import org.jzkit.z3950.gen.v3.Z39_50_APDU_1995.InitializeResponse_type;
import org.jzkit.z3950.gen.v3.Z39_50_APDU_1995.NamePlusRecord_type;
import org.jzkit.z3950.gen.v3.Z39_50_APDU_1995.PresentResponse_type;
import org.jzkit.z3950.gen.v3.Z39_50_APDU_1995.Records_type;
import org.jzkit.z3950.gen.v3.Z39_50_APDU_1995.SearchResponse_type;
import org.jzkit.z3950.gen.v3.Z39_50_APDU_1995.record_inline13_type;
import org.jzkit.z3950.util.Z3950Constants;
import org.marc4j.MarcReader;
import org.marc4j.MarcXmlWriter;
import org.marc4j.marc.Record;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.w3c.dom.Document;

/**
 * Z3950Client uses prefix queries to search meta data in Marc21 format.
 * It wraps {@link SynchronousOriginBean} the same way as {@link ZClient}.
 *
 * <p>Not thread safe!</p>
 * <p>See  {@code src/main/resources/log4j.properties} to configure jzkit logging.</p>
 *
 * @author Jan Pokorsky
 */
public final class Z3950Client {

    private static final Logger LOG = Logger.getLogger(Z3950Client.class.getName());
    private static Level LEVEL = Level.FINE;
    private static final int MAX_RESULTS = 100;
    private static final int SEARCH_PAGE_SIZE = MAX_RESULTS;
    private final SynchronousOriginBean client;
    private final int port;
    private final String host;
    private String query;
    private String base;
    private String recordFormat;
    private final String current_result_set_name = "RS0";

    public Z3950Client(String host, int port, String base) {
        ClassPathXmlApplicationContext c = new ClassPathXmlApplicationContext("/z3950/Z3950ApplicationContext.xml");
        client = new SynchronousOriginBean(new OIDRegister("/a2j.properties"));
        client.setApplicationContext(c);
        this.host = host;
        this.port = port;
        this.recordFormat = "marc21";
        this.base = base;
    }

    public void close() {
        client.disconnect();
    }

    /**
     * Queries Z39.50 server.
     * @param query RPN query
     * @return found records in Marc21 format
     * @see <a href='http://www.indexdata.com/zebra/doc/querymodel-rpn.html'>
     *      RPN queries and semantics</a>
     */
    public Iterable<byte[]> search(String query) throws Z3950ClientException {
        this.query = query;
        try {
            InitializeResponse_type conn = connect(host, port);
            logConnection(conn, host, port);
            if (!conn.result.booleanValue()) {
                throw new Z3950ClientException(this, "Connection failed.");
            }

            SearchResponse_type search = find(query);
            logSearchResponse(search);
            List<NamePlusRecord_type> result = Collections.emptyList();
            if (search.records != null && search.numberOfRecordsReturned.intValue() > 0) {
                result = getRecords(search.records);
            }
            SearchResult searchResult = new SearchResult(
                    Math.min(MAX_RESULTS, search.resultCount.intValue()),
                    search.nextResultSetPosition.intValue(),
                    result);
            return searchResult;
        } catch (Z3950ClientException ex) {
            throw ex; // do not wrap
        } catch (Exception ex) {
            throw new Z3950ClientException(this, ex);
        }
    }

    private InitializeResponse_type connect(String host, int port) {
        client.clearAllDatabases();
        client.addDatatabse(base);
        client.setRecordSyntax(recordFormat);
        InitializeResponse_type resp = client.connect(host, port);
        return resp;
    }

    private SearchResponse_type find(String query) throws Z3950Exception, InvalidQueryException {
        QueryModel qm = new PrefixString(query);
        SearchResponse_type resp = client.sendSearch(qm, null, current_result_set_name);
        return resp;
    }

    private PresentResponse_type fetchRecords(int resultIndex, int pageSize) {
        PresentResponse_type resp = client.sendPresent(resultIndex, pageSize, "F", current_result_set_name);
        logPresentResponse(resp);
        return resp;
    }

    private List<NamePlusRecord_type> getRecords(PresentResponse_type resp) {
        if (resp.numberOfRecordsReturned.intValue() > 0) {
            Records_type records = resp.records;
            return getRecords(records);
        }
        return Collections.emptyList();
    }
    
    private List<NamePlusRecord_type> getRecords(Records_type records) {
        if (records.which == Records_type.responserecords_CID) {
            @SuppressWarnings("unchecked")
            List<NamePlusRecord_type> l = (List<NamePlusRecord_type>) records.o;
            return l;
        } else {
            throw new IllegalStateException("Unexpected Response Record CID: " + records.which);
        }
    }

    private static byte[] process(NamePlusRecord_type npr) throws UnsupportedEncodingException {
        LOG.log(LEVEL, String.format("NPR: name: %s, type: %s,\ncontent: %s", npr.name, npr.record.which, npr.record.o));
        if (npr.record.which == record_inline13_type.retrievalrecord_CID) {
            EXTERNAL_type external = (EXTERNAL_type) npr.record.o;
            LOG.log(LEVEL, String.format("External Descriptor: %s", external.data_value_descriptor));
            byte[] content = (byte[]) external.encoding.o;
//            LOG.info("Content:\n" + new String(content, "cp1250"));
            return content;
        }
        throw new IllegalStateException("Unexpected record type: " + npr.record.which);
    }

    /** Converts Marc21 to MarcXML */
    public static Document toMarcXml(byte[] marc21, String charset) {
        MarcReader reader = new MyMarcStreamReader(new ByteArrayInputStream(marc21), charset);
        DOMResult result = new DOMResult();
        MarcXmlWriter writer = new MarcXmlWriter(result);
        if (reader.hasNext()) {
            Record record = reader.next();
            writer.write(record);
        }
        writer.close();

        Document retval = (Document) result.getNode();
        return retval;
    }

    private static void logPresentResponse(PresentResponse_type resp) {
        if (resp == null) {
            throw new NullPointerException();
        }
        if (!LOG.isLoggable(LEVEL)) {
            return ;
        }
        LOG.log(LEVEL, String.format(
                "Reference ID: %s"
                    + ",\nPresent Status: %s"
                    + ",\nNumber of Records: %s"
                    + ",\nNext RS Position: %s"
                    + ",\nOther Info: %s",
                toString(resp.referenceId),
                resp.presentStatus,
                resp.numberOfRecordsReturned,
                resp.nextResultSetPosition,
                resp.otherInfo));
    }

    private static void logSearchResponse(SearchResponse_type resp) {
        if (resp == null) {
            throw new NullPointerException();
        }
        if (!LOG.isLoggable(LEVEL)) {
            return ;
        }
        LOG.log(LEVEL, String.format(
                "Reference ID: %s"
                    + "\nSearch Status: %s"
                    + "\nResult Count: %s"
                    + "\nNum Records Returned: %s"
                    + "\nNext RS position: %s",
                toString(resp.referenceId),
                resp.searchStatus,
                resp.resultCount,
                resp.numberOfRecordsReturned,
                resp.nextResultSetPosition));
    }

    private static void logConnection(InitializeResponse_type resp, String host, int port) {
        if (!resp.result.booleanValue()) {
            LOG.log(Level.SEVERE, "Connection failed! {0}:{1}", new Object[]{host, port});
            return ;
        }
        if (!LOG.isLoggable(LEVEL)) {
            return ;
        }
        StringBuilder options = new StringBuilder();
        for (int i = 0; i < Z3950Constants.z3950_option_names.length; i++) {
            if (options.length() > 0) {
                options.append(", ");
            }
            if (resp.options.isSet(i)) {
                options.append(Z3950Constants.z3950_option_names[i]);
            }
        }

        LOG.log(LEVEL, String.format(
                "Reference ID: %s"
                    + ",\nImplementation ID: %s"
                    + ",\nName: %s"
                    + ",\nVersion: %s"
                    + ",\nOptions: %s",
                toString(resp.referenceId),
                resp.implementationId,
                resp.implementationName,
                resp.implementationVersion,
                options
                ));

    }

    @Override
    public String toString() {
        return String.format("Z3950Client{%s:%s/%s, format: %s, query: %s}",
                host, port, base, recordFormat, query);
    }

    private static String toString(byte[] b) {
        return b == null ? String.valueOf(b) : new String(b);
    }

    private final class SearchResult implements Iterator<byte[]>, Iterable<byte[]> {

        private int resultCount;
        private List<NamePlusRecord_type> records;
        private int resultIndex = 1;
        private int recordIndex = 0;

        public SearchResult(int resultCount, int resultIndex, List<NamePlusRecord_type> records) {
            this.resultCount = resultCount;
            this.resultIndex = resultIndex;
            this.records = records;
        }

        @Override
        public boolean hasNext() {
            return resultCount > 0;
        }

        @Override
        public byte[] next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }

            try {
                if (records.isEmpty() || recordIndex >= records.size()) {
                    PresentResponse_type resp = fetchRecords(resultIndex, Math.min(SEARCH_PAGE_SIZE, resultCount));
                    records = getRecords(resp);
                    recordIndex = 0;
                    if (records.isEmpty()) {
                        throw new IllegalStateException(
                                String.format("resultCount: %s, resultIndex: %s", resultCount, resultIndex));
                    }
                    resultIndex = resp.nextResultSetPosition.intValue();
                }
                --resultCount;
                NamePlusRecord_type record = records.get(recordIndex++);
                return process(record);
            } catch (Exception ex) {
                throw new IllegalStateException(this.toString(), ex);
            }
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException("Not supported.");
        }

        @Override
        public Iterator<byte[]> iterator() {
            return this;
        }

    }

}
