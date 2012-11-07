/*
 * Copyright (C) 2011 Jan Pokorsky
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
package cz.incad.pas.editor.client;

import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.Node;
import com.google.gwt.dom.client.NodeList;
import com.google.gwt.regexp.shared.RegExp;
import com.google.gwt.regexp.shared.SplitResult;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.JSONEncoder;
import com.smartgwt.client.widgets.tile.TileGrid;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * GWT client helper utilities.
 *
 * @author Jan Pokorsky
 */
public final class ClientUtils {

    private static final Logger LOG = Logger.getLogger(ClientUtils.class.getName());

    /**
     * Simplified version of {@link String#format(java.lang.String, java.lang.Object[]) String.format}
     * For now it supports only {@code %s} format specifier.
     */
    public static String format(String format, Object... args) {
        RegExp re = RegExp.compile("%s");
        SplitResult split = re.split(format);
        StringBuilder sb = new StringBuilder();
        sb.append(split.get(0));
        for (int i = 1; i < split.length(); i++) {
            sb.append(args[i - 1]);
            sb.append(split.get(i));
        }
        return sb.toString();
    }

    /**
     * Logs a formatted message.
     * @param logger logger
     * @param level logging level
     * @param format see {@link #format(java.lang.String, java.lang.Object[]) format} doc
     * @param args arguments referenced by the format specifiers
     */
    public static void log(Logger logger, Level level, String format, Object... args) {
        if (logger.isLoggable(level)) {
            logger.log(level, format(format, args));
        }
    }

    /** Info {@link #log(java.util.logging.Logger, java.util.logging.Level, java.lang.String, java.lang.Object[]) log}. */
    public static void info(Logger logger, String format, Object... args) {
        log(logger, Level.INFO, format, args);
    }

    /** Fine {@link #log(java.util.logging.Logger, java.util.logging.Level, java.lang.String, java.lang.Object[]) log}. */
    public static void fine(Logger logger, String format, Object... args) {
        log(logger, Level.FINE, format, args);
    }

    /** Severe {@link #log(java.util.logging.Logger, java.util.logging.Level, java.lang.String, java.lang.Object[]) log}. */
    public static void severe(Logger logger, String format, Object... args) {
        log(logger, Level.SEVERE, format, args);
    }

    /** Warning {@link #log(java.util.logging.Logger, java.util.logging.Level, java.lang.String, java.lang.Object[]) log}. */
    public static void warning(Logger logger, String format, Object... args) {
        log(logger, Level.WARNING, format, args);
    }

    /**
     * Dumps Element content and traverse its children.
     * <p/><b>WARNING:</b> it is com.google.gwt.dom.client.Element not com.google.gwt.xml.client.Element!!!
     * 
     * @param elm an element to dump
     * @param indent row indentation for current level
     * @param indentIncrement increment for next level
     * @param sb dumped content
     * @return dumped content
     */
    public static StringBuilder dump(Element elm, String indent, String indentIncrement, StringBuilder sb) {
        int childCount = elm.getChildCount();
        String innerText = elm.getInnerText();
        String lang = elm.getLang();
        String nodeName = elm.getNodeName();
        short nodeType = elm.getNodeType();
        String getString = elm.getString();
        String tagNameWithNS = elm.getTagName();
        String xmlLang = elm.getAttribute("xml:lang");

        sb.append(ClientUtils.format("%sElement {nodeName: %s, nodeType: %s, tagNameWithNS: %s, lang: %s,"
                + " childCount: %s, getString: %s, xmlLang: %s}\n",
                indent, nodeName, nodeType, tagNameWithNS, lang, childCount, getString, xmlLang));
        NodeList<Node> childNodes = elm.getChildNodes();
        indent += indentIncrement;
        for (int i = 0; i < childNodes.getLength(); i++) {
            Node child = childNodes.getItem(i);
            if (Element.is(child)) {
                dump(Element.as(child), indent, indentIncrement, sb);
            } else {
                sb.append(ClientUtils.format("%sNode: nodeType: %s, nodeName: %s, childCount: %s, nodeValue: %s\n",
                        indent, child.getNodeType(), child.getNodeName(), child.getChildCount(), child.getNodeValue()));
            }
        }
        return sb;
    }
    
    public static String dump(Record r, String msg) {
        StringBuilder sb = new StringBuilder();
        if (r == null) {
            return ClientUtils.format("%s, record is NULL", msg);
        }
        sb.append(ClientUtils.format("%s, getAttributes:\n", msg));
        for (String attr : r.getAttributes()) {
            try {
                Object value = r.getAttributeAsObject(attr);
                sb.append(ClientUtils.format("  attr: %s, value: %s, class: %s\n", attr, value, safeGetClass(value)));
            } catch (Exception ex) {
                String value = r.getAttribute(attr);
                sb.append(ClientUtils.format("  !FAILED: attr: %s, value: %s, class: %s\n", attr, value, safeGetClass(value)));
//                Logger.getLogger("").log(Level.SEVERE, attr, ex);
            }
        }

        sb.append("-- toMap:\n");
        Map<?, ?> m;
        try {
            m = r.toMap();
        } catch (Exception ex) {
//            Logger.getLogger("").log(Level.SEVERE, "Record.toMap", ex);
            sb.append("Record.toMap FAILED");
            return sb.toString();
        }
        dump(m, "  ", "  ", sb);

        for (Map.Entry<?, ?> e : m.entrySet()) {
            Object value = e.getValue();
            sb.append(ClientUtils.format("  map.key: %s, value: %s, value class %s\n", e.getKey(), value, safeGetClass(value)));
            if (value instanceof List) {
                List<?> l = (List) value;
                for (Object valItem : l) {
                    sb.append(ClientUtils.format("    item.value: %s, value class %s\n", valItem, safeGetClass(valItem)));
                }
            }
        }

        return sb.toString();
    }

    public static StringBuilder dump(List<?> l, String indent, String indentIncrement, StringBuilder sb) {
        for (Object valItem : l) {
            sb.append(ClientUtils.format("%sitem.value: %s, value class %s\n", indent, valItem, safeGetClass(valItem)));
        }
        return sb;
    }

    public static StringBuilder dump(Map<?, ?> m, String indent, String indentIncrement, StringBuilder sb) {
        for (Map.Entry<?, ?> e : m.entrySet()) {
            Object value = e.getValue();
            sb.append(ClientUtils.format("%smap.key: %s, value: %s, value class %s\n", indent, e.getKey(), value, safeGetClass(value)));
            if (value instanceof List) {
                dump((List) value, indent, indentIncrement, sb);
            } else if (value instanceof Map) {
                dump((Map) value, indent + indentIncrement, indentIncrement, sb);
            }
        }
        return sb;
    }

    /** dumps object in JSON */
    public static String dump(Object jso) {
        String dump;
        if (jso != null) {
            try {
                dump = new JSONEncoder().encode(jso);
            } catch (Exception ex) {
                // this occurs in development mode sometimes; log it silently
                dump = String.valueOf(jso) + ", NPE: raise log level for details.";
                LOG.log(Level.FINE, dump, ex);
            }
        } else {
            dump = String.valueOf(jso);
        }
        return dump;
    }

    /**
     * Same like {@code value.geClass()} but prevents {@link NullPointerException}.
     *
     * @return class or {@code null}
     */
    public static Class<?> safeGetClass(Object value) {
        return value != null ? value.getClass() : null;
    }

    /**
     * Helper to scroll to a particular tile.
     * @param grid a tile grid
     * @param tileIndex index of the tile
     */
    public static void scrollToTile(TileGrid grid, int tileIndex) {
        int tileHeight = grid.getTileHeight();
        int tilesPerLine = grid.getTilesPerLine();
        int tileHMargin = grid.getTileHMargin();

        int tileRow = tileIndex / tilesPerLine - 1;
        int tileColumn = tileIndex % tilesPerLine;
        int top = tileHMargin / 2 + (tileRow * (tileHeight + tileHMargin));
        grid.scrollTo(1, top);
    }

    /**
     * Removes all attributes with {@code null} value.
     *
     * Useful before passing record to {@link DataSource} that encodes {@code null}
     * attributes as {@code "name":"null"} in JSON.
     * @param r record to process
     * @return copy of the record without {@code null} attributes
     */
    public static Record removeNulls(Record r) {
        boolean hasNull = false;
        HashMap<Object, Object> nonNunlls = new HashMap<Object, Object>();
        Map<?, ?> recordMap = r.toMap();
        for (Map.Entry<?, ?> entry : recordMap.entrySet()) {
            if (entry.getValue() != null) {
                nonNunlls.put(entry.getKey(), entry.getValue());
            } else {
                hasNull = true;
            }
        }
        return hasNull ? new Record(nonNunlls) : r;
    }

    /**
     * Replacement for {@link ResultSet#getRange(int, int) } to work around
     * {@link ArrayStoreException} thrown in production mode.
     * 
     * @see <a href='http://forums.smartclient.com/showpost.php?p=85402&postcount=34'>proposed workaround</a>
     * @see <a href='http://forums.smartclient.com/showpost.php?p=85402&postcount=38'>Fixed in SmartGWT 3.1</a>
     * @since SmartGWT 3.0
     */
    public static void getRangeWorkAround(ResultSet resultSet, int start, int end) {
        try {
            resultSet.getRange(start, end);
        } catch (ArrayStoreException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public static final BooleanCallback EMPTY_BOOLEAN_CALLBACK = new BooleanCallback() {

        @Override
        public void execute(Boolean value) {
            // no op
        }
    };

    public static final class DataSourceFieldBuilder<T extends DataSourceField> {

        public static final OperatorId[] TEXT_OPERATIONS = {
            OperatorId.EQUALS, OperatorId.NOT_EQUAL,
            OperatorId.ICONTAINS, OperatorId.INOT_CONTAINS,
            OperatorId.ISTARTS_WITH, OperatorId.IENDS_WITH};

        private final T field;

        public static <T extends DataSourceField> DataSourceFieldBuilder<T> field(T field) {
            return new DataSourceFieldBuilder<T>(field);
        }

        public DataSourceFieldBuilder(T field) {
            this.field = field;
        }

        public DataSourceFieldBuilder<T> required() {
            field.setRequired(true);
            return this;
        }

        public DataSourceFieldBuilder<T> primaryKey() {
            field.setPrimaryKey(true);
            return this;
        }

        public DataSourceFieldBuilder<T> hidden() {
            field.setHidden(true);
            return this;
        }

        public DataSourceFieldBuilder<T> filter(boolean filter) {
            field.setCanFilter(filter);
            return this;
        }

        public DataSourceFieldBuilder<T> validOperators(OperatorId... ids) {
            field.setValidOperators(ids);
            return this;
        }
        
        public T build() {
            return field;
        }

    }

    /**
     * Helper to wait on running asynchronous tasks.
     * It runs as soon as all expectations are released.
     */
    public static abstract class SweepTask implements Runnable, BooleanCallback {

        private int semaphore = 0;

        /**
         * Provides task implementation.
         */
        protected abstract void processing();

        /**
         * Call to expect further async task.
         */
        public SweepTask expect() {
            semaphore++;
            return this;
        }

        /**
         * Call when some async task is done.
         */
        public void release() {
            semaphore--;
            if (semaphore == 0) {
                processing();
            }
        }

        /**
         * Async call of release in case the value is true.
         */
        @Override
        public void execute(Boolean value) {
            if (Boolean.TRUE.equals(value)) {
                release();
            }
        }

        /**
         * Async call of release.
         * @param value
         */
        @Override
        public void run() {
            release();
        }


    }

}
