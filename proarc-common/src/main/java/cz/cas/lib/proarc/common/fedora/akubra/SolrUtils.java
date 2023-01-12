package cz.cas.lib.proarc.common.fedora.akubra;

import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.solr.common.SolrDocument;

public class SolrUtils {

    public enum SortOperation {DESC, ASC};

    public enum QueryOperator {AND, OR};

    public static final String FIELD_PID = "pid";
    public static final String FIELD_MODEL = "model";
    public static final String FIELD_OWNER = "owner";
    public static final String FIELD_LABEL = "label";
    public static final String FIELD_STATE = "state";
    public static final String FIELD_CREATED = "created";
    public static final String FIELD_MODIFIED = "modified";
    public static final String FIELD_ORGANIZATION = "organization";
    public static final String FIELD_USER = "user";
    public static final String FIELD_STATUS = "status";
    public static final String FIELD_EXPORT_NDK = "ndkExport";
    public static final String FIELD_EXPORT_KRAMERIUS = "krameriusExport";
    public static final String FIELD_EXPORT_ARCHIVE = "archiveExport";
    public static final String FIELD_EXPORT_CROSSREF = "crossrefExport";
    public static final String FIELD_LOCKED = "isLocked";
    public static final String FIELD_DEVICE = "device";
    public static final String FIELD_MEMBERS = "members";
    public static final String FIELD_SOURCE = "source";
    public static final String FIELD_PAGE_INDEX = "pageIndex";
    public static final String FIELD_PAGE_NUMBER = "pageNumber";
    public static final String FIELD_PAGE_TYPE = "pageType";

    public static StringBuilder appendAndValue(StringBuilder queryBuilder, String value) {
        return appendValue(queryBuilder, value, QueryOperator.AND.name());
    }

    public static StringBuilder appendORValue(StringBuilder queryBuilder, String value) {
        return appendValue(queryBuilder, value, QueryOperator.OR.name());
    }

    private static StringBuilder appendListValue(StringBuilder queryBuilder, String value) {
        return appendValue(queryBuilder, "\"" + value + "\"", QueryOperator.OR.name());
    }

    private static StringBuilder appendValue(StringBuilder queryBuilder, String value, String operator) {
        if (queryBuilder == null) {
            queryBuilder = new StringBuilder();
        }

        if (queryBuilder.toString().isEmpty()) {
            queryBuilder.append(value.trim());
        } else {
            queryBuilder.append(" ").append(operator.trim()).append(" ").append(value.trim());
        }
        return queryBuilder;
    }

    public static List<String> toList(String value1, String value2) {
        List<String> list = new ArrayList<>();
        list.add(value1);
        list.add(value2);
        return list;
    }

    public static SolrUtils.SortOperation transfromSort(String sortOperation) {
        if (sortOperation == null || sortOperation.isEmpty()) {
            return null;
        }
        if ("ASC".equals(sortOperation.toUpperCase())) {
            return SolrUtils.SortOperation.ASC;
        } else if ("DESC".equals(sortOperation.toUpperCase())) {
            return SolrUtils.SortOperation.DESC;
        } else {
            return null;
        }
    }

    public static String getPidsQuery(List<String> pids) {
        return getListQuery(pids, FIELD_PID);
    }

    public static String getModelQuery(List<String> models) {
        return getListQuery(models, FIELD_MODEL);
    }

    public static String getUserQuery(List<String> usernames, Boolean allowAllForUser) {
        if (allowAllForUser == Boolean.TRUE) {
            usernames.add("all");
        }
        return getListQuery(usernames, FIELD_USER);
    }

    private static String getListQuery(List<String> list, String key) {
        StringBuilder queryBuilder = new StringBuilder();
        queryBuilder.append(key).append(":(");
        boolean isFirst = true;
        for (String value : list) {
            if (isFirst) {
                isFirst = false;
                queryBuilder.append("\"" + value + "\"");
            } else {
                queryBuilder = appendListValue(queryBuilder, value);
            }
        }
        queryBuilder.append(")");
        return queryBuilder.toString();
    }

    public static SearchViewItem createItem(SolrDocument solrDocument) {
        SearchViewItem item = new SearchViewItem();

        item.setPid(getString(solrDocument, FIELD_PID));
        item.setModel(getString(solrDocument, FIELD_MODEL));
        item.setOwner(getString(solrDocument, FIELD_OWNER));
        item.setLabel(getString(solrDocument, FIELD_LABEL));
        item.setState(getString(solrDocument, FIELD_STATE));
        item.setCreated(getDate(solrDocument, FIELD_CREATED));
        item.setModified(getDate(solrDocument, FIELD_MODIFIED));
        item.setOrganization(getString(solrDocument, FIELD_ORGANIZATION));
        item.setUser(getString(solrDocument, FIELD_USER));
        item.setStatus(getString(solrDocument, FIELD_STATUS));
        item.setNdkExportPath(getString(solrDocument, FIELD_EXPORT_NDK));
        item.setK0(getContainsString(solrDocument, FIELD_EXPORT_NDK, FIELD_EXPORT_KRAMERIUS, FIELD_EXPORT_ARCHIVE, FIELD_EXPORT_CROSSREF));
        item.setK1(getContainsString(solrDocument, FIELD_EXPORT_NDK));
        item.setKrameriusExportPath(getString(solrDocument, FIELD_EXPORT_KRAMERIUS));
        item.setK2(getContainsString(solrDocument, FIELD_EXPORT_KRAMERIUS));
        item.setArchiveExportPath(getString(solrDocument, FIELD_EXPORT_ARCHIVE));;
        item.setK3(getContainsString(solrDocument, FIELD_EXPORT_ARCHIVE));
        item.setCrossrefExportPath(getString(solrDocument, FIELD_EXPORT_CROSSREF));;
        item.setK4(getContainsString(solrDocument, FIELD_EXPORT_CROSSREF));
        item.setIsLocked(getBoolean(solrDocument, FIELD_LOCKED));
        if (isPage(item.getModel())) {
            item.setPageIndex(getString(solrDocument, FIELD_PAGE_INDEX));
            item.setPageNumber(getString(solrDocument, FIELD_PAGE_NUMBER));
            item.setPageType(getString(solrDocument, FIELD_PAGE_TYPE));
        } else if (isAudioPage(item.getModel())) {
            item.setPageIndex(getString(solrDocument, FIELD_PAGE_INDEX));
        }
        item.setK5(getContainsBoolean(solrDocument, FIELD_LOCKED));
        return item;
    }

    public static boolean isAudioPage(String model) {
        return NdkAudioPlugin.MODEL_PAGE.equals(model);
    }

    public static boolean isPage(String model) {
        return NdkPlugin.MODEL_PAGE.equals(model) || NdkPlugin.MODEL_NDK_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model);
    }

    private static String getContainsString(SolrDocument solrDocument, String... keys) {
        for (String key : keys) {
            String value = getString(solrDocument, key);
            if (value != null && !value.isEmpty()) {
                return "1";
            }
        }
        return "0";
    }

    private static String getContainsBoolean(SolrDocument solrDocument, String... keys) {
        for (String key : keys) {
            Boolean value = getBoolean(solrDocument, key);
            if (value != null && Boolean.TRUE.equals(value)) {
                return "1";
            }
        }
        return "0";
    }

    private static Boolean getBoolean(SolrDocument solrDocument, String key) {
        String value = getBooleanString(solrDocument, key);
        return value == null || value.isEmpty() ? false : Boolean.parseBoolean(value);
    }

    private static String getBooleanString(SolrDocument solrDocument, String key) {
        Boolean value = (Boolean) solrDocument.get(key);
        return value == null ? null : String.valueOf(value);
    }

    private static String getString(SolrDocument solrDocument, String key) {
        return (String) solrDocument.get(key);
    }

    private static String getDate(SolrDocument solrDocument, String key) {
        Date date = (Date) solrDocument.get(key);
        if (date == null) {
            return null;
        }
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
        String dateAsString = simpleDateFormat.format(date);
        return dateAsString;
    }


}
