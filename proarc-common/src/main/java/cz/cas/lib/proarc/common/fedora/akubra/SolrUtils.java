package cz.cas.lib.proarc.common.fedora.akubra;

import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.solr.common.SolrDocument;

public class SolrUtils {

    public enum SortOperation {DESC, ASC}

    ;

    public enum QueryOperator {AND, OR}

    ;

    public static StringBuilder appendAndValue(StringBuilder queryBuilder, String value) {
        return appendValue(queryBuilder, value, "AND");
    }

    public static StringBuilder appendORValue(StringBuilder queryBuilder, String value) {
        return appendValue(queryBuilder, value, "OR");
    }

    private static StringBuilder appendListValue(StringBuilder queryBuilder, String value) {
        return appendValue(queryBuilder, "\"" + value + "\"", "OR");
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
        return getListQuery(pids, "pid");
    }

    public static String getModelQuery(List<String> models) {
        return getListQuery(models, "model");
    }

    public static String getUserQuery(List<String> usernames, Boolean allowAllForUser) {
        if (allowAllForUser == Boolean.TRUE) {
            usernames.add("all");
        }
        return getListQuery(usernames, "user");
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

        item.setPid(getString(solrDocument, "pid"));
        item.setModel(getString(solrDocument, "model"));
        item.setOwner(getString(solrDocument, "owner"));
        item.setLabel(getString(solrDocument, "label"));
        item.setState(getString(solrDocument, "state"));
        item.setCreated(getDate(solrDocument, "created"));
        item.setModified(getDate(solrDocument, "modified"));
        item.setOrganization(getString(solrDocument, "organization"));
        item.setUser(getString(solrDocument, "user"));
        item.setStatus(getString(solrDocument, "status"));
        item.setK1(getString(solrDocument, "ndkExport"));
        item.setK2(getString(solrDocument, "krameriusExport"));
        item.setK3(getString(solrDocument, "archiveExport"));
        item.setK4(getString(solrDocument, "crossrefExport"));
        item.setK5(getBooleanString(solrDocument, "isLocked"));
        return item;
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
