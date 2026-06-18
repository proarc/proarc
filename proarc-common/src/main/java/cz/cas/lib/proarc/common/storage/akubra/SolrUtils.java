package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocument;

public class SolrUtils {

    public enum SortOperation {DESC, ASC}

    public enum QueryOperator {AND, OR}

    public static final String FIELD_PID = "pid";
    public static final String FIELD_PARENT_PID = "parentPid";
    public static final String FIELD_MODEL = "model";
    public static final String FIELD_OWNER = "owner";
    public static final String FIELD_LABEL = "label";
    public static final String FIELD_LABEL_SORT = "labelSort";
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
    public static final String FIELD_SOFTWARE = "software";
    public static final String FIELD_MEMBERS = "members";
    public static final String FIELD_SOURCE = "source";
    public static final String FIELD_PAGE_INDEX = "pageIndex";
    public static final String FIELD_PAGE_NUMBER = "pageNumber";
    public static final String FIELD_PAGE_TYPE = "pageType";
    public static final String FIELD_PAGE_POSITION = "pagePosition";
    public static final String FIELD_GENRE = "genre";
    public static final String FIELD_URNNBN = "urnNbn";
    public static final String FIELD_DESCRIPTION_STANDARD = "descriptionStandard";
    public static final String FIELD_FULLTEXT = "fulltext";
    public static final String FIELD_DATE = "date";
    public static final String FIELD_STREAM = "stream";
    public static final String FIELD_OPERATION = "operation";
    public static final String FIELD_VALIDATION_STATUS = "validationStatus";
    public static final String FIELD_VALIDATION_PROCES = "validationProcessId";
    public static final String FIELD_PART_NUMBER = "partNumber";
    public static final String FIELD_IDENTIFIRES = "identifiers";

    public static final String PROPERTY_STATE_ACTIVE = "Active";
    public static final String PROPERTY_STATE_DEACTIVE = "Deactive";

    public static final String VALIDATION_STATUS_OK = "OK";
    public static final String VALIDATION_STATUS_ERROR = "ERROR";
    public static final String VALIDATION_STATUS_UNKNOWN = "UNKNOWN";

    public static final String PROPERTY_PARENTPID_NO_PARENT = "NO_PARENT";

    public static StringBuilder appendAndValue(StringBuilder queryBuilder, String value) {
        return appendValue(queryBuilder, value, QueryOperator.AND.name());
    }

    public static StringBuilder appendORValue(StringBuilder queryBuilder, String value) {
        return appendValue(queryBuilder, value, QueryOperator.OR.name());
    }

    private static StringBuilder appendListValue(StringBuilder queryBuilder, String value) {
        return appendValue(queryBuilder, "\"" + value + "\"", QueryOperator.OR.name());
    }

    private static StringBuilder appendPidListValue(StringBuilder queryBuilder, String value) {
        return appendValue(queryBuilder, value.replaceAll(":", "\\\\:"), QueryOperator.OR.name());
    }

    private static StringBuilder appendValue(StringBuilder queryBuilder, String value, String operator) {
        if (value == null) {
            return queryBuilder;
        }
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
        return getListFilterQuery(pids.stream()
                .map(s -> s.startsWith("uuid:") ? s : null) // ochrana proti jiným stringům nez uuid
                .collect(Collectors.toList()), FIELD_PID);
    }

    public static String getIdentifiersQuery(List<String> pids) {
        return getListFilterQuery(pids.stream()
                .map(s -> s.startsWith("uuid:") ? null : s) // ochrana proti jiným stringům nez uuid
                .collect(Collectors.toList()), FIELD_IDENTIFIRES);
    }

    public static String getModelQuery(List<String> models) {
        return getListFilterQuery(models, FIELD_MODEL);
    }

    public static String getUserQuery(List<String> usernames) {
        return getListFilterQuery(usernames, FIELD_USER);
    }

    private static String getListQuery(List<String> list, String key) {
        StringBuilder queryBuilder = new StringBuilder();
        queryBuilder.append(key).append(":(");
        boolean isFirst = true;
        for (String value : list) {
            if (value == null || value.isEmpty()) {
                continue;
            }
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

    private static String getListFilterQuery(List<String> list, String key) {
        StringBuilder queryBuilder = new StringBuilder();
        boolean isFirst = true;
        for (String value : list) {
            if (value == null || value.isEmpty()) {
                continue;
            }
            if (isFirst) {
                isFirst = false;
                value = value.trim();
                value = ClientUtils.escapeQueryChars(value);
                if (value.contains("*")) {
                    value = value.replace("\\*", "*");
                    queryBuilder.append(key).append(":").append(value).append("");
                } else {
                    queryBuilder.append(key).append(":\"").append(value).append("\"");
                }
            } else {
                value = value.trim();
                value = ClientUtils.escapeQueryChars(value);
                if (value.contains("*")) {
                    value = value.replace("\\*", "*");
                    queryBuilder.append(" ").append(QueryOperator.OR.name()).append(" ").append(key).append(":").append(value);
                } else {
                    queryBuilder.append(" ").append(QueryOperator.OR.name()).append(" ").append(key).append(":\"").append(value).append("\"");
                }
            }
        }
        return queryBuilder.toString();
    }

    private static String getPidListQuery(List<String> list, String key) {
        StringBuilder queryBuilder = new StringBuilder();
        queryBuilder.append(key).append(":(");
        boolean isFirst = true;
        for (String value : list) {
            if (value == null || value.isEmpty()) {
                continue;
            }
            if (!FoxmlUtils.validPid(value)) {
                value += "*";
            }
            if (isFirst) {
                isFirst = false;
                queryBuilder.append(value.replaceAll(":", "\\\\:"));
            } else {
                queryBuilder = appendPidListValue(queryBuilder, value);
            }
        }
        queryBuilder.append(")");
        return queryBuilder.toString();
    }

    public static SearchViewItem createItem(SolrDocument solrDocument, Locale locale) {
        SearchViewItem item = new SearchViewItem();

        item.setPid(getString(solrDocument, FIELD_PID));
        item.setModel(getString(solrDocument, FIELD_MODEL));
        item.setParentPid(getString(solrDocument, FIELD_PARENT_PID));
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
        item.setArchiveExportPath(getString(solrDocument, FIELD_EXPORT_ARCHIVE));
        ;
        item.setK3(getContainsString(solrDocument, FIELD_EXPORT_ARCHIVE));
        item.setCrossrefExportPath(getString(solrDocument, FIELD_EXPORT_CROSSREF));
        ;
        item.setK4(getContainsString(solrDocument, FIELD_EXPORT_CROSSREF));
        item.setIsLocked(getBoolean(solrDocument, FIELD_LOCKED));
        item.setUrnNbn(getString(solrDocument, FIELD_URNNBN));
        item.setPartNumber(getString(solrDocument, FIELD_PART_NUMBER));
        item.setDescriptionStandard(getString(solrDocument, FIELD_DESCRIPTION_STANDARD));
        item.setValidationStatus(getString(solrDocument, FIELD_VALIDATION_STATUS));
        item.setValidationProcess(getInteger(solrDocument, FIELD_VALIDATION_PROCES));
        if (isPage(item.getModel())) {
            item.setPageIndex(getString(solrDocument, FIELD_PAGE_INDEX));
            item.setPageNumber(getString(solrDocument, FIELD_PAGE_NUMBER));
            item.setPageType(getString(solrDocument, FIELD_PAGE_TYPE));
            item.setPagePosition(getString(solrDocument, FIELD_PAGE_POSITION));
            item.setPageRepre(getString(solrDocument, FIELD_GENRE));
        } else if (isAudioPage(item.getModel())) {
            item.setPageIndex(getString(solrDocument, FIELD_PAGE_INDEX));
        }
        item.setK5(getContainsBoolean(solrDocument, FIELD_LOCKED));
        resolveObjectLabel(item, locale);
        return item;
    }

    private static void resolveObjectLabel(SearchViewItem item, Locale locale) {
        MetaModel model = MetaModelRepository.getInstance().find(item.getModel());
        if (model == null) {
            return;
        }
        SearchView.HasSearchViewHandler hasHandler = model.getPlugin().getHandlerProvider(SearchView.HasSearchViewHandler.class);
        if (hasHandler != null) {
            String labelNew = hasHandler.createSearchViewHandler().getObjectLabel(item, locale);
            item.setLabel(labelNew);
        }
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

    private static Integer getInteger(SolrDocument solrDocument, String key) {
        Integer value = (Integer) solrDocument.get(key);
        return (value == null || value < 1) ? null : value;
    }

    private static String getDate(SolrDocument solrDocument, String key) {
        Date date = (Date) solrDocument.get(key);
        if (date == null) {
            return null;
        }
        DateTimeFormatter formatterNew = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").withZone(ZoneId.of("UTC"));
        String dateAsString = formatterNew.format(date.toInstant());
        return dateAsString;
//        DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
//        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
//        String dateAsString = formatter.format(date);
//        return dateAsString;
    }

    public static String now() {
        DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
        return formatter.format(new Date());
    }

    public static void indexParentResult(SolrSearchView solrSearchView, SolrObjectFeeder solrObjectFeeder, String pid) throws DigitalObjectException, IOException {
        if (solrSearchView == null || solrObjectFeeder == null) {
            return;
        }
        List<SearchViewItem> parents = solrSearchView.findReferrers(pid);
        if (parents == null || parents.isEmpty() || parents.size() > 1) {
            return;
        }
        List<SearchViewItem> children = solrSearchView.findChildren(parents.get(0).getPid());
        if (children == null || children.isEmpty()) {
            return;
        }

        String status = VALIDATION_STATUS_OK;

        for (SearchViewItem child : children) {
            if (VALIDATION_STATUS_OK.equals(child.getValidationStatus())) {
                if (!status.equals(VALIDATION_STATUS_UNKNOWN)) {
                    status = VALIDATION_STATUS_OK;
                }
            } else if (VALIDATION_STATUS_ERROR.equals(child.getValidationStatus())) {
                status = VALIDATION_STATUS_ERROR;
                break;
            } else if (VALIDATION_STATUS_UNKNOWN.equals(child.getValidationStatus())) {
                status = VALIDATION_STATUS_UNKNOWN;
            }
        }
        solrObjectFeeder.feedValidationResult(parents.get(0).getPid(), null, status);
        indexParentResult(solrSearchView, solrObjectFeeder, parents.get(0).getPid());
    }
}
