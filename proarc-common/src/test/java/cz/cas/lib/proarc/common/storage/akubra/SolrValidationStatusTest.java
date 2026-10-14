package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.storage.SearchViewItem;
import java.util.Arrays;
import java.util.Collections;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.junit.jupiter.api.Test;

import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.VALIDATION_STATUS_ERROR;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.VALIDATION_STATUS_OK;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.VALIDATION_STATUS_UNKNOWN;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.VALIDATION_STATUS_WARNING;

class SolrValidationStatusTest {

    @Mocked SolrSearchView search;
    @Mocked SolrObjectFeeder feeder;

    @Test
    void parentRetainsWarningRegardlessOfChildOrder() throws Exception {
        assertParentStatus(VALIDATION_STATUS_WARNING, VALIDATION_STATUS_WARNING, VALIDATION_STATUS_OK);
        assertParentStatus(VALIDATION_STATUS_WARNING, VALIDATION_STATUS_OK, VALIDATION_STATUS_WARNING);
    }

    @Test
    void parentErrorTakesPrecedenceOverWarning() throws Exception {
        assertParentStatus(VALIDATION_STATUS_ERROR, VALIDATION_STATUS_WARNING, VALIDATION_STATUS_ERROR);
        assertParentStatus(VALIDATION_STATUS_ERROR, VALIDATION_STATUS_ERROR, VALIDATION_STATUS_WARNING);
    }

    @Test
    void parentRemainsUnknownUntilAllChildrenAreValidated() throws Exception {
        assertParentStatus(VALIDATION_STATUS_UNKNOWN, VALIDATION_STATUS_WARNING, VALIDATION_STATUS_UNKNOWN);
        assertParentStatus(VALIDATION_STATUS_UNKNOWN, VALIDATION_STATUS_UNKNOWN, VALIDATION_STATUS_WARNING);
    }

    private void assertParentStatus(String expected, String first, String second) throws Exception {
        SearchViewItem parent = new SearchViewItem();
        String parentPid = "uuid:parent:" + first + ":" + second;
        parent.setPid(parentPid);
        SearchViewItem child1 = new SearchViewItem();
        child1.setValidationStatus(first);
        SearchViewItem child2 = new SearchViewItem();
        child2.setValidationStatus(second);
        new Expectations() {{
            search.findReferrers("uuid:child"); result = Collections.singletonList(parent);
            search.findChildren(parentPid); result = Arrays.asList(child1, child2);
            search.findReferrers(parentPid); result = Collections.emptyList();
        }};
        SolrUtils.indexParentResult(search, feeder, "uuid:child");
        new Verifications() {{ feeder.feedValidationResult(parentPid, null, expected); times = 1; }};
    }
}
