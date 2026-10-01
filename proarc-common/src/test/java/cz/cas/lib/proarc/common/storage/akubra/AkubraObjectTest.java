package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AkubraObjectTest {

    @Test
    void getModelLoadsAndCachesModelFromSolr() {
        AtomicInteger queries = new AtomicInteger();
        SolrSearchView searchView = new SolrSearchView(null, null) {
            @Override
            public String findModel(String pid) {
                queries.incrementAndGet();
                assertEquals("uuid:test", pid);
                return "model:test";
            }
        };
        AkubraObject object = new AkubraObject(null, null, null, searchView, "uuid:test");

        assertEquals("model:test", object.getModel());
        assertEquals("model:test", object.getModel());
        assertEquals(1, queries.get());
    }

    @Test
    void setModelDoesNotQuerySolr() {
        AtomicInteger queries = new AtomicInteger();
        SolrSearchView searchView = new SolrSearchView(null, null) {
            @Override
            public String findModel(String pid) {
                queries.incrementAndGet();
                return "model:solr";
            }
        };
        AkubraObject object = new AkubraObject(null, null, null, searchView, "uuid:test");

        object.setModel("model:set");

        assertEquals("model:set", object.getModel());
        assertEquals(0, queries.get());
    }
}
