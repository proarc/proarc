package cz.cas.lib.proarc.webapp.server.rest;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.List;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ProArcRequestTest {

    @Test
    void deserializesDistributeMembersTargetsAndReindexFlag() throws Exception {
        String json = """
                {
                  "srcPid": "uuid:source",
                  "runReindex": true,
                  "targets": [
                    {"dstPid": "uuid:target-1", "pid": ["uuid:page-1", "uuid:page-2"]},
                    {"dstPid": "uuid:target-2", "pid": ["uuid:page-3"]}
                  ]
                }
                """;

        ProArcRequest.DistributeMembersRequest request = new ObjectMapper()
                .readValue(json, ProArcRequest.DistributeMembersRequest.class);

        assertEquals("uuid:source", request.srcParentPid);
        assertTrue(request.runReindex);
        assertEquals(2, request.targets.size());
        assertEquals("uuid:target-1", request.targets.get(0).dstParentPid);
        assertEquals(List.of("uuid:page-1", "uuid:page-2"), request.targets.get(0).pids);
    }
}
