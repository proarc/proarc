package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.webapp.server.rest.v1.ExportResourceV1;
import cz.cas.lib.proarc.webapp.server.rest.v2.ExportResource;
import org.glassfish.jersey.server.ApplicationHandler;
import org.glassfish.jersey.server.ResourceConfig;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

class ExportResourceModelTest {

    @Test
    void exportApiVersionsHaveUnambiguousResourceMethods() {
        ResourceConfig config = new ResourceConfig()
                .register(ExportResourceV1.class)
                .register(ExportResource.class);
        ApplicationHandler handler = assertDoesNotThrow(() -> new ApplicationHandler(config));
        handler.onShutdown(null);
    }
}
