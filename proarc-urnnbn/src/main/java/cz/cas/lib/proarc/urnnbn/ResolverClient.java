/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.urnnbn;

import cz.cas.lib.proarc.urnnbn.model.registration.Import;
import cz.cas.lib.proarc.urnnbn.model.response.Response;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.ResponseProcessingException;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.glassfish.jersey.logging.LoggingFeature;

/**
 * The resolver HTTP client.
 *
 * @author Jan Pokorsky
 */
public final class ResolverClient {

    /**
     * The supported API version.
     */
    public static final String API_VERSION = "v3";

    private static final Logger LOG = Logger.getLogger(ResolverClient.class.getName());

    private Client httpClient;
    private final String serviceUrl;
    private final String registrar;
    private final Long archiver;
    private final String user;
    private final String passwd;

    public ResolverClient(String serviceUrl, String registrar, Long archiver,
            String user, String passwd) {

        this.serviceUrl = serviceUrl;
        this.registrar = registrar;
        this.archiver = archiver;
        this.user = user;
        this.passwd = passwd;
    }

    public String getDigitalInstances() {
        String response = resource().path("digitalInstances").request().get(String.class);
        return response;
    }

    /**
     * Registers an digital document to get URN:NBN.
     * <p>{@code POST http://resolver.nkp.cz/v4/registrars/boa001/digitalDocuments}
     * @param object a digital document
     * @return the resolver response
     */
    public Response registerObject(Import object) {
        if (registrar == null || registrar.isEmpty()) {
            throw new IllegalArgumentException("registrar");
        }
        if (object == null) {
            throw new NullPointerException("object");
        }
        if (archiver != null && object.getDigitalDocument().getArchiverId() == null) {
            object.getDigitalDocument().setArchiverId(archiver);
        }

        Response response = null;
        try {
            response = resource()
                    .path("registrars")
                    .path(registrar)
                    .path("digitalDocuments")
                    .request()
                        .post(Entity.entity(object, MediaType.APPLICATION_XML_TYPE), Response.class);
        } catch (WebApplicationException ex) {
            response = readResponseError(ex.getResponse(), ex);
        } catch (ResponseProcessingException ex) {
            response = readResponseError(ex.getResponse(), ex);
        }
        return response;
    }

    public Response removeUuidCzidloRecord(String urnNbn) {
        return removeIdentifierCzidloRecord(urnNbn, "uuid");
    }

    public Response removeIdentifierCzidloRecord(String urnNbnValue, String identifier) {
        if (urnNbnValue == null || urnNbnValue.isEmpty()) {
            throw new IllegalArgumentException("urnNbn");
        }

        Response response = null;
        try {
            response = resource()
                    .path("resolver")
                    .path(urnNbnValue)
                    .path("registrarScopeIdentifiers")
                    .path(identifier)
                    .request()
                        .delete(Response.class);
        } catch (WebApplicationException ex) {
            response = readResponseError(ex.getResponse(), ex);
        } catch (ResponseProcessingException ex) {
            response = readResponseError(ex.getResponse(), ex);
        }
        return response;
    }

    public Response updateCzidloRecord(String urnNbnValue, String identifier, String value) {
        if (urnNbnValue == null || urnNbnValue.isEmpty()) {
            throw new IllegalArgumentException("urnNbn");
        }

        Response response = null;
        try {
            response = resource()
                    .path("resolver")
                    .path(urnNbnValue)
                    .path("registrarScopeIdentifiers")
                    .path(identifier)
                    .request()
                        .put(Entity.entity(value, MediaType.APPLICATION_XML_TYPE), Response.class);
        } catch (WebApplicationException ex) {
            response = readResponseError(ex.getResponse(), ex);
        } catch (ResponseProcessingException ex) {
            response = readResponseError(ex.getResponse(), ex);
        }
        return response;
    }

    public Response deactivateUrnNbnValue(String urnNbnValue) {
        if (urnNbnValue == null || urnNbnValue.isEmpty()) {
            throw new IllegalArgumentException("urnNbn");
        }
        Response response = null;
        try {
            response = resource().path("urnnbn").path(urnNbnValue).request().delete(Response.class);
        } catch (WebApplicationException ex) {
            response = readResponseError(ex.getResponse(), ex);
        } catch (ResponseProcessingException ex) {
            response = readResponseError(ex.getResponse(), ex);
        }
        return response;
    }

    private Response readResponseError(javax.ws.rs.core.Response errResponse, RuntimeException ex) {
        try {
            Response response = null;
            errResponse.bufferEntity();
            MediaType errType = errResponse.getMediaType();
            if (errType != null && "xml".equalsIgnoreCase(errType.getSubtype())) {
                // try to map resolver warning to jaxb response
                try {
                    response = errResponse.readEntity(Response.class);
                } catch (Exception exception) {
                    String msg = errResponse.readEntity(String.class);
                    throw new IllegalStateException(msg, ex);
                }
            }
            if (response == null) {
                throw ex;
            }
            return response;
        } finally {
            errResponse.close();
        }
    }

    private WebTarget resource() {
        // https://resolver.nkp.cz/v4
        WebTarget target = getHttpClient().target(serviceUrl);
        if (LOG.isLoggable(Level.FINEST)) {
            target.register(new LoggingFeature(LOG));
        }
        return target;
    }

    Client getHttpClient() {
        if (httpClient == null) {
            try {
                SSLContext sslCtx = SSLContext.getInstance("SSL");
                TrustManager tm = new TrustThemAll();
                sslCtx.init(null, new TrustManager[] {tm}, null);

                httpClient = ClientBuilder.newBuilder()
                        .sslContext(sslCtx)
                        .register(HttpAuthenticationFeature.basic(user, passwd))
                        .build();
                httpClient.property(ClientProperties.FOLLOW_REDIRECTS, true);
                httpClient.property(ClientProperties.CONNECT_TIMEOUT, 2 * 60 * 1000); // 2 min
            } catch (NoSuchAlgorithmException | KeyManagementException ex) {
                throw new IllegalStateException(ex);
            }
        }
        return httpClient;
    }

    public boolean isDebug() {
        return LOG.isLoggable(Level.FINEST);
    }

    public void setDebug(boolean debug) {
        LOG.setLevel(debug ? Level.FINEST : null);
    }

    private static class TrustThemAll implements X509TrustManager {

        public TrustThemAll() {
        }

        @Override
        public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        }

        @Override
        public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        }

        @Override
        public X509Certificate[] getAcceptedIssuers() {
            return null;
        }
    }

}
