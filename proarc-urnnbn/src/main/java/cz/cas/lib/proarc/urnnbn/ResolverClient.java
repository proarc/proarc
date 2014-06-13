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

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.UniformInterfaceException;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.api.client.filter.HTTPBasicAuthFilter;
import com.sun.jersey.api.client.filter.LoggingFilter;
import com.sun.jersey.client.urlconnection.HTTPSProperties;
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
import javax.ws.rs.core.MediaType;

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
    private boolean debug;

    public ResolverClient(String serviceUrl, String registrar, Long archiver,
            String user, String passwd) {

        this.serviceUrl = serviceUrl;
        this.registrar = registrar;
        this.archiver = archiver;
        this.user = user;
        this.passwd = passwd;
    }

    public String getDigitalInstances() {
        String response = resource().path("digitalInstances").get(String.class);
        return response;
    }

    /**
     * Registers an digital document to get URN:NBN.
     * <p>{@code POST http://resolver.nkp.cz/api/v3/registrars/boa001/digitalDocuments}
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
                    .entity(object, MediaType.APPLICATION_XML_TYPE)
                    .post(Response.class);
        } catch (UniformInterfaceException ex) {
            response = readResponseError(ex);
        }
        return response;
    }

    private Response readResponseError(UniformInterfaceException ex) {
        Response response = null;
        ClientResponse errResponse = ex.getResponse();
        errResponse.bufferEntity();
        MediaType errType = errResponse.getType();
        if (errType != null && "xml".equalsIgnoreCase(errType.getSubtype())) {
            // try to map resolver warning to jaxb response
            try {
                response = errResponse.getEntity(Response.class);
            } catch (Exception exception) {
                String msg = errResponse.getEntity(String.class);
                throw new IllegalStateException(msg, ex);
            }
        }
        if (response == null) {
            throw ex;
        }
        return response;
    }

    private WebResource resource() {
        // https://resolver.nkp.cz/api/v3
        WebResource resource = getHttpClient().resource(serviceUrl);
        if (debug || LOG.isLoggable(Level.FINEST)) {
            resource.addFilter(new LoggingFilter(System.out));
        }
        return resource;
    }

    Client getHttpClient() {
        if (httpClient == null) {
            try {
                SSLContext sslCtx = SSLContext.getInstance("SSL");
                TrustManager tm = new TrustThemAll();
                sslCtx.init(null, new TrustManager[] {tm}, null);
                DefaultClientConfig jerseyConfig = new DefaultClientConfig();
                jerseyConfig.getProperties().put(
                        HTTPSProperties.PROPERTY_HTTPS_PROPERTIES,
                        new HTTPSProperties(null, sslCtx));
                httpClient = Client.create(jerseyConfig);
                httpClient.addFilter(new HTTPBasicAuthFilter(user, passwd));
                httpClient.setFollowRedirects(true);
                httpClient.setConnectTimeout(2 * 60 * 1000); // 2 min
            } catch (NoSuchAlgorithmException ex) {
                throw new IllegalStateException(ex);
            } catch (KeyManagementException ex) {
                throw new IllegalStateException(ex);
            }
        }
        return httpClient;
    }

    public boolean isDebug() {
        return debug;
    }

    public void setDebug(boolean debug) {
        this.debug = debug;
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
