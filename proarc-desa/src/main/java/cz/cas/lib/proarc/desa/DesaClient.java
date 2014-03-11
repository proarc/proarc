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
package cz.cas.lib.proarc.desa;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.filter.HTTPBasicAuthFilter;
import com.sun.jersey.api.client.filter.LoggingFilter;
import com.sun.xml.ws.client.ClientTransportException;
import cz.cas.lib.proarc.desa.nomenclature.Nomenclatures;
import cz.cas.lib.proarc.desa.soap.AuthenticateUserFault;
import cz.cas.lib.proarc.desa.soap.AuthenticateUserRequest;
import cz.cas.lib.proarc.desa.soap.AuthenticateUserResponse;
import cz.cas.lib.proarc.desa.soap.FileHashAlg;
import cz.cas.lib.proarc.desa.soap.NomenclatureListType;
import cz.cas.lib.proarc.desa.soap.SIPSubmission;
import cz.cas.lib.proarc.desa.soap.SIPSubmissionFault;
import cz.cas.lib.proarc.desa.soap.SIPSubmissionService;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.ws.BindingProvider;

/**
 * The client for DESA SOAP and REST WS interface.
 *
 * @author Jan Pokorsky
 */
public final class DesaClient {

    private static final Logger LOG = Logger.getLogger(DesaClient.class.getName());
    private static JAXBContext NOMEN_JAXB;
    private final String soapUrl;
    private final String restUrl;
    private final String user;
    private final String passwd;
    private SIPSubmission sipSubmission;
    private Client httpClient;
    private DatatypeFactory xmlTypeFactory;
    private boolean debug;

    public DesaClient(String soapUrl, String restUrl, String user, String passwd) {
        this.soapUrl = soapUrl;
        this.restUrl = restUrl;
        this.user = user;
        this.passwd = passwd;
    }

    /**
     * Gets SIP transporter.
     *
     * @param operator operator user name
     * @param producerCode producer code
     * @return the transporter
     */
    public SIP2DESATransporter getSipTransporter(String operator, String producerCode) {
        return new SIP2DESATransporter(this, operator, producerCode);
    }

    /**
     * Gets nomenclatures from the remote registry.
     * @param operator operator login name
     * @param nomenclatureAcronyms acronyms to query
     * @return nomenclatures
     */
    public Nomenclatures getNomenclatures(String operator, String producerCode, List<String> nomenclatureAcronyms) {
        try {
            Source src = getNomenclaturesSource(operator, producerCode, nomenclatureAcronyms);
            return getNomenUnmarshaller().unmarshal(src, Nomenclatures.class).getValue();
        } catch (JAXBException ex) {
            String msg = String.format("producer: %s, acronyms: %s", producerCode, nomenclatureAcronyms);
            throw new IllegalStateException(msg, ex);
        }
    }

    /**
     * Gets nomenclatures.
     *
     * @param operator operator login name
     * @param nomenclatureAcronyms acronyms to query
     * @return nomenclature list as XML
     */
    public Source getNomenclaturesSource(String operator, String producerCode, List<String> nomenclatureAcronyms) {
        XMLGregorianCalendar currentDate = getXmlTypes().newXMLGregorianCalendar(new GregorianCalendar());
        NomenclatureListType nsType = new NomenclatureListType();
        nsType.getNomenclatureAcronyme().addAll(nomenclatureAcronyms);
        try {
            byte[] result = getSoapClient().getNomenclatures(null, producerCode, operator, nsType, currentDate);
            ByteArrayInputStream bis = new ByteArrayInputStream(result);
            return new StreamSource(bis);
        } catch (SIPSubmissionFault e) {
            String msg = String.format("producer: %s, acronyms: %s", producerCode, nomenclatureAcronyms);
            throw new IllegalStateException(msg, e);
        }
    }

    /**
     * Submits SIP via HTTP API.
     * 
     * @param file package contents
     * @param operator user name who sends the package
     * @param producerCode producer code (not ID)
     * @param producerSipId package ID
     * @param fileHashAlg optional hash algorithm code
     * @param fileHash optional package hash
     * @param lang optional language of error messages
     * @return AIP Version ID assigned by DESA
     */
    public String submitPackage(File file, String operator, String producerCode,
            String producerSipId, FileHashAlg fileHashAlg, String fileHash, String lang) {

        WebResource resource = resource();
        addQueryParam(resource, "fileHashAlg", fileHashAlg == null ? null : fileHashAlg.value());
        addQueryParam(resource, "fileHash", fileHash);
        ClientResponse response = resource()
                .path("submitpackage")
                .queryParam("userName", operator)
                .queryParam("producerCode", producerCode)
                .queryParam("producerSipId", producerSipId)
                .acceptLanguage(lang)
                .type(MediaType.APPLICATION_OCTET_STREAM_TYPE)
                .post(ClientResponse.class, file);
        if (response.getStatus() > 400) {
            String error = response.getEntity(String.class);
            throw new IllegalStateException(String.format("HTTP %s, Error: %s", response.getStatus(), error));
        }
        String aipVersionId = response.getHeaders().getFirst("X-DEA-AipVersionId");
        return aipVersionId;
    }

    /**
     * Authenticates an operator.
     *
     * @param operator operator user name
     * @param passwd operator password
     * @param producerCode producer code (not ID)
     * @return the authenticated operator
     * @throws AuthenticateUserFault authentication failure
     * @throws ClientTransportException soap failure
     */
    public AuthenticateUserResponse authenticateUser(
            String operator, String passwd, String producerCode)
            throws AuthenticateUserFault, ClientTransportException {

        AuthenticateUserRequest request = new AuthenticateUserRequest();
        request.setLogin(operator);
        request.setPassword(passwd);
        request.setProducerCode(producerCode);
        AuthenticateUserResponse response = getSoapClient().authenticateUser(request);
        if ("OK".equals(response.getStatus())) {
            return response;
        } else {
            String msg = String.format("operator: %s, producer: %s, status: %s",
                    operator, producerCode, response.getStatus());
            throw new AuthenticateUserFault(msg, null);
        }
    }

    public boolean isDebug() {
        return debug;
    }

    public void setDebug(boolean debug) {
        this.debug = debug;
    }

    DatatypeFactory getXmlTypes() {
        if (xmlTypeFactory == null) {
            try {
                xmlTypeFactory = DatatypeFactory.newInstance();
            } catch (DatatypeConfigurationException ex) {
                throw new IllegalStateException(ex);
            }
        }
        return xmlTypeFactory;
    }

    SIPSubmission getSoapClient() {
        if (sipSubmission == null) {
            SIPSubmissionService service = new SIPSubmissionService();
            sipSubmission = service.getSIPSubmissionSOAP();
            Map<String, Object> context = ((BindingProvider) sipSubmission).getRequestContext();
            context.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, soapUrl);
            context.put(BindingProvider.USERNAME_PROPERTY, user);
            context.put(BindingProvider.PASSWORD_PROPERTY, passwd);
        }
        return sipSubmission;
    }

    Client getHttpClient() {
        if (httpClient == null) {
            httpClient = Client.create();
            httpClient.addFilter(new HTTPBasicAuthFilter(user, passwd));
            httpClient.setFollowRedirects(true);
            httpClient.setConnectTimeout(2 * 60 * 1000); // 2 min
        }
        return httpClient;
    }

    private WebResource resource() {
        WebResource resource = getHttpClient().resource(restUrl);
        if (debug) {
            resource.addFilter(new LoggingFilter(System.out));
        }
        return resource;
    }

    private static void addQueryParam(WebResource wr, String name, String value) {
        if (value != null) {
            wr.queryParam(name, value);
        }
    }

    /**
     * Initialize JAXB transformers for Nomenclatures
     */
    private Unmarshaller getNomenUnmarshaller() throws JAXBException {
        return getNomenJaxbContext().createUnmarshaller();
    }

    /**
     * Gets the cached thread safe JAXB context.
     */
    private static JAXBContext getNomenJaxbContext() throws JAXBException {
        if (NOMEN_JAXB == null) {
            NOMEN_JAXB = JAXBContext.newInstance(Nomenclatures.class);
        }
        return NOMEN_JAXB;
    }

}
