/*
 * Copyright (C) 2018 Martin Rumanek
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

package cz.cas.lib.proarc.common.export.mockrepository;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.client.request.GetObjectXML;
import com.yourmediashelf.fedora.client.response.FedoraResponse;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Optional;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import mockit.Mock;
import mockit.MockUp;
import org.apache.commons.lang.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;


/**
 * Mock of Fedora Object repository. If you add some foxml object to resources, you should manually add relation to {@link MockSearchView}.
 */
public class MockFedoraClient extends MockUp<FedoraClient> {
    @Mock
    GetObjectXML getObjectXML(String pid) {
        return new GetObjectXML(pid) {
            @Override
            public FedoraResponse execute(FedoraClient fedora) {
                return new FedoraResponse() {
                    @Override
                    public int getStatus() {
                        return HttpURLConnection.HTTP_OK;
                    }

                    @Override
                    public InputStream getEntityInputStream() {
                        try {
                            return getClass().getResource(StringUtils.remove(pid, "uuid:") + ".xml").openStream();
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    }

                    @Override
                    public <T> T getEntity(Class<T> c) {
                        return null;
                    }

                    @Override
                    public String getType() {
                        return null;
                    }

                    @Override
                    public void close() {
                    }
                };
            }
        };
    }

    @Mock
    GetDatastreamDissemination getDatastreamDissemination(
            String pid, String dsId) {
        return new GetDatastreamDissemination(pid, dsId) {
            @Override
            public FedoraResponse execute(FedoraClient fedora) {
                return new FedoraResponse() {
                    @Override
                    public int getStatus() {
                        return HttpURLConnection.HTTP_OK;
                    }

                    @Override
                    public InputStream getEntityInputStream() {
                        try {
                            Optional<URL> stream = Optional.ofNullable(getClass().getResource(dsId + "/" + StringUtils.remove(pid, "uuid:")));
                            if (stream.isPresent()) {
                                return stream.get().openStream();
                            } else {
                                URL objectUrl = getClass().getResource(StringUtils.remove(pid, "uuid:") + ".xml");
                                DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
                                dbf.setNamespaceAware(true);
                                DocumentBuilder db = dbf.newDocumentBuilder();
                                Document objectXML = db.parse(objectUrl.openStream());
                                XPath xPath = XPathFactory.newInstance().newXPath();
                                SimpleNamespaceContext namespaces = new SimpleNamespaceContext().add("foxml", "info:fedora/fedora-system:def/foxml#");
                                xPath.setNamespaceContext(namespaces);
                                Document datastreamXML = db.newDocument();
                                Node node = (Node) xPath.compile("//foxml:datastream[@ID='" + dsId + "']").evaluate(objectXML, XPathConstants.NODE);
                                Node importedNode = datastreamXML.importNode(node, true);
                                datastreamXML.appendChild(importedNode);

                                ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                                Source xmlSource = new DOMSource(datastreamXML);
                                Result outputTarget = new StreamResult(outputStream);
                                TransformerFactory.newInstance().newTransformer().transform(xmlSource, outputTarget);
                                return new ByteArrayInputStream(outputStream.toByteArray());
                            }
                        } catch (Exception e) {
                            throw new RuntimeException(e);
                        }
                    }

                    @Override
                    public <T> T getEntity(Class<T> c) {
                        return null;
                    }

                    @Override
                    public String getType() {
                        return null;
                    }

                    @Override
                    public void close() {
                    }
                };
            }
        };
    }

}
