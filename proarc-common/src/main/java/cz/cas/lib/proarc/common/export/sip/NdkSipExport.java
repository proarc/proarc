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

package cz.cas.lib.proarc.common.export.sip;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Optional;
import java.util.logging.Logger;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.NdkExport;
import cz.cas.lib.proarc.common.export.mets.NdkExportOptions;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.mets.info.Info;

public class NdkSipExport extends NdkExport {
    private static final Logger LOG = Logger.getLogger(NdkSipExport.class.getName());

    private final RemoteStorage rstorage;

    public NdkSipExport(RemoteStorage rstorage, NdkExportOptions options) {
        super(rstorage, options);
        this.rstorage = rstorage;
    }


    @Override
    /**
     * @see http://www.ndk.cz/standardy-digitalizace/E_born_MONO_NDK_22.pdf
     */
    protected MetsElementVisitor createMetsVisitor() {
        return new MetsElementVisitor() {
            @Override
            public void insertIntoMets(IMetsElement metsElement) throws MetsExportException {
                IMetsElement rootElement = metsElement.getMetsContext().getRootElement();

                if (Const.MONOGRAPH_UNIT.equalsIgnoreCase(rootElement.getElementType())) {
                    metsElement.getMetsContext().setPackageID(getPackageID(metsElement));
                    Path packageRoot = createPackageDir(rootElement);
                    saveInfoFile(packageRoot, metsElement);
                }
            }

            private String getPackageID(IMetsElement metsElement) {
                return "test"; //TODO-MR
            }

            private void  saveInfoFile(Path packageRoot, IMetsElement metsElement) {
                try {
                    Info info = new Info();
                    GregorianCalendar c = new GregorianCalendar();
                    c.setTime(new Date());
                    XMLGregorianCalendar date = DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
                    info.setCreated(date);
                    info.setSize(50);
                    info.setPackageid("");
                    info.setMainmets("");
                    info.setValidation(new Info.Validation());

                    Info.Itemlist itemlist = new Info.Itemlist();
                    itemlist.setItemtotal(BigInteger.ONE);
                    itemlist.getItem().add("bl");
                    info.setItemlist(itemlist);
                    Info.Titleid titleid = new Info.Titleid();
                    titleid.setType("urnnbn");
                    info.getTitleid().add(titleid);

                    Info.Checksum checksum = new Info.Checksum();
                    checksum.setChecksum("blabla");
                    checksum.setType("md5");

                    info.setChecksum(checksum);

                    JAXBContext jaxbContext = JAXBContext.newInstance(Info.class);
                    Marshaller marshaller = jaxbContext.createMarshaller();
                    marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
                    marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
                    marshaller.marshal(info, packageRoot.resolve("info_" + getPackageID(metsElement) + ".xml").toFile());
                } catch (JAXBException | DatatypeConfigurationException e) {
                    e.printStackTrace();
                }
            }
        };
    }

    private Path createPackageDir(IMetsElement metsElement) throws MetsExportException {
        if (metsElement.getMetsContext().getPackageID() == null) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Package ID is null", false, null);
        }
        try {
            Path path = Paths.get(metsElement.getMetsContext().getOutputPath()).resolve(metsElement.getMetsContext().getPackageID());
            Path packageDir = Files.createDirectories(path);
            Path originalPath = Files.createDirectory(packageDir.resolve("original"));
            Path metadataPath = Files.createDirectory(packageDir.resolve("metadata"));

            Optional<DatastreamType> rawDatastream = metsElement.getSourceObject().getDatastream().stream().filter(stream -> "RAW".equalsIgnoreCase(stream.getID())).findFirst();
            rawDatastream.ifPresent(type -> {
                        GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), "RAW");
                        try {
                            InputStream dsStream = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                            Files.copy(dsStream, originalPath.resolve("oc_" + metsElement.getMetsContext().getPackageID() + ".pdf"));
                        } catch (FedoraClientException e) {
                            e.printStackTrace();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
            );

            Optional<DatastreamType> modsDatastream = metsElement.getSourceObject().getDatastream().stream().filter(stream -> "BIBLIO_MODS".equalsIgnoreCase(stream.getID())).findFirst();
            modsDatastream.ifPresent(type -> {
                        GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), "BIBLIO_MODS");
                        try {
                            InputStream dsStream = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                            Files.copy(dsStream, metadataPath.resolve("test.xml"));
                        } catch (FedoraClientException e) {
                            e.printStackTrace();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
            );

            return packageDir;
        } catch (IOException e) {
            throw new MetsExportException(e.getMessage());
        }
    }
}
