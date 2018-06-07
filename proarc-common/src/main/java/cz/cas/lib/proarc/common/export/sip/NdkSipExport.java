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

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.NdkExport;
import cz.cas.lib.proarc.common.export.mets.NdkExportOptions;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import org.apache.commons.codec.digest.DigestUtils;

/**
 * @see {<a href="http://www.ndk.cz/standardy-digitalizace/E_born_MONO_NDK_22.pdf">Specification of emonograph</a>}
 */
public class NdkSipExport extends NdkExport {
    private static final Logger LOG = Logger.getLogger(NdkSipExport.class.getName());

    public NdkSipExport(RemoteStorage rstorage, NdkExportOptions options) {
        super(rstorage, options);
    }

    @Override
    protected MetsElementVisitor createMetsVisitor() {
        return new MetsElementVisitor() {
            @Override
            public void insertIntoMets(IMetsElement metsElement) throws MetsExportException {
                IMetsElement rootElement = metsElement.getMetsContext().getRootElement();

                if (Const.MONOGRAPH_UNIT.equalsIgnoreCase(rootElement.getElementType())) {
                    metsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(metsElement));
                    Path packageRoot = createPackageDir(rootElement);
                    saveInfoFile(packageRoot, metsElement);
                }
            }

            private void saveInfoFile(Path packageRoot, IMetsElement metsElement) throws MetsExportException {
                MetsUtils.saveInfoFile(packageRoot.getParent().toString(), metsElement.getMetsContext(), null, null, null);
            }

            private Path createPackageDir(IMetsElement metsElement) throws MetsExportException {
                if (metsElement.getMetsContext().getPackageID() == null) {
                    throw new MetsExportException(metsElement.getOriginalPid(), "Package ID is null", false, null);
                }
                try {
                    List<Path> packageFiles = new ArrayList<>();
                    Path path = Paths.get(metsElement.getMetsContext().getOutputPath()).resolve(metsElement.getMetsContext().getPackageID());
                    Path packageDir = Files.createDirectories(path);
                    Path originalPath = Files.createDirectory(packageDir.resolve("original"));
                    Path metadataPath = Files.createDirectory(packageDir.resolve("metadata"));

                    Optional<DatastreamType> rawDatastream = metsElement.getSourceObject().getDatastream().stream().filter(stream -> "RAW".equalsIgnoreCase(stream.getID())).findFirst();
                    if (rawDatastream.isPresent()) {
                        GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), "RAW");
                        InputStream dsStream = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                        Path originalPathDoc = originalPath.resolve("oc_" + metsElement.getMetsContext().getPackageID() + ".pdf");
                        Files.copy(dsStream, originalPathDoc);
                        packageFiles.add(originalPathDoc);
                    }

                    Optional<DatastreamType> modsDatastream = metsElement.getSourceObject().getDatastream().stream().filter(stream -> "BIBLIO_MODS".equalsIgnoreCase(stream.getID())).findFirst();
                    if (modsDatastream.isPresent()) {
                        GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), "BIBLIO_MODS");
                        InputStream dsStream = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                        Path metadataPathDoc = metadataPath.resolve("mods_volume.xml");
                        Files.copy(dsStream, metadataPathDoc);
                        packageFiles.add(metadataPathDoc);
                    }

                    metsElement.getMetsContext().getFileList().addAll(
                            packageFiles.stream().map(filePath -> {
                                String md5 = null;
                                Long size = null;
                                try {
                                    md5 = DigestUtils.md5Hex(Files.readAllBytes(filePath));
                                    size = Files.size(filePath);
                                } catch (IOException e) {
                                    LOG.warning(filePath + ": md5 is not calculated");
                                }
                                return new FileMD5Info(filePath.toString(), md5, size);
                            }).collect(Collectors.toList()));


                    return packageDir;
                } catch (FedoraClientException | IOException e) {
                    throw new MetsExportException(e.getMessage());
                }
            }
        };
    }


    protected MetsContext buildContext(RemoteStorage.RemoteObject fo, String packageId, File targetFolder) {
        MetsContext context = super.buildContext(fo, packageId, targetFolder);
        context.setPackageVersion(2.2f);
        return context;
    }
}
