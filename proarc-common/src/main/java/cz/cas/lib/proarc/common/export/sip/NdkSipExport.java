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
import cz.cas.lib.proarc.common.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.NdkExport;
import cz.cas.lib.proarc.common.export.mets.NdkExportOptions;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import org.apache.commons.codec.digest.DigestUtils;

import static cz.cas.lib.proarc.common.export.mets.Const.CHAPTER;
import static cz.cas.lib.proarc.common.export.mets.Const.MONOGRAPH_MULTIPART;
import static cz.cas.lib.proarc.common.export.mets.Const.MONOGRAPH_UNIT;

/**
 * @see <a href="http://www.ndk.cz/standardy-digitalizace/E_born_MONO_NDK_22.pdf">Specification of emonograph</a>
 */
public class NdkSipExport extends NdkExport {
    private static final Logger LOG = Logger.getLogger(NdkSipExport.class.getName());
    public static final float PACKAGE_VERSION = 2.2f;

    public NdkSipExport(RemoteStorage rstorage, NdkExportOptions options) {
        super(rstorage, options);
    }

    @Override
    protected MetsElementVisitor createMetsVisitor() {
        return new SipElementVisitor();
    }


    protected MetsContext buildContext(RemoteStorage.RemoteObject fo, String packageId, File targetFolder) {
        MetsContext context = super.buildContext(fo, packageId, targetFolder);
        context.setPackageVersion(PACKAGE_VERSION);
        return context;
    }

    private static class SipElementVisitor extends MetsElementVisitor {
        @Override
        public void insertIntoMets(IMetsElement metsElement) throws MetsExportException {

            Collection<Path> packageFiles = new ArrayList<>();
            metsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(metsElement));
            IMetsElement rootElement = metsElement.getMetsContext().getRootElement();
            Path packageRoot = createPackageDir(rootElement);

            if (MONOGRAPH_UNIT.equalsIgnoreCase(rootElement.getElementType())) {
                //metsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(metsElement));
                packageFiles.addAll(saveStreams(metsElement, packageRoot));

                for (MetsElement childElement : metsElement.getChildren()) {
                    packageFiles.addAll(saveStreams(childElement, packageRoot));
                }
            }

            metsElement.getMetsContext().getFileList().addAll(
                    packageFiles.stream().map(filePath -> {
                        String md5 = null;
                        long size = -1;
                        try {
                            md5 = DigestUtils.md5Hex(Files.readAllBytes(filePath));
                            size = Files.size(filePath);
                        } catch (IOException e) {
                            LOG.warning(filePath + ": md5 or size is not calculated");
                        }
                        return new FileMD5Info(filePath.toString(), md5, size);
                    }).collect(Collectors.toList()));

            saveInfoFile(packageRoot, metsElement);

        }

        private void saveInfoFile(Path packageRoot, IMetsElement metsElement) throws MetsExportException {
            MetsUtils.saveInfoFile(packageRoot.getParent().toString(), metsElement.getMetsContext(), null, null, null);
        }

        /**
         * Scaffold empty SIP package
         *
         * @param metsElement element with specified package id
         * @return path of package
         * @throws MetsExportException translated from IOException
         */
        private Path createPackageDir(IMetsElement metsElement) throws MetsExportException {
            if (metsElement.getMetsContext().getPackageID() == null) {
                throw new MetsExportException(metsElement.getOriginalPid(), "Package ID is null", false, null);
            }
            try {
                Path path = Paths.get(metsElement.getMetsContext().getOutputPath()).resolve(metsElement.getMetsContext().getPackageID());
                Path packageDir = Files.createDirectories(path);
                Files.createDirectory(packageDir.resolve("original"));
                Files.createDirectory(packageDir.resolve("metadata"));

                return packageDir;
            } catch (IOException e) {
                MetsExportException ex = new MetsExportException(e.getMessage());
                ex.addException("can not create package", true, e);
                throw ex;
            }
        }

        private List<Path> saveStreams(IMetsElement metsElement, Path packageDir) throws
                MetsExportException {
            try {
                List<Path> packageFiles = new ArrayList<>();

                Optional<DatastreamType> rawDatastream = metsElement.getSourceObject().getDatastream().stream().filter(stream -> "RAW".equalsIgnoreCase(stream.getID())).findFirst();
                if (rawDatastream.isPresent()) {
                    GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), "RAW");
                    InputStream dsStream = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                    Path originalPathDoc = packageDir.resolve("original").resolve("oc_" + metsElement.getMetsContext().getPackageID() + ".pdf");
                    // check null
                    if (Files.copy(dsStream, originalPathDoc) == 0) {
                        throw new MetsExportException("empty RAW datastream " + metsElement.getOriginalPid());
                    }
                    packageFiles.add(originalPathDoc);
                }

                Optional<DatastreamType> modsDatastream = metsElement.getSourceObject().getDatastream().stream().filter(stream -> "BIBLIO_MODS".equalsIgnoreCase(stream.getID())).findFirst();
                if (modsDatastream.isPresent()) {
                    GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), "BIBLIO_MODS");
                    InputStream dsStream = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();

                    String modsName;
                    switch (MetsUtils.getElementType(metsElement.getModel())) {
                        case MONOGRAPH_UNIT:
                            modsName = "mods_volume.xml";
                            break;
                        case MONOGRAPH_MULTIPART:
                            modsName = "mods_title.xml";
                            break;
                        case CHAPTER:
                            modsName = "mods_chapter.xml";
                            break;
                        default:
                            throw new IllegalArgumentException("unknown model " + metsElement.getModel());
                    }
                    Path metadataPathDoc = packageDir.resolve("metadata").resolve(modsName);
                    Files.copy(dsStream, metadataPathDoc);
                    packageFiles.add(metadataPathDoc);
                }

                return Collections.unmodifiableList(packageFiles);
            } catch (FedoraClientException | IOException e) {
                MetsExportException ex = new MetsExportException(e.getMessage());
                ex.addException(e.getMessage(), true, e);
                throw ex;
            }
        }
    }
}
