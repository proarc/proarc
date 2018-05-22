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

import java.io.File;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.export.ExportException;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.NdkExport;
import cz.cas.lib.proarc.common.export.mets.NdkExportOptions;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;

public class NdkSipExport extends NdkExport {
    private static final Logger LOG = Logger.getLogger(NdkSipExport.class.getName());

    private final RemoteStorage rstorage;
    private final MetsUtils metsUtils;

    public NdkSipExport(RemoteStorage rstorage, NdkExportOptions options) {
        super(rstorage, options);
        this.rstorage = rstorage;
        metsUtils = new MetsUtils();
    }

    public NdkSipExport(RemoteStorage rstorage, NdkExportOptions options, MetsUtils metsUtils) {
        super(rstorage, options);
        this.rstorage = rstorage;
        this.metsUtils = metsUtils;
    }


    @Override
    protected Result export(File target, String pid, String packageId, boolean hierarchy, boolean keepResult, String log) throws ExportException {
        Result result = new Result();
        if (keepResult) {
            result.setTargetFolder(target);
        }
        RemoteStorage.RemoteObject fo = rstorage.find(pid);
        MetsContext mc = buildContext(fo, packageId, target);
        try {
            List<String> packageIdentifiers = metsUtils.findPSPPIDs(fo.getPid(), mc, hierarchy);
            DigitalObject dobj = metsUtils.readFoXML(pid, fo.getClient());
            SipTree tree = SipTree.getTree(dobj, rstorage);
            List<SipTree> sipTree = tree.flattened().collect(Collectors.toList());
            List<SipTree> path = tree.getPathToRoot();
        } catch (MetsExportException e) {
            e.printStackTrace();
        }
        return null;
    }

}
