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

import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.NdkExport;
import cz.cas.lib.proarc.common.export.mets.NdkExportOptions;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElementVisitor;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import java.io.File;

/**
 * @see <a href="http://www.ndk.cz/standardy-digitalizace/E_born_MONO_NDK_22.pdf">Specification of emonograph</a>
 * @see <a href="https://www.ndk.cz/standardy-digitalizace/dmf_eborn_perio">Specification of eperiodical</a>
 */
public class NdkSipExport extends NdkExport {
    static final float PACKAGE_VERSION = 2.2f;

    public NdkSipExport(RemoteStorage rstorage, NdkExportOptions options) {
        super(rstorage, options);
    }

    @Override
    protected IMetsElementVisitor createMetsVisitor() {
        return new SipElementVisitor();
    }

    @Override
    protected MetsContext buildContext(RemoteStorage.RemoteObject fo, String packageId, File targetFolder) {
        MetsContext context = super.buildContext(fo, packageId, targetFolder);
        context.setPackageVersion(PACKAGE_VERSION);
        return context;
    }

}
