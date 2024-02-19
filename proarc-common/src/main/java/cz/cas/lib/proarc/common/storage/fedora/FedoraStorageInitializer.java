/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.storage.fedora;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Prepares meta model in Fedora Commons Repository.
 *
 * @author Jan Pokorsky
 */
public final class FedoraStorageInitializer {

    private final FedoraStorage storage;

    public FedoraStorageInitializer(FedoraStorage storage) {
        this.storage = storage;
    }

    public void init() {
        try {
            String serverVersion = storage.getClient().getServerVersion();
            if (!serverVersion.startsWith("3.8")) {
                throw new IllegalStateException(String.format(
                        "The connected Fedora version is %s. ProArc requires version 3.8.*!", serverVersion));
            }
            checkMetaModels();
        } catch (Exception ex) {
            throw new IllegalStateException("Cannot initialize Fedora Repository!", ex);
        }
    }

    private void checkMetaModels() {
        MetaModelRepository modelRepo = MetaModelRepository.getInstance();
        Collection<MetaModel> models = modelRepo.find();
        ArrayList<String> modelPids = new ArrayList<String>();
        modelPids.add("model:proarcobject");
        modelPids.add("proarc:device");
        modelPids.add("proarc:audiodevice");
        modelPids.add("proarc:group");
        modelPids.add("proarc:user");
        for (MetaModel model : models) {
            modelPids.add(model.getPid());
        }
        for (String modelPid : modelPids) {
            checkMetaModel(modelPid);
        }
    }

    private void checkMetaModel(String modelPid) {
        try {
            if (!storage.exist(modelPid)) {
                ingestModel(modelPid);
            }
        } catch (DigitalObjectException ex) {
            throw new IllegalStateException(modelPid, ex);
        }
    }

    private LocalObject getModelObjectResource(String modelPid) {
        String modelFilename = modelPid.replace(':', '_');
        String resourcePath = String.format("/metamodel/%s.xml", modelFilename);
        URL resource = FedoraStorageInitializer.class.getResource(resourcePath);
        if (resource == null) {
            throw new IllegalStateException(resourcePath);
        }
        DigitalObject modelObj = FoxmlUtils.unmarshal(resource, DigitalObject.class);
        LocalObject lobject = new LocalStorage().create(modelObj);
        return lobject;
    }

    private void ingestModel(String modelPid) throws DigitalObjectException {
        LocalObject lobject = getModelObjectResource(modelPid);
        storage.ingest(lobject, "proarc", "Install " + modelPid);
    }

}
