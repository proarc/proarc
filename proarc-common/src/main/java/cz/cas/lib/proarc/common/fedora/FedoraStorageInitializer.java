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
package cz.cas.lib.proarc.common.fedora;

import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import javax.ws.rs.core.Response.Status;

/**
 * Prepares meta model in Fedora Commons Repository.
 *
 * @author Jan Pokorsky
 */
public final class FedoraStorageInitializer {

    private final RemoteStorage storage;

    public FedoraStorageInitializer(RemoteStorage storage) {
        this.storage = storage;
    }

    public void init() {
        try {
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
        for (MetaModel model : models) {
            modelPids.add(model.getPid());
        }
        for (String modelPid : modelPids) {
            checkMetaModel(modelPid);
        }
    }

    private void checkMetaModel(String modelPid) {
        try {
            storage.getClient().getLastModifiedDate(modelPid);
        } catch (FedoraClientException ex) {
            if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                try {
                    ingestModel(modelPid);
                } catch (FedoraClientException fex) {
                    throw new IllegalStateException(modelPid, fex);
                }
            } else {
                throw new IllegalStateException(modelPid, ex);
            }
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

    private void ingestModel(String modelPid) throws FedoraClientException {
        LocalObject lobject = getModelObjectResource(modelPid);
        storage.ingest(lobject, "proarc", "Install " + modelPid);
    }

}
