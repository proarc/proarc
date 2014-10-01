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
package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.Group;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;

/**
 * The helper to access and manipulate digital objects.
 *
 * @author Jan Pokorsky
 */
public class DigitalObjectManager {

    private static DigitalObjectManager INSTANCE;
    private static final Logger LOG = Logger.getLogger(DigitalObjectManager.class.getName());

    public static DigitalObjectManager getDefault() {
        return INSTANCE;
    }

    public static void setDefault(DigitalObjectManager manager) {
        INSTANCE = manager;
    }

    private final AppConfiguration appConfig;
    private final ImportBatchManager ibm;
    private RemoteStorage remotes;
    private final MetaModelRepository models;
    private final UserManager userManager;

    public DigitalObjectManager(AppConfiguration appConfig, ImportBatchManager ibm,
            RemoteStorage remotes, MetaModelRepository models, UserManager userManager) {

        this.appConfig = appConfig;
        this.ibm = ibm;
        this.remotes = remotes;
        this.models = models;
        this.userManager = userManager;
    }

    /**
     * Creates the handler to edit a digital object contents.
     * @param fo digital object
     * @return the handler
     */
    public DigitalObjectHandler createHandler(FedoraObject fo) {
        return new DigitalObjectHandler(fo, models);
    }

    public FedoraObject find(String pid, Integer batchId) throws DigitalObjectNotFoundException {
        Batch batch = null;
        if (batchId != null) {
            batch = ibm.get(batchId);
            if (batch == null) {
                throw new DigitalObjectNotFoundException(pid, batchId, null, null, null);
//                throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID, String.valueOf(batchId));
            }
        }
        return find2(pid, batch);
    }

    public FedoraObject find2(String pid, Batch batch) throws DigitalObjectNotFoundException {
        FedoraObject fobject;
        if (batch != null) {
            // XXX move to LocalObject.flush or stream.write
//            if (!readonly) {
//                ImportResource.checkBatchState(batch);
//            }
            if (pid == null || ImportBatchManager.ROOT_ITEM_PID.equals(pid)) {
                fobject = ibm.getRootObject(batch);
            } else {
                BatchItemObject item = ibm.findBatchObject(batch.getId(), pid);
                if (item == null) {
                    throw new DigitalObjectNotFoundException(pid, batch.getId(), null, null, null);
//                    throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
                }
                fobject = new LocalStorage().load(pid, item.getFile());
            }
        } else {
            if (pid == null) {
                throw new NullPointerException("pid");
            }
            fobject = getRemotes().find(pid);
        }
        return fobject;
    }

    // XXX replace with CreateObjectHandler (getLocal, ingest, setParent, )?
    // XXX current impl is unusable for the batch import
    public Item createDigitalObject(
            String modelId, String pid,
            String parentPid, UserProfile user, String xml, String message
            ) throws DigitalObjectException, DigitalObjectExistException {

        if (modelId == null) {
            throw new IllegalArgumentException("modelId");
        }
        if (pid != null) {
            boolean invalid = pid.length() < 5;
            try {
                if (!invalid) {
                    UUID uuid = UUID.fromString(FoxmlUtils.pidAsUuid(pid));
                    pid = FoxmlUtils.pidFromUuid(uuid.toString());
                }
            } catch (IllegalArgumentException e) {
                throw new IllegalArgumentException("pid", e);
            }
        }
        if (user == null) {
            throw new IllegalArgumentException("user");
        }
        DigitalObjectHandler parentHandler = null;
        if (parentPid != null) {
            FedoraObject parentObject = find(parentPid, null);
            parentHandler = DigitalObjectManager.getDefault().createHandler(parentObject);
        }

        LocalObject localObject = new LocalStorage().create(pid);
        localObject.setOwner(user.getUserName());
        DigitalObjectHandler doHandler = DigitalObjectManager.getDefault().createHandler(localObject);
        doHandler.setParameterParent(parentHandler);
        doHandler.setParameterUser(user);

        RelationEditor relations = doHandler.relations();
        relations.setModel(modelId);
        Integer defaultGroupId = user.getDefaultGroup();
        if (defaultGroupId != null) {
            Group group = userManager.findGroup(defaultGroupId);
            String grpPid = group.getName();
            relations.setOwners(Collections.singletonList(grpPid));
        }
        relations.write(0, message);

        DescriptionMetadata<String> descMetadata = new DescriptionMetadata<String>();
        descMetadata.setData(xml);
        doHandler.metadata().setMetadataAsXml(descMetadata, message);

        if (parentHandler != null) {
            RelationEditor parentRelsExt = parentHandler.relations();
            List<String> members = parentRelsExt.getMembers();
            members.add(localObject.getPid());
            parentRelsExt.setMembers(members);
            parentRelsExt.write(parentRelsExt.getLastModified(), message);
        }
        doHandler.commit();

        getRemotes().ingest(localObject, user.getUserName(), message);
        if (parentHandler != null) {
            parentHandler.commit();
        }

        Item item = new Item(localObject.getPid());
        item.setLabel(localObject.getLabel());
        item.setModel(modelId);
        item.setOwner(localObject.getOwner());
        item.setParentPid(parentPid);
        return item;
    }

    private RemoteStorage getRemotes() {
        if (remotes == null) {
            try {
                remotes = RemoteStorage.getInstance(appConfig);
            } catch (IOException ex) {
                throw new IllegalStateException(ex);
            }
        }
        return remotes;
    }

}
