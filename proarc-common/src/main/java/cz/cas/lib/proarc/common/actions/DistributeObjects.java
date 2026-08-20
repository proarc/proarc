/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.actions;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.process.BatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.process.internal.ValidationProcess;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/** Distributes members of one source object among multiple destination objects. */
public class DistributeObjects {

    private final AppConfiguration appConfig;
    private final AkubraConfiguration akubraConfiguration;
    private final UserProfile user;
    private final Locale locale;
    private final String fedoraLog;
    private final DigitalObjectHandler sourceHandler;
    private final Map<String, SearchViewItem> searchItems;
    private final List<Target> targets = new ArrayList<Target>();

    public DistributeObjects(
            AppConfiguration appConfig,
            AkubraConfiguration akubraConfiguration,
            UserProfile user,
            Locale locale,
            String fedoraLog,
            DigitalObjectHandler sourceHandler,
            Map<String, SearchViewItem> searchItems
    ) {
        this.appConfig = appConfig;
        this.akubraConfiguration = akubraConfiguration;
        this.user = user;
        this.locale = locale;
        this.fedoraLog = fedoraLog;
        this.sourceHandler = sourceHandler;
        this.searchItems = searchItems;
    }

    public void addTarget(
            DigitalObjectHandler destinationHandler,
            List<String> pids,
            List<DigitalObjectHandler> childHandlers,
            List<BatchItemObject> batchObjects
    ) {
        targets.add(new Target(destinationHandler, pids, childHandlers, batchObjects));
    }

    public List<SearchViewItem> execute(boolean runReindex) throws DigitalObjectException, IOException {
        validate();

        Set<String> movePids = new HashSet<String>();
        for (Target target : targets) {
            movePids.addAll(target.pids);
        }
        deleteMembers(movePids);

        List<SearchViewItem> added = new ArrayList<SearchViewItem>(movePids.size());
        for (Target target : targets) {
            added.addAll(addMembers(target));
        }

        sourceHandler.commit();
        for (Target target : targets) {
            target.destinationHandler.commit();
        }

        if (runReindex) {
            reindex();
        }
        return added;
    }

    private void validate() throws DigitalObjectException {
        Set<String> sourceMembers = new HashSet<String>(sourceHandler.relations().getMembers());
        for (Target target : targets) {
            String destinationPid = target.getDestinationPid();
            String destinationModel = target.destinationHandler.getModel().getPid();
            if (!ValidationProcess.canContainPage(destinationModel)) {
                throw new DigitalObjectException(destinationPid,
                        "Target model does not allow pages: " + destinationModel);
            }

            Set<String> existingPids = new HashSet<String>(target.destinationHandler.relations().getMembers());
            for (int i = 0; i < target.pids.size(); i++) {
                String pid = target.pids.get(i);
                if (!sourceMembers.contains(pid)) {
                    throw new DigitalObjectException(pid, "Source parent does not contain member: " + pid);
                }
                if (existingPids.contains(pid)) {
                    throw new DigitalObjectException(pid, destinationPid + " already contains: " + pid);
                }
                if (!searchItems.containsKey(pid)) {
                    throw new DigitalObjectException(pid, "Unknown pid: " + pid);
                }

                DigitalObjectHandler childHandler = target.childHandlers.get(i);
                MetaModel childModel = childHandler.getModel();
                StringBuilder reason = new StringBuilder();
                if (!childModel.isAllowedRelation(childHandler,
                        target.destinationHandler.getModel().getPid(), reason)) {
                    throw new DigitalObjectException(pid,
                            "NDK restrictions do not allow: " + childModel.getPid()
                                    + " to be in direct relation with: " + destinationModel
                                    + ". " + reason);
                }
            }
        }
    }

    private void deleteMembers(Set<String> movePids) throws DigitalObjectException, IOException {
        RelationEditor editor = sourceHandler.relations();
        List<String> members = editor.getMembers();
        if (members.removeAll(movePids)) {
            editor.setMembers(members);
            editor.write(editor.getLastModified(), fedoraLog);
        }

        AkubraStorage storage = getAkubraStorage();
        if (storage != null) {
            String sourcePid = sourceHandler.getFedoraObject().getPid();
            for (String pid : movePids) {
                storage.indexParentPid(pid, sourcePid);
            }
        }
    }

    private List<SearchViewItem> addMembers(Target target) throws DigitalObjectException, IOException {
        String destinationPid = target.getDestinationPid();
        RelationEditor editor = target.destinationHandler.relations();
        List<String> members = editor.getMembers();
        List<SearchViewItem> added = new ArrayList<SearchViewItem>(target.pids.size());
        AkubraStorage storage = getAkubraStorage();

        for (String pid : target.pids) {
            members.add(pid);
            SearchViewItem item = searchItems.get(pid);
            item.setParentPid(destinationPid);
            added.add(item);
            if (storage != null) {
                storage.indexParentPid(pid, destinationPid);
            }
        }

        editor.setMembers(members);
        editor.write(editor.getLastModified(), fedoraLog);
        return added;
    }

    private void reindex() throws DigitalObjectException, IOException {
        for (Target target : targets) {
            if (target.batchObjects == null) {
                ReindexDigitalObjects reindex = new ReindexDigitalObjects(
                        appConfig, akubraConfiguration, user, target.getDestinationPid(), null);
                reindex.reindex(target.getDestinationPid(), locale);
            } else {
                String modelId = target.childHandlers.get(0).getModel().getPid();
                ReindexDigitalObjects reindex = new ReindexDigitalObjects(
                        appConfig, akubraConfiguration, user, target.pids.get(0), modelId);
                reindex.reindexLocal(target.batchObjects);
            }
        }
    }

    private AkubraStorage getAkubraStorage() throws IOException {
        if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            return AkubraStorage.getInstance(akubraConfiguration);
        }
        return null;
    }

    private static final class Target {

        private final DigitalObjectHandler destinationHandler;
        private final List<String> pids;
        private final List<DigitalObjectHandler> childHandlers;
        private final List<BatchItemObject> batchObjects;

        private Target(
                DigitalObjectHandler destinationHandler,
                List<String> pids,
                List<DigitalObjectHandler> childHandlers,
                List<BatchItemObject> batchObjects
        ) {
            this.destinationHandler = destinationHandler;
            this.pids = pids;
            this.childHandlers = childHandlers;
            this.batchObjects = batchObjects;
        }

        private String getDestinationPid() {
            return destinationHandler.getFedoraObject().getPid();
        }
    }
}
