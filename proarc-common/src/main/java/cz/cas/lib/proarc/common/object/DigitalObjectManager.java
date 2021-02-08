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
import cz.cas.lib.proarc.common.fedora.FedoraDao;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.WorkflowStorage;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.Group;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.workflow.WorkflowException;
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_NEW;

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
     * @param fobject digital object
     * @return the handler
     */
    public DigitalObjectHandler createHandler(FedoraObject fobject) {
        if (fobject == null) {
            throw new NullPointerException("fobject");
        }
        return new DigitalObjectHandler(fobject, models);
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

    /**
     * Object in workflow doesn't have a PID, so they are identified by workflow job id
     *
     * @return WorkflowObject with BIBLIO MODS
     * @throws DigitalObjectNotFoundException
     */
    public FedoraObject find(BigDecimal workflowJobId, String modelId, Locale locale) throws DigitalObjectNotFoundException {
        return new WorkflowStorage().load(workflowJobId, modelId, locale);
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

    public Item createDigitalObject(
            String modelId, String pid,
            String parentPid, UserProfile user, String xml, String message
            ) throws DigitalObjectException, DigitalObjectExistException {
        return create(modelId, pid, parentPid, user, xml, message).createDigitalObject();
    }

    public CreateHandler create(
            String modelId, String pid,
            String parentPid, UserProfile user, String xml, String message
            ) throws DigitalObjectException, DigitalObjectExistException {
        return new CreateHandler(modelId, pid, parentPid, user, xml, message);
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

    static void checkPid(String pid) {
        boolean invalid = pid.length() < 5;
        try {
            if (!invalid) {
                UUID uuid = UUID.fromString(FoxmlUtils.pidAsUuid(pid));
                FoxmlUtils.pidFromUuid(uuid.toString());
            }
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("pid", e);
        }
    }

    public class CreateHandler extends FedoraDao  {

        private final String modelId;
        private String pid;
        private String parentPid;
        private DigitalObjectHandler parentHandler;
        private final UserProfile user;
        private Group group;
        private boolean initGroup = true;
        private String xml;
        private String message;
        private final Map<String, Object> params = new HashMap<>();

        private Boolean hasNext = true;

        private LocalDate seriesDateFrom;
        private LocalDate seriesDateTo;
        private Set<DayOfWeek> seriesDaysIncluded;

        private Integer seriesPartNumberFrom;

        public CreateHandler(String modelId, String pid, String parentPid, UserProfile user, String xml, String message) {
            Objects.requireNonNull(modelId, "modelId");
            Objects.requireNonNull(user, "user");
            this.modelId = modelId;
            this.user = user;
            if (pid != null) {
                checkPid(pid);
                this.pid = pid;
            }
            this.parentPid = parentPid;
            this.xml = xml;
            this.message = message;
        }

        public void setMetadataXml(String xml) {
            this.xml = xml;
        }

        public String getMetadataXml() {
            return xml;
        }

        /**
         * Prepares the handler to produce a series of NDK issues.
         * @param from a date to start the series
         * @param to an optional date to end the series, inclusive
         * @param dayIdxs an optional list of ISO day indicies to include in the series
         * @param partNumberFrom a partNumber to start the series
         * @return the handler
         */
        public CreateHandler issueSeries(LocalDate from, LocalDate to, List<Integer> dayIdxs, Integer partNumberFrom) {
            this.seriesDateFrom = Objects.requireNonNull(from, "from");
            if (to != null) {
                if (to.isBefore(from)) {
                    throw new IllegalArgumentException(String.format("Invalid date interval. from > to: %s > %s", from, to));
                }
                if (from.getYear() != to.getYear()) {
                    throw new IllegalArgumentException(String.format("Not the same year: %s, %s", from, to));
                }
            } else {
                to = from.with(TemporalAdjusters.lastDayOfYear());
            }
            this.seriesDateTo = to;
            this.seriesDaysIncluded = dayIdxs == null || dayIdxs.isEmpty()
                    ? EnumSet.allOf(DayOfWeek.class)
                    : dayIdxs.stream()
                        .filter(day -> day != null && day >= 1 && day <= 7)
                        .map(i -> DayOfWeek.of(i))
                        .collect(Collectors.toSet());
            if (seriesDaysIncluded.contains(seriesDateFrom.getDayOfWeek())) {
                params.put(DigitalObjectHandler.PARAM_ISSUE_DATE,
                        seriesDateFrom.format(DateTimeFormatter.ofPattern("dd.MM.yyyy")));
            } else {
                // move the start to the first acceptable day
                this.hasNext = nextDate();
            }
            this.seriesPartNumberFrom = partNumberFrom;
            if (partNumberFrom != null) {
                params.put(DigitalObjectHandler.PARAM_PART_NUMBER, seriesPartNumberFrom.toString());
            }
            return this;
        }

        public boolean isBatch() {
            return this.seriesDateFrom != null;
        }

        public boolean hasNext() {
            if (hasNext == null) {
                hasNext = nextDate() && nextPartNumber();
            }
            return hasNext;
        }

        private void next() {
            if (Boolean.TRUE.equals(hasNext)) {
                hasNext = null;
            }
        }

        private boolean nextPartNumber() {
            if (seriesPartNumberFrom != null) {
                ++seriesPartNumberFrom;
                params.put(DigitalObjectHandler.PARAM_PART_NUMBER, seriesPartNumberFrom.toString());
            }
            return true;
        }

        private boolean nextDate() {
            if (seriesDateFrom != null) {
                seriesDateFrom = seriesDateFrom.plusDays(1);
                while (!seriesDateFrom.isAfter(seriesDateTo)) {
                    if (seriesDaysIncluded.contains(seriesDateFrom.getDayOfWeek())) {
                        params.put(DigitalObjectHandler.PARAM_ISSUE_DATE,
                                seriesDateFrom.format(DateTimeFormatter.ofPattern("dd.MM.yyyy")));
                        return true;
                    }
                    seriesDateFrom = seriesDateFrom.plusDays(1);
                }
            }
            return false;
        }

        public UserProfile getUser() {
            return user;
        }

        public Group getUserGroup() {
            if (initGroup) {
                initGroup = false;
                Integer defaultGroupId = user.getDefaultGroup();
                if (defaultGroupId != null) {
                    group = userManager.findGroup(defaultGroupId);
                }
            }
            return group;
        }

        public DigitalObjectHandler getParentHandler() throws DigitalObjectNotFoundException {
            if (parentHandler == null && parentPid != null) {
                FedoraObject parentObject = find(parentPid, null);
                parentHandler = DigitalObjectManager.getDefault().createHandler(parentObject);
            }
            return parentHandler;
        }


        public List<Item> create() throws DigitalObjectException {
            boolean isBatch = isBatch();
            if (isBatch) {
                return createBatch();
            } else {
                return Collections.singletonList(createDigitalObject());
            }
        }

        /**
         * Creates a fedora object and connects it to workflow
         *
         * @param wfJobId workflow job id
         * @return list of created items with pid
         * @throws DigitalObjectException
         * @throws WorkflowException
         */
        public List<Item> createAndConnectToWorkflowJob(BigDecimal wfJobId, Locale locale) throws WorkflowException {
            if (isBatch()) {
                throw new IllegalArgumentException("Only single object (usually top level) is supported to be connected to job");
            } else if (wfJobId == null) {
                throw new IllegalArgumentException("Workwlow job id cannot be null");
            }
            WorkflowManager workflowManager = WorkflowManager.getInstance();
            MaterialFilter filter = new MaterialFilter();
            filter.setLocale(locale);
            filter.setJobId(wfJobId);
            filter.setType(MaterialType.PHYSICAL_DOCUMENT);
            List<MaterialView> physicalMaterials = workflowManager.findMaterial(filter);

            if (physicalMaterials.size() != 1) {
                throw new WorkflowException("Wrong number of physical materials").addUnexpectedError();
            }

            MaterialView digitalMaterial = workflowManager.createDigitalMaterialFromPhysical(this, physicalMaterials.get(0));

            Item item = new Item(digitalMaterial.getPid());
            item.setLabel(digitalMaterial.getLabel());

            return Collections.singletonList(item);
        }

        public Job getWfJob(String pid, Locale locale) {
            WorkflowManager workflowManager = WorkflowManager.getInstance();
            List<MaterialView> materials = findAllWorkflowJob(workflowManager, locale, 10000);
            for (MaterialView material : materials) {
                if (pid.equals(material.getPid())) {
                    return workflowManager.getJobs(material.getId());
                }
            }
            return null;

        }

        public List<MaterialView> findAllWorkflowJob(WorkflowManager workflowManager, Locale locale, int maxCount) {
            MaterialFilter filter = new MaterialFilter();
            filter.setLocale(locale);
            filter.setType(MaterialType.DIGITAL_OBJECT);
            filter.setMaxCount(maxCount);
            return workflowManager.findMaterial(filter);
        }

        public List<TaskView> getTask(BigDecimal jobId, Locale locale) {
            TaskFilter filter = new TaskFilter();
            filter.setLocale(locale);
            filter.setJobId(jobId);

            WorkflowManager workflowManager = WorkflowManager.getInstance();
            WorkflowProfiles workflowProfiles = WorkflowProfiles.getInstance();
            WorkflowDefinition workflow = workflowProfiles.getProfiles();
            List<TaskView> tasks = workflowManager.tasks().findTask(filter, workflow);

            return tasks;
        }

        private List<Item> createBatch() throws DigitalObjectException {
            ArrayList<Item> items = new ArrayList<>();
            while (hasNext()) {
                // adjust series params
                next();
                items.add(createDigitalObject());
            }
            return items;
        }

        public Item createDigitalObject() throws DigitalObjectException, DigitalObjectExistException {
            DigitalObjectHandler parentHandler = getParentHandler();

            LocalObject localObject = new LocalStorage().create(pid);
            localObject.setOwner(user.getUserName());
            DigitalObjectHandler doHandler = DigitalObjectManager.getDefault().createHandler(localObject);
            doHandler.setParameterParent(parentHandler);
            doHandler.setParameterUser(user);
            params.entrySet().forEach((entry) -> {
                doHandler.setParameter(entry.getKey(), entry.getValue());
            });

            RelationEditor relations = doHandler.relations();
            relations.setModel(modelId);
            relations.setOrganization(user.getOrganization());
            relations.setStatus(STATUS_NEW);

            String defaultProcessor = "all";
            if (appConfig != null) {
                defaultProcessor = appConfig.getImportConfiguration().getDefaultProcessor();
            }

            if ("user".equals(user.getRole())) {
                relations.setUser(user.getUserName());
            } else {
                relations.setUser(defaultProcessor);
            }
            if (getUserGroup() != null) {
                String grpPid = getUserGroup().getName();
                relations.setOwners(Collections.singletonList(grpPid));
            }
            relations.write(0, message);

            DescriptionMetadata<String> descMetadata = new DescriptionMetadata<>();
            descMetadata.setData(xml);
            doHandler.metadata().setMetadataAsXml(descMetadata, message, "new");

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

            if (tx != null) {
                tx.addPid(localObject.getPid());
            }

            Item item = new Item(localObject.getPid());
            item.setLabel(localObject.getLabel());
            item.setModel(modelId);
            item.setOwner(localObject.getOwner());
            item.setParentPid(parentPid);
            return item;
        }
    }

}
