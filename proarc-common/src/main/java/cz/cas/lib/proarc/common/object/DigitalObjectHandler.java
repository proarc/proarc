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

import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.WorkflowStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.util.HashMap;
import java.util.Map;

/**
 * Helps to handle digital object data and metadata. It is implemented as pluggable
 * in order to allow to change behavior according to type of handled objects.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectHandler {

    /**
     * NDK createDate for an issue.
     */
    public static final String PARAM_ISSUE_DATE = "series.mods.issueCreated";
    /**
     * NDK partName.
     */
    public static final String PARAM_PART_NUMBER = "series.mods.titleInfo.partNumber";
    /**
     * Special Signatura
     */
    public static final String PARAM_SIGNATURA = "series.mods.physicalLocation.signatura";
    public static final String PARAM_ISSUE_DATE_END_OF_RANGE = "series.mods.issueCreated.endOfRange";
    static final String PARAM_PARENT = DigitalObjectHandler.class.getName() + ".parent";
    static final String PARAM_USER = DigitalObjectHandler.class.getName() + ".user";

    private RelationEditor relationEditor;
    private DcStreamEditor dcMetadata;
    private final ProArcObject fobject;
    private DigitalObjectPlugin plugin;
    private final MetaModelRepository models;
    private final Map<String, Object> parameters = new HashMap<String, Object>();

    public DigitalObjectHandler(ProArcObject fobject, MetaModelRepository models) {
        if (fobject == null) {
            throw new NullPointerException("fobject");
        }
        this.fobject = fobject;
        this.models = models;
    }

    public ProArcObject getFedoraObject() {
        return fobject;
    }

    /**
     * Writes changes to a storage.
     *
     * @throws DigitalObjectException failure
     */
    public void commit() throws DigitalObjectException {
        getFedoraObject().flush();
    }

    // XXX replace with MetadataHandler<OaiDcType> impl

    /**
     * The administrative metadata of the digital object.
     *
     * @return metadata
     * @throws DigitalObjectException failure
     */
    public DcStreamEditor objectMetadata() throws DigitalObjectException {
        if (dcMetadata == null) {
            dcMetadata = new DcStreamEditor(getFedoraObject());
        }
        return dcMetadata;
    }
//    public MetadataHandler<OaiDcType> objectMetadata() {
//        if (dcMetadata == null) {
//            dcMetadata = new DcAdminMetadataHandler();
//        }
//        return dcMetadata;
//    }

    /**
     * The description metadata.
     *
     * @param <T> type of metadata
     * @return metadata
     * @throws DigitalObjectException failure
     */
    public <T> MetadataHandler<T> metadata() throws DigitalObjectException {
        @SuppressWarnings("unchecked")
        HasMetadataHandler<T> handlerProvider = getPlugin().getHandlerProvider(HasMetadataHandler.class);
        if (handlerProvider != null) {
            return handlerProvider.createMetadataHandler(this);
        } else {
            throw new UnsupportedOperationException("Missing description metadata handler for " + getPlugin());
        }
    }

    public DisseminationHandler dissemination(String dsId) throws DigitalObjectException {
        HasDisseminationHandler handlerProvider = getPlugin().getHandlerProvider(HasDisseminationHandler.class);
        if (handlerProvider != null) {
            return handlerProvider.createDisseminationHandler(dsId, this);
        } else {
            return new DefaultDisseminationHandler(dsId, this);
        }
    }

    public RelationEditor relations() throws DigitalObjectException {
        if (relationEditor == null) {
            relationEditor = new RelationEditor(getFedoraObject());
        }
        return relationEditor;
    }

    public MetaModel getModel() throws DigitalObjectException {
        MetaModel model;
        if (getFedoraObject() instanceof WorkflowStorage.WorkflowObject) {
            WorkflowStorage.WorkflowObject workflowObject = (WorkflowStorage.WorkflowObject) getFedoraObject();
            model = models.find(workflowObject.getModel());
        } else {
            // XXX optimize not to require rels-ext fetch
            String modelId = relations().getModel();
            model = models.find(modelId);
        }
        return model;
    }

    public DigitalObjectHandler getParameterParent() {
        return getParameter(PARAM_PARENT);
    }

    public void setParameterParent(DigitalObjectHandler parent) {
        setParameter(PARAM_PARENT, parent);
    }

    public UserProfile getParameterUser() {
        return getParameter(PARAM_USER);
    }

    public void setParameterUser(UserProfile user) {
        setParameter(PARAM_USER, user);
    }

    /**
     * Sets parameters to customize object handling. Parameters are shared
     * among handler implementations.
     *
     * @param name  parameter name
     * @param value parameter value
     */
    public void setParameter(String name, Object value) {
        parameters.put(name, value);
    }

    public <P> P getParameter(String name) {
        return (P) parameters.get(name);
    }

    private DigitalObjectPlugin getPlugin() throws DigitalObjectException {
        if (plugin == null) {
            MetaModel model = getModel();
            plugin = model.getPlugin();
        }
        return plugin;
    }

//    private static final class DcAdminMetadataHandler implements MetadataHandler<OaiDcType> {
//
//    }

}
