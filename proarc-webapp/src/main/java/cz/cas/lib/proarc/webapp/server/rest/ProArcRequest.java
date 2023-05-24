package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.ws.rs.DefaultValue;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

public class ProArcRequest {

    /**
     * {@link cz.cas.lib.proarc.webapp.server.rest.v1.DigitalObjectResourceV1.DeleteObjectRequest} request body.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class DeleteObjectRequest {

        @XmlElement(name = ImportResourceApi.BATCHITEM_BATCHID)
        public Integer batchId;

        @XmlElement(name = DigitalObjectResourceApi.DELETE_PID_PARAM)
        public List<String> pids;

        @XmlElement(name = DigitalObjectResourceApi.DELETE_HIERARCHY_PARAM)
        @DefaultValue("true")
        public boolean hierarchy;

        @XmlElement(name = DigitalObjectResourceApi.DELETE_PURGE_PARAM)
        @DefaultValue("false")
        public boolean purge;

        @XmlElement(name = DigitalObjectResourceApi.DELETE_RESTORE_PARAM)
        @DefaultValue("false")
        public boolean restore;

        public Set<String> getPidsAsSet() {
            return new HashSet<String>(pids);
        }
    }

    /**
     * {@link DigitalObjectResourceV1.SetMember} request body.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class SetMemberRequest {

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PARENT)
        public String parentPid;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID)
        public Integer batchId;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PID)
        public List<String> toSetPids;
    }

    /**
     * The helper for {@link cz.cas.lib.proarc.webapp.server.rest.v1.DigitalObjectResourceV1.MoveMembers} request body.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class MoveMembersRequest {

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_MOVE_SRCPID)
        public String srcParentPid;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_MOVE_DSTPID)
        public String dstParentPid;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID)
        public Integer batchId;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PID)
        public List<String> pids;
    }

    /**
     * {@link DigitalObjectResource.copyDescriptionMetadataToPages} request body.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class CopyPagesMetadataRequest {

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_SOURCE_PIDS)
        public List<String> sourcePidsArray;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_DESTINATION_PIDS)
        public List<String> destinationPidsArray;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID)
        public Integer batchId;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_TYPE)
        public Boolean copyPageType;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_INDEX)
        public Boolean copyPageIndex;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_NUMBER)
        public Boolean copyPageNumber;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_POSITION)
        public Boolean copyPagePosition;
    }
}
