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
     * {@link DigitalObjectResource.DeleteObjectRequest} request body.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class DeleteObjectRequest {

        @XmlElement(name = ImportResourceApi.BATCHITEM_BATCHID)
        Integer batchId;

        @XmlElement(name = DigitalObjectResourceApi.DELETE_PID_PARAM)
        List<String> pids;

        @XmlElement(name = DigitalObjectResourceApi.DELETE_HIERARCHY_PARAM)
        @DefaultValue("true")
        boolean hierarchy;

        @XmlElement(name = DigitalObjectResourceApi.DELETE_PURGE_PARAM)
        @DefaultValue("false")
        boolean purge;

        @XmlElement(name = DigitalObjectResourceApi.DELETE_RESTORE_PARAM)
        @DefaultValue("false")
        boolean restore;

        public Set<String> getPidsAsSet() {
            return new HashSet<String>(pids);
        }
    }

    /**
     * {@link DigitalObjectResource.SetMember} request body.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class SetMemberRequest {

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PARENT)
        String parentPid;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID)
        Integer batchId;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PID)
        List<String> toSetPids;
    }

    /**
     * The helper for {@link DigitalObjectResource.MoveMembers} request body.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class MoveMembersRequest {

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_MOVE_SRCPID)
        String srcParentPid;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_MOVE_DSTPID)
        String dstParentPid;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID)
        Integer batchId;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PID)
        List<String> pids;
    }

    /**
     * {@link DigitalObjectResource.copyDescriptionMetadataToPages} request body.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class CopyPagesMetadataRequest {

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_SOURCE_PIDS)
        List<String> sourcePidsArray;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_DESTINATION_PIDS)
        List<String> destinationPidsArray;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID)
        Integer batchId;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_TYPE)
        Boolean copyPageType;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_INDEX)
        Boolean copyPageIndex;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_NUMBER)
        Boolean copyPageNumber;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_POSITION)
        Boolean copyPagePosition;
    }
}
