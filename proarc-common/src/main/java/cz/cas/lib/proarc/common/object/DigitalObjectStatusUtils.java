package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DigitalObjectStatusUtils {
    public static final String STATUS_NEW = "new";
    public static final String STATUS_ASSIGN = "assign";
    public static final String STATUS_CONNECTED = "connected";
    public static final String STATUS_PROCESSING = "processing";
    public static final String STATUS_DESCRIBED = "described";
    public static final String STATUS_EXPORTED = "exported";

    static final Set<String> VALID_VALUES = Collections.unmodifiableSet(new HashSet<>(
            Arrays.asList(STATUS_NEW, STATUS_ASSIGN, STATUS_CONNECTED, STATUS_DESCRIBED, STATUS_PROCESSING, STATUS_EXPORTED)));

    public static boolean validValue(String status) {
        return VALID_VALUES.contains(status);
    }

    public static void setState(String pid, String status) throws DigitalObjectException {
        if (pid == null) {
            return;
        }
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject proArcObject = dom.find(pid, null);
        setState(proArcObject, status);
    }

    public static void setState(ProArcObject proArcObject, String status) throws DigitalObjectException {
        if (validValue(status)) {
            RelationEditor relations = new RelationEditor(proArcObject);
            relations.setStatus(status);
            relations.write(relations.getLastModified(), "Status update: " + status);
            proArcObject.flush();
            if (STATUS_EXPORTED.equals(status)) {
                setChildrenStatus(relations.getMembers(), status);
            }
        } else {
            throw new DigitalObjectException(proArcObject.getPid(), "Wrong type of status " + status);
        }
    }

    private static void setChildrenStatus(List<String> members, String status) throws DigitalObjectException {
        for (String pid : members) {
            setState(pid, status);
        }
    }
}
