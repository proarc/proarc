package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
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
        FedoraObject fedoraObject = dom.find(pid, null);
        setState(fedoraObject, status);
    }

    public static void setState(FedoraObject fedoraObject, String status) throws DigitalObjectException {
        if (validValue(status)) {
            RelationEditor relations = new RelationEditor(fedoraObject);
            relations.setStatus(status);
            relations.write(relations.getLastModified(), "Status update: " + status);
            fedoraObject.flush();
            if (STATUS_EXPORTED.equals(status)) {
                setChildrenStatus(relations.getMembers(), status);
            }
        } else {
            throw new DigitalObjectException(fedoraObject.getPid(), "Wrong type of status " + status);
        }
    }

    private static void setChildrenStatus(List<String> members, String status) throws DigitalObjectException {
        for (String pid : members) {
            setState(pid, status);
        }
    }
}
