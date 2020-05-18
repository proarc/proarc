package cz.cas.lib.proarc.webapp.client.widget;

import java.util.LinkedHashMap;

public class UserRole {

    public static final String ROLE_SUPERADMIN = "superAdmin";
    public static final String ROLE_ADMIN = "admin";
    public static final String ROLE_USER = "user";

    public static LinkedHashMap<String, String> getMap() {
        LinkedHashMap<String, String> valueMap = new LinkedHashMap();
        valueMap.put(ROLE_SUPERADMIN, "Super Admin");
        valueMap.put(ROLE_ADMIN, "Admin");
        valueMap.put(ROLE_USER, "User");
        return valueMap;
    }
}
