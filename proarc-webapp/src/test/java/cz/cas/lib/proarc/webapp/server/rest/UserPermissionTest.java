package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.user.UserProfile;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class UserPermissionTest {

    @Test
    void reviewPermissionIsDisabledByDefaultIncludingAdministrators() {
        UserProfile user = user(1000);
        user.setSysAdminFunction(true);
        assertFalse(user.getCheckBatchFunction());
        assertFalse(UserPermission.hasPermission(user, RestConsts.PERMISSION_FUNCTION_CHECK_BATCH));
        user.setCheckBatchFunction(null);
        assertFalse(user.hasCheckBatchFunction());
    }

    @Test
    void ownerCanAccessOwnBatchByIdValue() {
        assertTrue(UserPermission.canAccessImportBatch(user(1000), user(1000)));
    }

    @Test
    void ordinaryUserCannotAccessAnotherUsersPrivateBatch() {
        UserProfile user = user(1000);
        user.setPrepareBatchFunction(true);
        assertFalse(UserPermission.canAccessImportBatch(user, user(2000)));
        assertFalse(UserPermission.canAccessImportBatch(user, null));
    }

    @Test
    void preparersBatchesRemainShared() {
        UserProfile owner = user(2000);
        owner.setPrepareBatchFunction(true);
        assertTrue(UserPermission.canAccessImportBatch(user(1000), owner));
    }

    @Test
    void reviewerCanAccessOtherOrganizationsBatchesWithoutPreparationPermission() {
        UserProfile reviewer = user(1000);
        reviewer.setOrganization("library-a");
        reviewer.setCheckBatchFunction(true);
        UserProfile owner = user(2000);
        owner.setOrganization("library-b");
        assertTrue(UserPermission.hasPermission(reviewer, RestConsts.PERMISSION_FUNCTION_CHECK_BATCH));
        assertTrue(UserPermission.canAccessImportBatch(reviewer, owner));
        assertFalse(owner.hasPrepareBatchFunction());
        assertFalse(UserPermission.hasPermission(reviewer, RestConsts.PERMISSION_FUNCTION_SYS_ADMIN));
        assertFalse(UserPermission.hasPermission(reviewer, RestConsts.PERMISSION_FUNCTION_DELETE_ACTION));

        reviewer.setCheckBatchFunction(false);
        assertFalse(UserPermission.canAccessImportBatch(reviewer, owner));
    }

    private UserProfile user(int id) {
        UserProfile user = new UserProfile();
        user.setId(id);
        return user;
    }
}
