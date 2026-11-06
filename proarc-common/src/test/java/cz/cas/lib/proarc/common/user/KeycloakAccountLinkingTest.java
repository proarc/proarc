package cz.cas.lib.proarc.common.user;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.UserDao;
import cz.cas.lib.proarc.common.dao.empiredb.SqlTransaction;
import java.io.File;
import java.sql.Connection;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import javax.sql.DataSource;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.easymock.EasyMock.*;
import static org.junit.jupiter.api.Assertions.*;

class KeycloakAccountLinkingTest {
    @TempDir
    File tempDir;
    private UserDao users;
    private DaoFactory daos;
    private Connection connection;
    private UserManager manager;

    @BeforeEach
    void setup() throws Exception {
        users = createMock(UserDao.class);
        daos = createMock(DaoFactory.class);
        connection = createNiceMock(Connection.class);
        replay(connection);
        SqlTransaction tx = new SqlTransaction(connection);
        new File(tempDir, AppConfiguration.CONFIG_FILE_NAME).createNewFile();
        AppConfiguration config = AppConfigurationFactory.getInstance().create(
                new HashMap<>(Map.of(AppConfiguration.PROPERTY_APP_HOME, tempDir.getAbsolutePath())));
        manager = new UserManagerSql(createNiceMock(DataSource.class), config, daos);
        expect(daos.createTransaction()).andReturn(tx);
        expect(daos.createUser()).andReturn(users);
        users.setTransaction(tx);
    }

    @Test
    void existingIdentityIgnoresChangedOrMissingEmail() {
        UserProfile linked = user(1, "old@example.org", "subject", "issuer");
        expect(users.find(null, null, "subject", "issuer", null)).andReturn(List.of(linked));
        replay(users, daos);
        assertSame(linked, manager.resolveKeycloakIdentity("issuer", "subject", "different@example.org"));
        verify(users, daos);
    }

    @Test
    void firstLinkNormalizesEmailAndPersistsOnlyIdentity() {
        UserProfile unlinked = user(1, " Person@Example.org ", null, " ");
        expect(users.find(null, null, "subject", "issuer", null)).andReturn(List.of());
        expect(users.find(null, null, null, null, null)).andReturn(List.of(unlinked));
        expect(users.linkIdentity(1, "person@example.org", "issuer", "subject")).andReturn(true);
        expect(users.find(1)).andReturn(user(1, " Person@Example.org ", "subject", "issuer"));
        replay(users, daos);
        UserProfile result = manager.resolveKeycloakIdentity("issuer", "subject", "person@example.org");
        assertEquals("subject", result.getRemoteName());
        assertEquals("issuer", result.getRemoteType());
        verify(users, daos);
    }

    @Test
    void refusesDuplicateEmailsIncludingAlreadyLinkedAccounts() {
        expect(users.find(null, null, "subject", "issuer", null)).andReturn(List.of());
        expect(users.find(null, null, null, null, null)).andReturn(List.of(
                user(1, "user@example.org", null, null), user(2, "USER@example.org", "other", "issuer")));
        replay(users, daos);
        assertNull(manager.resolveKeycloakIdentity("issuer", "subject", "user@example.org"));
        verify(users, daos);
    }

    @Test
    void refusesToOverwriteDifferentIdentity() {
        denied(user(1, "user@example.org", "other", "issuer"));
    }

    @Test
    void refusesPartiallyClearedIdentity() {
        denied(user(1, "user@example.org", null, "issuer"));
    }

    @Test
    void duplicateIdentitiesNeverFallBackToEmail() {
        expect(users.find(null, null, "subject", "issuer", null)).andReturn(List.of(
                user(1, "one@example.org", "subject", "issuer"), user(2, "two@example.org", "subject", "issuer")));
        replay(users, daos);
        assertNull(manager.resolveKeycloakIdentity("issuer", "subject", "one@example.org"));
        verify(users, daos);
    }

    @Test
    void concurrentLinkLossDoesNotAuthenticate() {
        expect(users.find(null, null, "subject", "issuer", null)).andReturn(List.of());
        expect(users.find(null, null, null, null, null)).andReturn(List.of(user(1, "user@example.org", "", "")));
        expect(users.linkIdentity(1, "user@example.org", "issuer", "subject")).andReturn(false);
        replay(users, daos);
        assertNull(manager.resolveKeycloakIdentity("issuer", "subject", "user@example.org"));
        verify(users, daos);
    }

    private void denied(UserProfile candidate) {
        expect(users.find(null, null, "subject", "issuer", null)).andReturn(List.of());
        expect(users.find(null, null, null, null, null)).andReturn(List.of(candidate));
        replay(users, daos);
        assertNull(manager.resolveKeycloakIdentity("issuer", "subject", "user@example.org"));
        verify(users, daos);
    }

    private static UserProfile user(int id, String email, String subject, String issuer) {
        UserProfile user = new UserProfile();
        user.setId(id);
        user.setEmail(email);
        user.setRemoteName(subject);
        user.setRemoteType(issuer);
        return user;
    }
}
