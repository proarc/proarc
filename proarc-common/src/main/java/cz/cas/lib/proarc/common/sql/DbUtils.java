/*
 * Copyright (C) 2012 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.sql;

import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.storage.fedora.FedoraTransaction;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

/**
 *
 * @author Jan Pokorsky
 */
public final class DbUtils {

    private static final Logger LOG = Logger.getLogger(DbUtils.class.getName());

    public static DataSource getProarcSource() throws NamingException {
        DataSource source = InitialContext.doLookup("java:/comp/env/jdbc/proarc");
        if (source == null) {
            throw new IllegalStateException("Cannot find 'jdbc/proarc' resource!");
        }
        return source;
    }

    public static void close(Connection c) {
        close(c, false);
    }

    public static void close(Connection c, boolean rollback) {
        close(c, rollback, null);
    }

    public static void close(Connection c, String msg) {
        close(c, false, msg);
    }

    public static void close(Connection c, boolean rollback, String msg) {
        if (rollback) {
            rollback(c, msg);
        }
        try {
            c.close();
        } catch (SQLException ex) {
            LOG.log(Level.SEVERE, msg, ex);
        }
    }

    public static void close(ResultSet rs) {
        try {
            rs.close();
        } catch (SQLException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public static void close(Statement s) {
        try {
            s.close();
        } catch (SQLException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public static void rollback(Connection c) {
        rollback(c, null);
    }

    public static void rollback(Connection c, String msg) {
        try {
            c.rollback();
        } catch (SQLException ex) {
            LOG.log(Level.SEVERE, msg, ex);
        }
    }

    public static void close(FedoraTransaction fedoraTransaction) {
        if (fedoraTransaction != null) {
            fedoraTransaction.close();
        }
    }

    public static void close(Transaction transaction, FedoraTransaction fedoraTransaction) {
        if (transaction != null) {
            transaction.close();
        }
        if (fedoraTransaction != null) {
            fedoraTransaction.close();
        }
    }

    public static void rollback(Transaction transaction, FedoraTransaction fedoraTransaction) {
        if (transaction != null) {
            transaction.rollback();
        }
        if (fedoraTransaction != null) {
            fedoraTransaction.rollback();
        }
    }

    public static void commit(Transaction transaction, FedoraTransaction fedoraTransaction) {
        if (transaction != null) {
            transaction.commit();
        }
        if (fedoraTransaction != null) {
            fedoraTransaction.commit();
        }
    }
}
