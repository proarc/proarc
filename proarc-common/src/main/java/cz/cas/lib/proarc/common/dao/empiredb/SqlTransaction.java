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
package cz.cas.lib.proarc.common.dao.empiredb;

import cz.cas.lib.proarc.common.dao.Transaction;
import java.sql.Connection;
import java.sql.SQLException;

/**
 *
 * @author Jan Pokorsky
 */
public final class SqlTransaction implements Transaction {
    
    private final Connection c;

    public SqlTransaction(Connection c) {
        try {
            c.setAutoCommit(false);
            this.c = c;
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    public Connection getConnection() {
        return c;
    }

    public void commit() {
        try {
            c.commit();
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    public void rollback() {
        try {
            c.rollback();
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    public void close() {
        try {
            c.close();
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

}
