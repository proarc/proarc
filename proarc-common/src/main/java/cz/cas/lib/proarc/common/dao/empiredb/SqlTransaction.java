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
import java.sql.SQLException;
import org.apache.empire.db.DBContext;

/**
 *
 * @author Jan Pokorsky
 */
public final class SqlTransaction implements Transaction {
    
    private final DBContext context;

    public SqlTransaction(DBContext context) {
        try {
            context.getConnection().setAutoCommit(false);
            this.context = context;
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    public DBContext getContext() {
        return context;
    }

    public void commit() {
        context.commit();
    }

    public void rollback() {
        context.rollback();
    }

    public void close() {
        context.discard();
    }
}
