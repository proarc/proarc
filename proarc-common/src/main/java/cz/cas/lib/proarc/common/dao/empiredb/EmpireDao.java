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

import cz.cas.lib.proarc.common.dao.Dao;
import cz.cas.lib.proarc.common.dao.Transaction;
import org.apache.empire.db.DBContext;

/**
 *
 * @author Jan Pokorsky
 */
public abstract class EmpireDao implements Dao {

    protected final ProarcDatabase db;
    protected SqlTransaction tx;

    public EmpireDao(ProarcDatabase db) {
        this.db = db;
    }

    public final void setTransaction(SqlTransaction tx) {
        if (tx == null) {
            throw new NullPointerException("tx");
        }
        this.tx = tx;
    }

    public final void setTransaction(Transaction tx) {
        if (tx instanceof SqlTransaction) {
            setTransaction((SqlTransaction) tx);
        } else if (tx == null) {
            throw new NullPointerException("tx");
        } else {
            throw new IllegalArgumentException(tx.getClass().toString());
        }
    }

    protected final DBContext getContext() {
        return tx.getContext();
    }

}
