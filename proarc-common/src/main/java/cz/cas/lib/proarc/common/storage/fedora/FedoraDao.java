/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.storage.fedora;

import cz.cas.lib.proarc.common.dao.Dao;
import cz.cas.lib.proarc.common.dao.Transaction;

/**
 * DAO for the Fedora storage.
 *
 * @author Jan Pokorsky
 */
public abstract class FedoraDao implements Dao {

    protected FedoraTransaction tx;

    public final void setTransaction(FedoraTransaction tx) {
        if (tx == null) {
            throw new NullPointerException("tx");
        }
        this.tx = tx;
    }

    @Override
    public final void setTransaction(Transaction tx) {
        if (tx instanceof FedoraTransaction) {
            setTransaction((FedoraTransaction) tx);
        } else if (tx == null) {
            throw new NullPointerException("tx");
        } else {
            throw new IllegalArgumentException(tx.getClass().toString());
        }
    }

    public final FedoraStorage getRemoteStorage() {
        return tx.getRemoteStorage();
    }

}
