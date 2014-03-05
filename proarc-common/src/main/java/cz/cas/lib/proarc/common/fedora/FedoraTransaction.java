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
package cz.cas.lib.proarc.common.fedora;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The transaction for operations over the Fedora storage.
 *
 * <p>For now it can rollback just newly created objects.
 * <p>XXX support DigitalObjectHandler
 *
 * @author Jan Pokorsky
 */
public class FedoraTransaction implements Transaction {

    private static final Logger LOG = Logger.getLogger(FedoraTransaction.class.getName());
    private Set<String> newPids = new HashSet<String>();
    private final RemoteStorage remoteStorage;

    public FedoraTransaction(RemoteStorage remoteStorage) {
        this.remoteStorage = remoteStorage;
    }

    public void addPid(String pid) {
        newPids.add(pid);
    }

    @Override
    public void commit() {
        newPids.clear();
    }

    @Override
    public void rollback() {
        // purge
        for (String pid : newPids) {
            RemoteObject robject = remoteStorage.find(pid);
            try {
                FedoraClient.purgeObject(pid).logMessage("rollback").execute(robject.getClient());
            } catch (FedoraClientException ex) {
                LOG.log(Level.SEVERE, pid, ex);
            }
        }
    }

    @Override
    public void close() {
        rollback();
    }

    public RemoteStorage getRemoteStorage() {
        return remoteStorage;
    }

}
