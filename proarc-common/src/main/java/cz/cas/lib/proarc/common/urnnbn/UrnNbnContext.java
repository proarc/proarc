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
package cz.cas.lib.proarc.common.urnnbn;

import cz.cas.lib.proarc.common.export.mets.JhoveContext;
import cz.cas.lib.proarc.urnnbn.ResolverClient;

/**
 *
 * @author Jan Pokorsky
 */
public class UrnNbnContext {

    private UrnNbnStatusHandler status;
    private ResolverClient client;
    private JhoveContext jhoveContext;
    private String updateCzidloRecordOperation;
    private String updateCzidloRecordIdentifier;
    private boolean registerNewPid = false;
    private boolean registerAgainPid = false;
    private boolean invalidateUrnNbn = false;
    private boolean createSuccessor = false;
    private boolean updateCzidloRecord = false;


    public ResolverClient getClient() {
        return client;
    }

    public void setClient(ResolverClient client) {
        this.client = client;
    }

    public UrnNbnStatusHandler getStatus() {
        return status == null ? UrnNbnStatusHandler.DEFAULT : status;
    }

    public void setStatus(UrnNbnStatusHandler status) {
        this.status = status;
    }

    public JhoveContext getJhoveContext() {
        return jhoveContext;
    }

    public void setJhoveContext(JhoveContext jhoveContext) {
        this.jhoveContext = jhoveContext;
    }

    public boolean isRegisterNewPid() {
        return registerNewPid;
    }

    public void setRegisterNewPid(boolean registerNewPid) {
        this.registerNewPid = registerNewPid;
    }

    public boolean isInvalidateUrnNbn() {
        return invalidateUrnNbn;
    }

    public void setInvalidateUrnNbn(boolean invalidateUrnNbn) {
        this.invalidateUrnNbn = invalidateUrnNbn;
    }

    public boolean isCreateSuccessor() {
        return createSuccessor;
    }

    public void setCreateSuccessor(boolean createSuccessor) {
        this.createSuccessor = createSuccessor;
    }

    public boolean isRegisterAgainPid() {
        return registerAgainPid;
    }

    public void setRegisterAgainPid(boolean registerAgainPid) {
        this.registerAgainPid = registerAgainPid;
    }

    public boolean isUpdateCzidloRecord() {
        return updateCzidloRecord;
    }

    public void setUpdateCzidloRecord(boolean updateCzidloRecord) {
        this.updateCzidloRecord = updateCzidloRecord;
    }

    public String getUpdateCzidloRecordOperation() {
        return updateCzidloRecordOperation;
    }

    public void setUpdateCzidloRecordOperation(String updateCzidloRecordOperation) {
        this.updateCzidloRecordOperation = updateCzidloRecordOperation;
    }

    public String getUpdateCzidloRecordIdentifier() {
        return updateCzidloRecordIdentifier;
    }

    public void setUpdateCzidloRecordIdentifier(String updateCzidloRecordIdentifier) {
        this.updateCzidloRecordIdentifier = updateCzidloRecordIdentifier;
    }
}
