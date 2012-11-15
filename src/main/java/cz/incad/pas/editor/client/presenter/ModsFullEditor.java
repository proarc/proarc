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
package cz.incad.pas.editor.client.presenter;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.fi.muni.xkremser.editor.client.mods.ModsCollectionClient;
import cz.fi.muni.xkremser.editor.client.mods.ModsTypeClient;
import cz.fi.muni.xkremser.editor.client.view.ModsTab;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.rpc.ModsGwtRecord;
import cz.incad.pas.editor.client.rpc.ModsGwtServiceAsync;
import cz.incad.pas.editor.client.widget.DatastreamEditor;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Wraps {@link ModsTab} editor as data stream editor.
 *
 * @author Jan Pokorsky
 */
public final class ModsFullEditor implements DatastreamEditor, Refreshable {

    private static final Logger LOG = Logger.getLogger(ModsFullEditor.class.getName());

    private ModsTab wrappedEditor;
    private ModsGwtRecord editedMods;
    private final VLayout modsContainer;
    private String pid;
    private final ClientMessages i18n;

    public ModsFullEditor(ClientMessages i18n) {
        this.i18n = i18n;
        modsContainer = new VLayout();
        modsContainer.setHeight100();
        modsContainer.setWidth100();
        modsContainer.setAlign(VerticalAlignment.CENTER);
        modsContainer.setDefaultLayoutAlign(Alignment.CENTER);
    }

    @Override
    public void edit(String pid, String batchId, MetaModelRecord model) {
        this.pid = pid;
        load(pid);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        return new Canvas[0];
    }

    @Override
    public void refresh() {
        load(pid);
    }

    private void load(final String pid) {
        if (pid == null) {
            setFailedView();
        }
        editedMods = null;
        wrappedEditor = null;
        modsContainer.setMembers(new Img("[SKIN]/loadingSmall.gif", 16, 16));
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        service.read(pid, new AsyncCallback<ModsGwtRecord>() {

            @Override
            public void onFailure(Throwable caught) {
                LOG.log(Level.SEVERE, "read.onFailure: " + pid, caught);
                setFailedView();
                SC.warn(ClientUtils.format("Loading FAILED!<p>pid: %s<p>%s",
                        pid, caught.getLocalizedMessage()));
            }

            @Override
            public void onSuccess(ModsGwtRecord modsTransport) {
                ModsCollectionClient mc = modsTransport.getMods();
                ClientUtils.fine(LOG, "loadFull mods: %s", mc);
                loadEditor(modsTransport);
            }
        });
    }

    private void setFailedView() {
        modsContainer.setMembers(new Img("[SKIN]/Dialog/warn.png", 2 * 16, 2 * 16));
    }

    private void loadEditor(ModsGwtRecord modsRecord) {
        editedMods = modsRecord;
        modsContainer.setContents("");
        ModsCollectionClient modsCollection = modsRecord.getMods();
        wrappedEditor = new ModsTab(1, modsCollection);
        VLayout modsLayout = wrappedEditor.getModsLayout();
        modsContainer.setMembers(modsLayout);
    }

    public void save(BooleanCallback callback) {
        if (editedMods == null) {
            callback.execute(Boolean.TRUE);
            return ;
        }
        ModsTypeClient mc = wrappedEditor.getMods();
        ModsCollectionClient mcc = new ModsCollectionClient();
        mcc.setMods(Arrays.asList(mc));
        editedMods.setMods(mcc);
        saveFullData(pid, editedMods, callback);
    }

    private void saveFullData(final String pid, final ModsGwtRecord mods, final BooleanCallback callback) {
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        service.write(pid, mods, new AsyncCallback<String>() {

            @Override
            public void onFailure(Throwable caught) {
                LOG.log(Level.SEVERE, "write.onFailure: " + pid, caught);
                callback.execute(Boolean.FALSE);
                SC.warn(ClientUtils.format("Saving FAILED!<p>pid: %s<p>%s",
                        pid, caught.getLocalizedMessage()));
            }

            @Override
            public void onSuccess(String newOrExistingId) {
                ModsFullEditor.this.pid = newOrExistingId;
                ClientUtils.fine(LOG, "full MODS write.onSuccess: %s", newOrExistingId);
                callback.execute(Boolean.TRUE);
            }
        });
    }

    @Override
    public Canvas getUI() {
        return modsContainer;
    }

}
