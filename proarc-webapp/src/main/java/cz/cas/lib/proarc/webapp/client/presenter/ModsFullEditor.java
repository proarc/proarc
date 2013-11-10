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
package cz.cas.lib.proarc.webapp.client.presenter;

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
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.rpc.ModsGwtRecord;
import cz.cas.lib.proarc.webapp.client.rpc.ModsGwtServiceAsync;
import cz.cas.lib.proarc.webapp.client.widget.DatastreamEditor;
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
    private DigitalObject digitalObject;
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
    public void edit(DigitalObject digitalObject) {
        this.digitalObject = digitalObject;
        load();
    }

    @Override
    public void focus() {
        if (modsContainer != null) {
            modsContainer.focus();
        }
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
        load();
    }

    private void load() {
        if (digitalObject == null) {
            setFailedView();
        }
        editedMods = null;
        wrappedEditor = null;
        modsContainer.setMembers(new Img("[SKIN]/loadingSmall.gif", 16, 16));
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        service.read(digitalObject.getPid(), digitalObject.getBatchIdAsInt(), new AsyncCallback<ModsGwtRecord>() {

            @Override
            public void onFailure(Throwable caught) {
                LOG.log(Level.SEVERE, "read.onFailure: " + digitalObject, caught);
                setFailedView();
                SC.warn(ClientUtils.format("Loading FAILED!<p>%s<p>%s",
                        digitalObject, caught.getLocalizedMessage()));
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
        saveFullData(editedMods, callback);
    }

    private void saveFullData(final ModsGwtRecord mods, final BooleanCallback callback) {
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        service.write(digitalObject.getPid(), digitalObject.getBatchIdAsInt(), mods, new AsyncCallback<String>() {

            @Override
            public void onFailure(Throwable caught) {
                LOG.log(Level.SEVERE, "write.onFailure: " + digitalObject, caught);
                callback.execute(Boolean.FALSE);
                SC.warn(ClientUtils.format("Saving FAILED!<p>%s<p>%s",
                        digitalObject, caught.getLocalizedMessage()));
            }

            @Override
            public void onSuccess(String newOrExistingId) {
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
