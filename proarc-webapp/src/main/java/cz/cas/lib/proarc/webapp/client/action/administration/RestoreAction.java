/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.client.action.administration;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.DeleteAction;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import java.util.logging.Logger;

/**
 * The delete action invokes {@link Restorable} with selection received from
 * the source object using {@link Selectable}.
 *
 * @author Lukas Sykora
 */
public final class RestoreAction extends AbstractAction {

    private static final Logger LOG = Logger.getLogger(DeleteAction.class.getName());

    private final Restorable restorable;
    private final ClientMessages i18n;

    public RestoreAction(Restorable restorable, ClientMessages i18n) {
        super(i18n.RestoreDeletedObjectsAction_Title(), "16/restore.png", i18n.RestoreDeletedObjectsAction_Hint());
        this.restorable = restorable;
        this.i18n = i18n;
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        return selection != null && selection.length > 0;
    }

    @Override
    public void performAction(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        if (selection != null && selection.length > 0) {
            restore(selection);
        }
    }

    public void restore(Object[] selection) {
        restore(selection, null);
    }

    public void restore(Object[] selection, Record options) {
        if (selection != null && selection.length > 0) {
            restorable.restore(selection, options);
        }
    }

    /**
     * Implement to provide deletion of items.
     */
    public interface Restorable<T> {

        void restore(Object[] items);

        /**
         * Deletes items using options.
         * @param items items to deletete
         * @param options options to customize the delete
         */
        default void restore(Object[] items, T options) {
            restore(items);
        }

    }

    /**
     * Helper to delete records of {@link DataSource}.
     */
    public static final class RecordDeletable implements Restorable {

        private final DataSource ds;
        private final ClientMessages i18n;

        public RecordDeletable(DataSource ds, ClientMessages i18n) {
            if (ds == null) {
                throw new NullPointerException();
            }
            this.ds = ds;
            this.i18n = i18n;
        }

        @Override
        public void restore(Object[] items) {
            RestoreTask task = new RestoreTask(items, ds, i18n);
            task.restore();
        }

        private static class RestoreTask {

            private final Object[] items;
            private int itemIndex = 0;
            private final DataSource ds;
            private final ClientMessages i18n;

            public RestoreTask(Object[] items, DataSource ds, ClientMessages i18n) {
                this.items = items;
                this.ds = ds;
                this.i18n = i18n;
            }

            public void restore() {
                restoreItem();
            }

            private void restoreItem() {
                Record item = (Record) items[itemIndex];
                // TileGrid.removeSelectedData uses queuing support in case of multi-selection.
                // It will require extra support on server. For now remove data in separate requests.
                //thumbGrid.removeSelectedData();
                ds.removeData(item, new DSCallback() {

                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        if (RestConfig.isStatusOk(response)) {
                            itemIndex++;
                            if (itemIndex < items.length) {
                                restoreItem();
                            } else {
                                StatusView.getInstance().show(i18n.RestoreDeletedObjectsAction_Msg());
                            }
                        }
                    }
                });
            }
        }
    }
}
