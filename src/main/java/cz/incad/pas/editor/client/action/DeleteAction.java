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
package cz.incad.pas.editor.client.action;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import cz.incad.pas.editor.client.PasEditorMessages;

/**
 * The delete action invokes {@link Deletable} with selection received from
 * the source object using {@link Selectable}.
 *
 * @author Jan Pokorsky
 */
public final class DeleteAction extends AbstractAction {

    private final Deletable deletable;
    private final PasEditorMessages i18nPas;

    public DeleteAction(Deletable deletable, PasEditorMessages i18nPas) {
        super(i18nPas.DeleteAction_Title(), "[SKIN]/actions/remove.png", i18nPas.DeleteAction_Hint());
        this.deletable = deletable;
        this.i18nPas = i18nPas;
    }

    @Override
    public void performAction(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        if (selection != null && selection.length > 0) {
            askAndDelete(selection);
        }
    }

    public void askAndDelete(final Object[] selection) {
        if (selection == null || selection.length == 0) {
            return ;
        }
        SC.ask(i18nPas.DeleteAction_Window_Title(),
                i18nPas.DeleteAction_Window_Msg(String.valueOf(selection.length)),
                new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (value != null && value) {
                    delete(selection);
                }
            }
        });
    }
    
    public void delete(Object[] selection) {
        if (selection != null && selection.length > 0) {
            deletable.delete(selection);
        }
    }

    /**
     * Implement to provide deletion of items.
     */
    public interface Deletable {

        void delete(Object[] items);

    }

    /**
     * Helper to delete records of {@link DataSource}.
     */
    public static final class RecordDeletable implements Deletable {

        private final DataSource ds;

        public RecordDeletable(DataSource ds) {
            if (ds == null) {
                throw new NullPointerException();
            }
            this.ds = ds;
        }

        @Override
        public void delete(Object[] items) {
            for (Object item : items) {
                if (item instanceof Record) {
                // TileGrid.removeSelectedData uses queuing support in case of multi-selection.
                // It will require extra support on server. For now remove data in separate requests.
                //thumbGrid.removeSelectedData();
                    ds.removeData((Record) item);
                }
            }
        }

    }
}
