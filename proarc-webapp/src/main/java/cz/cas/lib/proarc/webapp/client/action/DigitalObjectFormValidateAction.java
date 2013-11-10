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
package cz.cas.lib.proarc.webapp.client.action;

import com.google.gwt.core.client.Callback;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.ModsCustomEditor;
import cz.cas.lib.proarc.webapp.client.widget.ProgressTracker;

/**
 * Validates list of digital objects gained from
 * a {@link Validatable} action source.
 *
 * <p>For now it runs fully on the client side. It uses MODS custom forms
 * for validation. The result of validation is shown inside a window and error
 * messages are added to each invalid row of the list. It is the list owner
 * responsibility to clear errors (e.g. on refresh).
 *
 * <p>The action shows processing status with the {@link ProgressTracker}.
 *
 * <p>The action is a singleton to share form instances.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectFormValidateAction extends AbstractAction {

    private static DigitalObjectFormValidateAction INSTANCE;

    private final ModsCustomEditor validator;
    private final ClientMessages i18n;
    private final ProgressTracker progress;

    public static DigitalObjectFormValidateAction getInstance(ClientMessages i18n) {
        if (INSTANCE == null) {
            INSTANCE = new DigitalObjectFormValidateAction(i18n);
        }
        return INSTANCE;
    }

    private DigitalObjectFormValidateAction(ClientMessages i18n) {
        super(i18n.DigitalObjectFormValidateAction_Title(),
                "[SKIN]/actions/configure.png",
                i18n.DigitalObjectFormValidateAction_Hint());
        this.i18n = i18n;
        this.validator = new ModsCustomEditor(i18n);
        this.progress = new ProgressTracker(i18n);
    }

    @Override
    public void performAction(ActionEvent event) {
        validate(getValidable(event));
    }

    public void validate(final Validatable validable) {
        if (validable != null) {
            Record[] selection = validable.getSelection();
            if (selection != null && selection.length > 0) {
                validate(validable, selection);
            }
        }

    }

    public void closeWindow() {
        progress.stop();
    }

    private  void validate(final Validatable validable, final Record[] digitalObjects) {
        if (digitalObjects != null && digitalObjects.length > 0) {
            // ensure models are fetched
            MetaModelDataSource.getModels(false, new Callback<ResultSet, Void>() {

                @Override
                public void onFailure(Void reason) {
                }

                @Override
                public void onSuccess(ResultSet result) {
                    new ValidateTask(validable, digitalObjects).execute();
                }
            });
        } else {
            // no-op
        }
    }

    private static Validatable getValidable(ActionEvent event) {
        Object source = event.getSource();
        if (source instanceof Validatable) {
            return (Validatable) source;
        }
        return null;
    }

    private final class ValidateTask implements ScheduledCommand {

        private int index = 0;
        private int length = -1;
        private int invalidItemsCount;
        private final Record[] digitalObjects;
        private boolean stop = false;
        private final Validatable validatable;

        public ValidateTask(Validatable validatable, Record[] digitalObjects) {
            this.digitalObjects = digitalObjects;
            this.validatable = validatable;
        }

        @Override
        public void execute() {
            if (length < 0) {
                initTask();
            }
            if (!stop && index < length) {
                validateMods();
            } else {
                closeTask();
            }
        }

        private void initTask() {
            validatable.init();
            length = digitalObjects.length;
            invalidItemsCount = 0;
            progress.setInit();
            progress.showInWindow(new Runnable() {

                @Override
                public void run() {
                    stop = true;
                }
            }, i18n.DigitalObjectFormValidateAction_ProgressWindow_Title());
        }

        private void closeTask() {
            if (invalidItemsCount == 0) {
                progress.setDone(i18n.DigitalObjectFormValidateAction_NoError_Msg());
            } else {
                progress.setDone(i18n.DigitalObjectFormValidateAction_Errors_Msg(
                        String.valueOf(invalidItemsCount)));
            }
            validatable.onFinish(index != length);
        }

        private void validateMods() {
            progress.setProgress(index, length);
            final Record record = digitalObjects[index];
            DigitalObject dobj = DigitalObject.create(record);
            validator.setShowFetchPrompt(false);
            validator.edit(dobj, new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    if (value != null && value) {
                        consumeValidation(record, validator.isValidDigitalObject());
                    } else {
                        // unknown validity
                    }
                    ++index;
                    // Scheduler is not necessary as fetch operations
                    // run asynchronously
                    ValidateTask.this.execute();
                }
            });
        }

        private void consumeValidation(Record r, boolean valid) {
            if (valid) {
                validatable.clearErrors(r);
            } else {
                ++invalidItemsCount;
                validatable.setErrors(r, i18n.DigitalObjectFormValidateAction_ListRowError_Hint());
            }
        }

    }

    /**
     * Describes selected digital objects that should be validated.
     * <p>Use it as an action source.
     */
    public interface Validatable extends Selectable<Record> {

        void clearErrors(Record r);
        /**
         * Called before validation start.
         */
        void init();
        void onFinish(boolean canceled);
        void setErrors(Record r, String errors);

    }

    /**
     * Implements {@link Validatable} for the {@link ListGrid}.
     * <p>If the ListGrid does not return a multi selection then all records
     * are used.
     * <p>The list should be fully fetched before validation!
     */
    public static class ValidatableList implements Validatable {

        private final ListGrid list;
        private String fieldName;

        /**
         * Clears all row errors.
         * @param lg list to clear
         */
        public static void clearRowErrors(ListGrid lg) {
            for (int i = lg.getRecords().length - 1; i >= 0; i--) {
                lg.clearRowErrors(i);
            }
        }

        public ValidatableList(ListGrid list) {
            if (list == null) {
                throw new NullPointerException("list");
            }
            this.list = list;
        }

        @Override
        public void clearErrors(Record r) {
            int rowNum = list.getRecordIndex(r);
            list.clearRowErrors(rowNum);
        }

        @Override
        public void init() {
            // use the first visible column to set errors
            fieldName = list.getFieldName(0);
            // force grid to show errors
            list.setCanEdit(true);
        }

        @Override
        public void setErrors(Record r, String errors) {
            int rowNum = list.getRecordIndex(r);
            list.setFieldError(rowNum, fieldName, errors);
        }

        @Override
        public Record[] getSelection() {
            ListGridRecord[] records = list.getSelectedRecords();
            if (records == null || records.length == 0 || records.length == 1) {
                // if it is not a multi selection then use all records of the list
                ResultSet resultSet = list.getResultSet();
                if (resultSet.allMatchingRowsCached()) {
                    records = list.getRecords();
                }
            }
            return records;
        }

        @Override
        public void onFinish(boolean canceled) {
            // no-op
        }

    }

}
