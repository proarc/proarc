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
package cz.cas.lib.proarc.webapp.client.action;

import com.smartgwt.client.data.Record;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import java.util.ArrayList;

/**
 * Selects a range of digital objects and use them as a template.
 * The selection is provided by {@link #getSelection() }.
 * In order to display the selection an action source should implement
 * the {@link CopySelector}.
 *
 * <p>For now it supports {@code model:page}.
 *
 * @author Jan Pokorsky
 */
public class DigitalObjectCopyMetadataAction extends AbstractAction {

    public static String RECORD_COPY_ATTR = "copyCandidate";
    private static Record[] SELECTION;

    public static Record[] getSelection() {
        return SELECTION;
    }

    public static void resetSelection() {
        SELECTION = null;
    }

    public static void removeSelection(Record[] removePids) {
        if (SELECTION != null) {
            ArrayList<Record> newSelection = null;
            for (Record selectionRecord : SELECTION) {
                String selectionPid = selectionRecord.getAttribute(DigitalObjectDataSource.FIELD_PID);
                boolean remove = false;
                for (Record removeRecord : removePids) {
                    String removePid = removeRecord.getAttribute(DigitalObjectDataSource.FIELD_PID);
                    if (selectionPid.equals(removePid)) {
                        remove = true;
                        break;
                    }
                }
                if (!remove) {
                    if (newSelection == null) {
                        newSelection = new ArrayList<Record>(SELECTION.length);
                    }
                    newSelection.add(selectionRecord);
                }
            }
            if (newSelection != null) {
                SELECTION = newSelection.toArray(new Record[newSelection.size()]);
            }
        }
    }

    public DigitalObjectCopyMetadataAction(String title, String icon, String tooltip) {
        super(title, icon, tooltip);
    }

    public DigitalObjectCopyMetadataAction(ClientMessages i18n) {
        this(i18n.DigitalObjectCopyMetadataAction_Title(),
                "[SKIN]/RichTextEditor/copy.png",
                i18n.DigitalObjectCopyMetadataAction_Hint());
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        boolean accept = false;
        if (selection != null && selection instanceof Record[]) {
            Record[] records = (Record[]) selection;
            accept = accept(records);
        }
        return accept;
    }

    public boolean accept(Record[] records) {
        if (records == null || records.length == 0) {
            return false;
        }
        for (Record record : records) {
            DigitalObject dobj = DigitalObject.createOrNull(record);
            if (dobj != null) {
                String modelId = dobj.getModelId();
                if ("model:page".equals(modelId) || "model:ndkpage".equals(modelId)) {
                    continue;
                }
            }
            return false;
        }
        return true;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] records = Actions.getSelection(event);
        CopySelector selector = getCopySelector(event);
        SELECTION = records;
        if (selector != null) {
            selector.showCopySelection(records);
        }
    }

    public static CopySelector getCopySelector(ActionEvent event) {
        Object source = event.getSource();
        if (source instanceof CopySelector) {
            return (CopySelector) source;
        } else {
            return null;
        }
    }

    /**
     * Helper to track the copy status inside an digital object record.
     */
    public static boolean isSelectedCopyRecord(Record r) {
        return "true".equals(r.getAttribute(RECORD_COPY_ATTR));
    }

    /**
     * Helper to track the copy status inside an digital object record.
     */
    public static void deselectCopyRecord(Record r) {
        if (isSelectedCopyRecord(r)) {
            r.setAttribute(RECORD_COPY_ATTR, (String) null);
        }
    }

    /**
     * Helper to track the copy status inside an digital object record.
     */
    public static void selectCopyRecord(Record r) {
        r.setAttribute(RECORD_COPY_ATTR, "true");
    }

    public interface CopySelector {

        void showCopySelection(Record[] records);

    }
}
