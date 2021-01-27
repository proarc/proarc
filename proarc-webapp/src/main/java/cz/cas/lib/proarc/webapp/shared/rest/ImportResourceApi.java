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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.shared.rest;

/**
 *
 * @author Jan Pokorsky
 */
public final class ImportResourceApi {

    // resource /import
    public static final String PATH = "import";

    // resource /import/folder
    public static final String FOLDER_PATH = "folder";
    public static final String IMPORT_FOLDER_PARENT_PARAM = "folder";

    // ImportFolder
    public static final String IMPORT_FOLDER_ELEMENT = "folder";
    public static final String IMPORT_FOLDER_STATE = "state";
    public static final String IMPORT_FOLDER_PATH = "path";

    // resource /import/batch
    public static final String BATCH_PATH = "batch";
    public static final String BATCHES_PATH = "batches";
    public static final String BATCHES_IN_PROCESS_PATH = "processingBatches";
    public static final String NEWBATCH_DEVICE_PARAM = "device";
    public static final String NEWBATCH_INDICES_PARAM = "indices";

    // ImportBatch
    public static final String IMPORT_BATCH_ELEMENT = "batch";
    public static final String IMPORT_BATCH_ID = "id";
    public static final String IMPORT_BATCH_FOLDER = "folderPath";
    public static final String IMPORT_BATCH_DESCRIPTION = "description";
    public static final String IMPORT_BATCH_PARENTPID = "parentPid";
    public static final String IMPORT_BATCH_CREATE = "create";
    public static final String IMPORT_BATCH_CREATE_FROM = "createFrom";
    public static final String IMPORT_BATCH_CREATE_TO = "createTo";
    public static final String IMPORT_BATCH_TIMESTAMP = "timestamp";
    public static final String IMPORT_BATCH_MODIFIED_FROM = "modifiedFrom";
    public static final String IMPORT_BATCH_MODIFIED_TO = "modifiedTo";
    public static final String IMPORT_BATCH_USERID = "userId";
    public static final String IMPORT_BATCH_USER = "user";
    public static final String IMPORT_BATCH_STATE = "state";
    public static final String IMPORT_BATCH_OPTIONS = "options";
    public static final String IMPORT_BATCH_ESTIMATEFILECOUNT = "estimateFileCount";
    public static final String IMPORT_BATCH_FAILURE = "failure";
    public static final String IMPORT_BATCH_ITEMS = "items";
    public static final String IMPORT_BATCH_PROFILE = "profile";
    public static final String IMPORT_BATCH_PAGECOUNT = "pageCount";
    public static final String IMPORT_START_ROW_PARAM = "_startRow";

    // resource /import/batch/item
    public static final String BATCHITEM_PATH = "item";

    // BatchItem
    public static final String BATCHITEM_BATCHID = "batchId";
    public static final String BATCHITEM_FILENAME = "filename";
    public static final String BATCHITEM_PID = "pid";
    public static final String BATCHITEM_MODEL = "model";
    public static final String BATCHITEM_PAGEINDEX = "pageIndex";
    public static final String BATCHITEM_PAGENUMBER = "pageNumber";
    public static final String BATCHITEM_PAGETYPE = "pageType";
    public static final String BATCHITEM_PAGETYPELABEL = "pageTypeLabel";
    public static final String BATCHITEM_TIMESTAMP = "timestamp";
    public static final String BATCHITEM_USER = "user";

}
