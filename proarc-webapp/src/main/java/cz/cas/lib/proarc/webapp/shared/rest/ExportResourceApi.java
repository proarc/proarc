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
 * Constants for {@link cz.cas.lib.proarc.webapp.server.rest.ExportResource}
 * shared by GWT client.
 *
 * @author Jan Pokorsky
 */
public final class ExportResourceApi {

    public static final String PATH = "export";

    public static final String ARCHIVE_PATH = "archive";
    public static final String ARCHIVE_PID_PARAM = "pid";

    public static final String CEJSH_PATH = "cejsh";
    public static final String CEJSH_PID_PARAM = "pid";

    public static final String CROSSREF_PATH = "crossref";
    public static final String CROSSREF_PID_PARAM = "pid";

    public static final String KRAMERIUS4_PATH = "kramerius4";
    public static final String KRAMERIUS4_PID_PARAM = "pid";
    public static final String KRAMERIUS4_HIERARCHY_PARAM = "hierarchy";

    public static final String KWIS_PATH = "kwis";
    public static final String KWIS_PID_PARAM = "pid";
    public static final String KWIS_HIERARCHY_PARAM = "hierarchy";

    public static final String DATASTREAM_PATH = "datastream";
    public static final String DATASTREAM_PID_PARAM = "pid";
    public static final String DATASTREAM_DSID_PARAM = "dsid";
    public static final String DATASTREAM_HIERARCHY_PARAM = "hierarchy";

    public static final String DESA_PATH = "desa";
    public static final String DESA_DRYRUN_PARAM = "dryRun";
    public static final String DESA_FORDOWNLOAD_PARAM = "forDownload";
    public static final String DESA_PID_PARAM = "pid";
    public static final String DESA_HIERARCHY_PARAM = "hierarchy";

    public static final String NDK_PATH = "ndk";
    public static final String NDK_PID_PARAM = "pid";
    
    public static final String RESULT_TARGET = "target";
    public static final String RESULT_ID = "exportId";
    public static final String RESULT_TOKEN = "token";
    public static final String RESULT_ERRORS = "errors";

    public static final String RESULT_ERROR = "error";
    public static final String RESULT_ERROR_PID = "pid";
    public static final String RESULT_ERROR_MESSAGE = "message";
    public static final String RESULT_ERROR_WARNING = "iswarning";
    public static final String RESULT_ERROR_LOG = "log";

}
