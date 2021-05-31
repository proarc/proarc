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
package cz.cas.lib.proarc.common.export;

import cz.cas.lib.proarc.common.export.desa.Const;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.workflow.WorkflowExportFile;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXB;

import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_EXPORTED;

/**
 *
 * @author Jan Pokorsky
 */
public final class ExportUtils {

    public static final String PROARC_EXPORT_STATUSLOG = "proarc_export_status.log";
    public static final String WORKFLOW_EXPORT_FILE = "workflow_information.xml";

    private static final Logger LOG = Logger.getLogger(ExportUtils.class.getName());

    /**
     * Creates new folder. If name already exists it finds similar free name.
     * @param parent target folder
     * @param name name of the new folder
     * @return the new folder
     */
    public static File createFolder(File parent, String name, boolean overwrite) {
        if (name == null || name.contains(":")) {
            throw new IllegalArgumentException(name);
        }
        if (parent == null) {
            throw new NullPointerException("parent");
        }
        File folder = new File(parent, name);
        if (overwrite) {
            if (!folder.mkdir()) {
                MetsUtils.deleteFolder(folder);
                folder.mkdir();
            }
        } else {
            for (int i = 1; !folder.mkdir(); i++) {
                folder = new File(parent, name + '_' + i);
            }
        }
        return folder;
    }

    /**
     * Creates XML file instance for given PID.
     * <p>It does not use special characters as ':' to avoid platform particularities.
     *
     * @param output target folder
     * @param pid PID of digital object
     * @return file
     */
    public static File pidAsXmlFile(File output, String pid) {
        String uuid = FoxmlUtils.pidAsUuid(pid);
        File foxml = new File(output, uuid + ".xml");
        return foxml;
    }

    /**
     * Writes an export result in XML.
     */
    public static void writeExportResult(File targetFolder, ExportResultLog result) {
        if (result.getEnd() == null) {
            result.setEnd(new Date());
        }
        if (result.getExports().size() == 1) {
            result.setBegin(null);
            result.setEnd(null);
        }
        File resultFile = new File(targetFolder, PROARC_EXPORT_STATUSLOG);
        try {
            JAXB.marshal(result, resultFile);
        } catch (Exception e) {
            LOG.log(Level.SEVERE, targetFolder.toString(), e);
        }
    }

    /**
     * Stores an export result to the digital object.
     * @param pid digital object ID
     * @param target export result (file/folder path, remote storage handle, ...)
     * @param log fedora log message
     * @throws DigitalObjectException failure
     */
    public static void storeObjectExportResult(String pid, String target, String type, String log) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fo = dom.find(pid, null);
        DigitalObjectHandler doh = dom.createHandler(fo);
        RelationEditor relations = doh.relations();
        switch(type) {
            case "NDK":
                relations.setNdkExportResult(target);
                relations.setExportResult(target);
                relations.setStatus(STATUS_EXPORTED);
                break;
            case "KRAMERIUS":
                relations.setKrameriusExportResult(target);
                relations.setExportResult(target);
                relations.setStatus(STATUS_EXPORTED);
                break;
            case "ARCHIVE":
                relations.setNdkExportResult(target);
                String archiveTarget = createArchiveTarget(target);
                relations.setArchiveExportResult(archiveTarget);
                relations.setExportResult(archiveTarget);
                relations.setStatus(STATUS_EXPORTED);
                break;
            case "CROSREFF":
                relations.setCrossrefExportResult(target);
                relations.setExportResult(target);
                relations.setStatus(STATUS_EXPORTED);
                break;
        }
        relations.write(relations.getLastModified(), log);
        doh.commit();
    }

    private static String createArchiveTarget(String path) {
        StringBuilder target = new StringBuilder();
        String[] targetFolder = path.split("/");
        for (int i = 0; i < targetFolder.length; i++) {
            if (targetFolder[i].contains("archive_")) {
                target.append(targetFolder[i]).append("/");
                return target.toString();
            } else {
                target.append(targetFolder[i]).append("/");
            }
        }
        return path;
    }

    public static String toString(Iterable<?> lines) {
        return toString(lines, "\n");
    }

    public static String toString(Iterable<?> lines, String lineEnd) {
        StringBuilder sb = new StringBuilder();
        for (Object line : lines) {
            if (sb.length() > 0) {
                sb.append(lineEnd);
            }
            sb.append(line);
        }
        return sb.toString();
    }

    public static String toString(Throwable ex) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        ex.printStackTrace(pw);
        pw.close();
        return sw.toString();
    }

    public static void writeWorkflowResult(File targetFolder, WorkflowExportFile wf) {
        File resultFile = new File(targetFolder, WORKFLOW_EXPORT_FILE);
        try {
            JAXB.marshal(wf, resultFile);
        } catch (Exception e) {
            LOG.log(Level.SEVERE, targetFolder.toString(), e);
        }
    }

    public static String getModel(String model) {
        if (model != null && model.startsWith(Const.FEDORAPREFIX)) {
            return model.replace(Const.FEDORAPREFIX, "");
        } else {
            return model;
        }
    }

    public static int getPageIndex(ModsDefinition mods) {
        if (mods.getPart().size() > 0) {
            for (PartDefinition part : mods.getPart()) {
                for (DetailDefinition detail : part.getDetail()) {
                    if ("pageIndex".equals(detail.getType()) && detail.getNumber().size() > 0) {
                        return Integer.valueOf(detail.getNumber().get(0).getValue());
                    }
                }
            }
        }
        return -1;
    }

    public static boolean containPageNumber(ModsDefinition mods) {
        if (mods.getPart().size() > 0) {
            for (DetailDefinition detail : mods.getPart().get(0).getDetail()) {
                if ("pageNumber".equals(detail.getType()) && detail.getNumber().size() > 0) {
                    return !detail.getNumber().get(0).getValue().isEmpty();
                }
                if ("page number".equals(detail.getType()) && detail.getNumber().size() > 0) {
                    return !detail.getNumber().get(0).getValue().isEmpty();
                }
            }
        }
        return false;
    }

    public static boolean containPageType(ModsDefinition mods) {
        if (mods.getPart().size() > 0) {
            if (mods.getPart().get(0).getType() != null) {
                return !mods.getPart().get(0).getType().isEmpty();
            }
        }
        return false;
    }
}
