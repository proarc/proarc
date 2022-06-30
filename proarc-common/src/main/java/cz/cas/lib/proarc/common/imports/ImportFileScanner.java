/*
 * Copyright (C) 2011 Jan Pokorsky
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
package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.imports.FileSet.FileEntry;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.text.Collator;
import java.text.ParseException;
import java.text.RuleBasedCollator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TreeMap;

/**
 *
 * @author Jan Pokorsky
 */
public final class ImportFileScanner {
    
    public enum State {
        IMPORTED, NEW, EMPTY, NOT_SET;
    }

    public static final String IMPORT_STATE_FILENAME = "proarc_import_status.log";
    /** system filenames to exclude from digital content list */
    private static final Set<String> EXCLUDE_FILENAMES = new HashSet<String>(Arrays.asList(
            IMPORT_STATE_FILENAME,
            ImportProcess.TMP_DIR_NAME
            ));

    private static final FileFilter FOLDER_FILTER = new FileFilter() {
        @Override
        public boolean accept(File f) {
            return f.isDirectory() && f.canRead() && f.canWrite() && !ImportProcess.TMP_DIR_NAME.equals(f.getName());
        }
    };

    private static RuleBasedCollator createCzechCollator() {
        RuleBasedCollator czechDefault = (RuleBasedCollator) Collator.getInstance(new Locale("cs"));
        try {
            return new RuleBasedCollator(
                    // Space before 0 results to "on", "on board", "online"
                    //   instead of "on", "online", "on board"
                    // '&' to reset definition does not work for space
                    "'\u0020' < 0"
                    + czechDefault.getRules());
        } catch (ParseException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * File name comparator. It delegates to extended Czech collator implementation.
     * @see <a href='http://www.docjar.com/html/api/sun/text/resources/CollationData_cs.java.html'>CollationData_cs.java</a>
     * @see java.text.CollationRules
     */
    private static final Comparator<File> FILE_COMPARATOR = new Comparator<File>() {
        
        private final Comparator<Object> czech = createCzechCollator();

        @Override
        public int compare(File o1, File o2) {
            return czech.compare(o1.getName(), o2.getName());
        }
    };

    private static final Comparator<FileSet> FILESET_COMPARATOR = new Comparator<FileSet>() {

        private final Comparator<Object> czech = createCzechCollator();

        @Override
        public int compare(FileSet o1, FileSet o2) {
            return czech.compare(o1.getName(), o2.getName());
        }
    };

    /**
     * Finds subfolders.
     *
     * @param folder folder to scan
     * @return list of direct subfolders
     */
    public List<Folder> findSubfolders(File folder, ImportHandler importer) throws FileNotFoundException, IllegalArgumentException {
        validateImportFolder(folder);

        File[] listFiles = folder.listFiles(FOLDER_FILTER);
        Arrays.sort(listFiles, FILE_COMPARATOR);
        List<Folder> content = new ArrayList<Folder>(listFiles.length);
        for (File file : listFiles) {
            content.add(new Folder(file, importer));
        }
        return content;
    }

    public List<File> findDigitalContent(File folder) throws IllegalArgumentException, FileNotFoundException {
        validateImportFolder(folder);

        File[] files = folder.listFiles();
        List<File> contents = new ArrayList<File>(files.length);
        for (File file : files) {
            if (file.isFile()&& file.canRead() && !EXCLUDE_FILENAMES.contains(file.getName())) {
                contents.add(file);
            }
        }
        Collections.sort(contents, FILE_COMPARATOR);
        return contents;
    }

    public static List<FileSet> getFileSets(List<File> files) {
        TreeMap<String, FileSet> items = new TreeMap<String, FileSet>(createCzechCollator());
        for (File file : files) {
            String filename = getName(file);
            filename = repairFilename(filename);
            FileSet itemFiles = items.get(filename);
            if (itemFiles == null) {
                itemFiles = new FileSet(filename);
                items.put(filename, itemFiles);
            }
            itemFiles.getFiles().add(new FileEntry(file));
        }
        return new ArrayList<FileSet>(items.values());
    }

    private static String repairFilename(String filename) {
        if (filename.startsWith("mca_") || filename.startsWith("MCA_") ||
                filename.startsWith("uca_") || filename.startsWith("UCA_")) {
            return  "SA_" + filename.substring(4);
        } else if (filename.startsWith("sa_") || filename.startsWith("SA_")) {
            return "SA_" + filename.substring(3);
        }
        return filename;
    }

    public static String getName(File f) {
        String fname = f.getName();
        int index = fname.indexOf('.');
        return index > 0 ? fname.substring(0, index) : fname;
    }

    static void validateImportFolder(File folder) throws FileNotFoundException, IllegalArgumentException {
        if (!folder.exists()) {
            throw new FileNotFoundException(folder.toString());
        }
        if (!folder.isDirectory()) {
            throw new IllegalArgumentException("FILE_IS_NOT_DIRECTORY");
        }
        if (!(folder.canRead() && folder.canWrite())) {
            throw new IllegalArgumentException("FILE_INSUFFICIENT_ACCESS_PERMISSIONS");
        }
    }

    static State folderImportState(File folder, ImportHandler importer) {
        if (isImported(folder)) {
            return State.IMPORTED;
        }
        if (importer == null) {
            return State.NOT_SET;
        }
        return importer.isImportable(folder) ? State.NEW : State.EMPTY;
    }

    static boolean isImported(File folder) {
        File stateFile = new File(folder, IMPORT_STATE_FILENAME);
        return stateFile.exists();
    }

    static void rollback(File folder) {
        File stateFile = new File(folder, IMPORT_STATE_FILENAME);
        stateFile.delete();
    }

    public static final class Folder {
        private File handle;
        private State status;
        private State statusDefault;
        private State statusArchive;
        private State statusKrameriusK4;
        private State statusKrameriusNdkMonograph;
        private State statusKrameriusNdkPeriodical;
        private State statusKrameriusStt;
        private State statusSoundrecording;
        private transient ImportHandler importer;


        private Folder(File handle, ImportHandler importer) {
            this.handle = handle;
            this.importer = importer;
        }

        public File getHandle() {
            return handle;
        }

        public State getStatus() {
            if (status == null) {
                status = folderImportState(handle, importer);
            }
            return status;
        }

        public State getStatusDefault(AppConfiguration appConfig) {
            if (statusDefault == null) {
                statusDefault = folderImportState(handle, createImporter(appConfig, ConfigurationProfile.DEFAULT));
            }
            return statusDefault;
        }

        public State getStatusArchive(AppConfiguration appConfig) {
            if (statusArchive == null) {
                statusArchive = folderImportState(handle, createImporter(appConfig, ConfigurationProfile.DEFAULT_ARCHIVE_IMPORT));
            }
            return statusArchive;
        }

        public State getStatusKrameriusK4(AppConfiguration appConfig) {
            if (statusKrameriusK4 == null) {
                statusKrameriusK4 = folderImportState(handle, createImporter(appConfig, ConfigurationProfile.DEFAULT_KRAMERIUS_IMPORT));
            }
            return statusKrameriusK4;
        }

        public State getStatusKrameriusNdkMonograph(AppConfiguration appConfig) {
            if (statusKrameriusNdkMonograph == null) {
                statusKrameriusNdkMonograph = folderImportState(handle, createImporter(appConfig, ConfigurationProfile.NDK_MONOGRAPH_KRAMERIUS_IMPORT));
            }
            return statusKrameriusNdkMonograph;
        }

        public State getStatusKrameriusNdkPeriodical(AppConfiguration appConfig) {
            if (statusKrameriusNdkPeriodical == null) {
                statusKrameriusNdkPeriodical = folderImportState(handle, createImporter(appConfig, ConfigurationProfile.NDK_PERIODICAL_KRAMERIUS_IMPORT));
            }
            return statusKrameriusNdkPeriodical;
        }

        public State getStatusKrameriusStt(AppConfiguration appConfig) {
            if (statusKrameriusStt == null) {
                statusKrameriusStt = folderImportState(handle, createImporter(appConfig, ConfigurationProfile.STT_KRAMERIUS_IMPORT));
            }
            return statusKrameriusStt;
        }

        public State getStatusSoundrecording(AppConfiguration appConfig) {
            if (statusSoundrecording == null) {
                statusSoundrecording = folderImportState(handle, createImporter(appConfig, ConfigurationProfile.DEFAULT_SOUNDRECORDING_IMPORT));
            }
            return statusSoundrecording;
        }

        private ImportHandler createImporter(AppConfiguration appConfig, String profileId) {
            ConfigurationProfile profile = appConfig.getProfiles().getProfile(ImportProfile.PROFILES, profileId);
            if (profile == null) {
                return null;
            }
            ImportProfile importProfile = appConfig.getImportConfiguration(profile);
            return importProfile.createImporter();
        }
    }

}
