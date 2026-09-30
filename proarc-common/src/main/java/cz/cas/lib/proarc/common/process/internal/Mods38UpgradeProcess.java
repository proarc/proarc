/*
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.process.internal;

import com.yourmediashelf.fedora.foxml.DigitalObject;
import cz.cas.lib.proarc.common.mods.Mods38Converter;
import cz.cas.lib.proarc.common.mods.Mods38Converter.Status;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.stream.StreamSource;

/** Upgrades MODS metadata discovered by walking the physical Akubra object store. */
public final class Mods38UpgradeProcess {

    private static final Logger LOG = Logger.getLogger(Mods38UpgradeProcess.class.getName());
    private static final String UNKNOWN_MODEL = "<unknown>";

    private final AkubraConfiguration configuration;
    private final AkubraStorage storage;

    public Mods38UpgradeProcess(AkubraConfiguration configuration) throws IOException {
        this.configuration = configuration;
        this.storage = AkubraStorage.getInstance(configuration);
    }

    public Result upgrade() throws IOException {
        Path objectStore = new File(configuration.getObjectStorePath()).toPath().toAbsolutePath().normalize();
        if (!Files.isDirectory(objectStore) || !Files.isReadable(objectStore)) {
            throw new IOException("Object store is not a readable directory: " + objectStore);
        }

        Result result = new Result();
        Files.walkFileTree(objectStore, new FileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                processFile(file, result);
                return Thread.currentThread().isInterrupted()
                        ? FileVisitResult.TERMINATE : FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFileFailed(Path file, IOException ex) {
                result.addFailure(file, UNKNOWN_MODEL, reason(ex));
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException ex) {
                if (ex != null) {
                    result.addFailure(dir, UNKNOWN_MODEL, reason(ex));
                }
                return FileVisitResult.CONTINUE;
            }
        });
        LOG.info(result.toLog());
        return result;
    }

    private void processFile(Path file, Result result) {
        String model = UNKNOWN_MODEL;
        try {
            if (!Files.isRegularFile(file) || !Files.isReadable(file)) {
                throw new IOException("File is not readable.");
            }
            DigitalObject digitalObject = FoxmlUtils.unmarshal(new StreamSource(file.toFile()), DigitalObject.class);
            if (digitalObject == null || digitalObject.getPID() == null) {
                throw new IOException("File does not contain a Fedora digital object with PID.");
            }
            LocalObject localObject = new LocalStorage().create(file.toFile(), digitalObject);
            model = new RelationEditor(localObject).getModel();
            if (!Mods38Converter.isSupportedModel(model)) {
                result.addUnsupportedModel(model);
                return;
            }

            Status status = Mods38Converter.upgradeObject(
                    storage.find(digitalObject.getPID()), model, "Upgrade MODS metadata to 3.8");
            if (status == Status.CONVERTED) {
                result.converted++;
            } else if (status == Status.ALREADY_CONVERTED) {
                result.alreadyConverted++;
            }
        } catch (Exception ex) {
            result.addFailure(file, model, reason(ex));
        }
    }

    private static String reason(Throwable throwable) {
        Throwable current = throwable;
        String reason = null;
        while (current != null) {
            if (current.getMessage() != null && !current.getMessage().trim().isEmpty()) {
                reason = current.getMessage();
            }
            current = current.getCause();
        }
        return reason == null ? throwable.getClass().getSimpleName() : reason;
    }

    public static final class Result {

        private int converted;
        private int alreadyConverted;
        private int unsupportedModels;
        private final Map<String, Integer> unsupportedModelCounts = new TreeMap<>();
        private final List<String> failures = new ArrayList<>();

        void addUnsupportedModel(String model) {
            unsupportedModels++;
            unsupportedModelCounts.merge(model == null ? UNKNOWN_MODEL : model, 1, Integer::sum);
        }

        private void addFailure(Path path, String model, String reason) {
            String entry = "path=" + path.toAbsolutePath().normalize()
                    + ", model=" + model + ", reason=" + reason;
            failures.add(entry);
            LOG.log(Level.WARNING, entry);
        }

        public String toLog() {
            StringBuilder log = new StringBuilder()
                    .append("MODS 3.8 upgrade finished:").append(System.lineSeparator())
                    .append(" - converted=").append(converted).append(System.lineSeparator())
                    .append(" - alreadyConverted=").append(alreadyConverted).append(System.lineSeparator())
                    .append(" - unsupportedModels=").append(unsupportedModels).append(System.lineSeparator())
                    .append(" - failed=").append(failures.size()).append(System.lineSeparator());
            if (unsupportedModels > 0) {
                log.append(System.lineSeparator());
                log.append("Unsupported models:");
                for (Map.Entry<String, Integer> entry : unsupportedModelCounts.entrySet()) {
                    log.append(System.lineSeparator()).append(" - ").append(entry.getKey())
                            .append(" ").append(entry.getValue());
                }
            }
            if (failures.size() > 0) {
                log.append(System.lineSeparator());
                log.append(System.lineSeparator());
                log.append("Errors:");
                for (String failure : failures) {
                    log.append(System.lineSeparator()).append(failure);
                }
            }
            return log.toString();
        }
    }
}
