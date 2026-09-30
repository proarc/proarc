/*
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.mods;

import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.NamePartDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PublisherDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.StringReader;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Validator;

/** Converts the supported legacy MODS records to MODS 3.8. */
public final class Mods38Converter {

    private static final Set<String> SOURCE_VERSIONS = new HashSet<>(Arrays.asList("3.4", "3.6"));
    private static final Set<String> EVENT_TYPES = new HashSet<>(Arrays.asList(
            ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION,
            ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION,
            ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION,
            ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE,
            ModsConstants.VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT));

    private Mods38Converter() {
    }

    public enum Status {
        CONVERTED,
        ALREADY_CONVERTED,
        UNSUPPORTED_MODEL
    }

    public static boolean isSupportedModel(String model) {
        return model != null && (model.startsWith("model:ndk") || model.startsWith("model:oldprint") ||
                model.startsWith("model:chronicle") || model.startsWith("model:clippings") || model.startsWith("model:graphic") ||
                model.equals("model:page") || model.equals("model:bdmarticle"));
    }

    public static Status convertAndValidate(ModsDefinition mods, String model) throws ConversionException {
        if (!isSupportedModel(model)) {
            return Status.UNSUPPORTED_MODEL;
        }
        if (mods == null) {
            throw new ConversionException("Missing MODS datastream.");
        }
        String version = trimToNull(mods.getVersion());
        if (ModsUtils.VERSION.equals(version)) {
            validate(mods);
            return Status.ALREADY_CONVERTED;
        }
        if (!SOURCE_VERSIONS.contains(version)) {
            throw new ConversionException("Unsupported MODS version: " + String.valueOf(version));
        }

        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            String eventType = trimToNull(originInfo.getEventType());
            if (eventType != null && !EVENT_TYPES.contains(eventType)) {
                throw new ConversionException("Unsupported originInfo eventType: " + eventType);
            }

            for (PublisherDefinition publisher : originInfo.getPublisher()) {
                NameDefinition agent = new NameDefinition();

                NamePartDefinition namePart = new NamePartDefinition();
                namePart.setValue(publisher.getValue());

                agent.getNamePart().add(namePart);

                ModsUtils.setAgentRole(agent, eventType);

                originInfo.getAgent().add(agent);
            }
            originInfo.getPublisher().clear();
        }
        mods.setVersion(ModsUtils.VERSION);
        validate(mods);
        return Status.CONVERTED;
    }

    public static void validate(ModsDefinition mods) throws ConversionException {
        try {
            Validator validator = ModsUtils.getSchema().newValidator();
            validator.validate(new StreamSource(new StringReader(ModsUtils.toXml(mods, false))));
        } catch (Exception ex) {
            throw new ConversionException("MODS 3.8 validation failed: " + ex.getMessage(), ex);
        }
    }

    public static Status upgradeObject(ProArcObject object, String model, String message)
            throws DigitalObjectException, ConversionException {
        if (!isSupportedModel(model)) {
            return Status.UNSUPPORTED_MODEL;
        }

        ModsStreamEditor modsEditor = new ModsStreamEditor(object);
        ModsDefinition mods = modsEditor.read();
        Status status = convertAndValidate(mods, model);
        if (status != Status.CONVERTED) {
            return status;
        }

        DigitalObjectHandler handler = new DigitalObjectHandler(object, MetaModelRepository.getInstance());
        NdkMapper mapper = NdkMapper.get(model);
        if (mapper == null) {
            throw new ConversionException("No metadata mapper for model: " + model);
        }
        mapper.setModelId(model);
        OaiDcType dc = mapper.toDc(mods, new NdkMapper.Context(handler));

        modsEditor.write(mods, modsEditor.getLastModified(), message);
        DcStreamEditor dcEditor = handler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dcRecord = dcEditor.read();
        dcRecord.setDc(dc);
        dcEditor.write(handler, dcRecord, message);
        handler.commit();
        return status;
    }

    private static String trimToNull(String value) {
        if (value == null || value.trim().isEmpty()) {
            return null;
        }
        return value.trim();
    }

    public static final class ConversionException extends Exception {

        public ConversionException(String message) {
            super(message);
        }

        public ConversionException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}
