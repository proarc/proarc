/*
 * Copyright (C) 2026 Lukas Sykora
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
package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import java.util.LinkedHashSet;
import java.util.Collection;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/** Metadata captured when an event happens, independent of later object changes. */
public record SolrLogMetadata(String model, Set<String> devices, Set<String> siglas, Set<String> software,
        Set<String> dois, Set<String> urnNbns, String label) {
    public static final SolrLogMetadata EMPTY = new SolrLogMetadata(null, Set.of(), Set.of());

    public SolrLogMetadata(String model, Set<String> devices, Set<String> siglas) {
        this(model, devices, siglas, Set.of());
    }

    public SolrLogMetadata(String model, Set<String> devices, Set<String> siglas, Set<String> software) {
        this(model, devices, siglas, software, Set.of());
    }

    public SolrLogMetadata(String model, Set<String> devices, Set<String> siglas, Set<String> software, Set<String> dois) {
        this(model, devices, siglas, software, dois, Set.of(), null);
    }

    public SolrLogMetadata {
        devices = Set.copyOf(devices);
        siglas = Set.copyOf(siglas);
        software = Set.copyOf(software);
        dois = Set.copyOf(dois);
        urnNbns = Set.copyOf(urnNbns);
    }

    public static SolrLogMetadata from(ProArcObject object) throws DigitalObjectException {
        RelationEditor relations = new RelationEditor(object);
        String model = relations.getModel();
        String device = relations.getDevice();
        String software = relations.getSoftware();
        Set<String> siglas = new LinkedHashSet<>();
        Set<String> dois = new LinkedHashSet<>();
        Set<String> urnNbns = new LinkedHashSet<>();
        String title = null;
        if (object.getStreamProfile(null).stream().anyMatch(profile -> ModsStreamEditor.DATASTREAM_ID.equals(profile.getDsID()))) {
            ModsDefinition mods = new ModsStreamEditor(object).read();
            if (!SolrUtils.isPage(model) && !isIssue(model)) title = ModsUtils.getLabel(mods, model);
            mods.getIdentifier().stream().filter(id -> id.getInvalid() == null)
                    .filter(id -> "urnnbn".equalsIgnoreCase(id.getType())
                    || "urn:nbn".equalsIgnoreCase(id.getType()))
                    .map(id -> id.getValue()).filter(value -> value != null && !value.isBlank())
                    .map(String::trim).forEach(urnNbns::add);
            // Issue labels need the date as well as the issue number.
            if (isIssue(model)) {
                List<String> parts = new ArrayList<>();
                mods.getPart().stream().flatMap(part -> part.getDetail().stream())
                        .filter(detail -> "issue".equals(detail.getType()))
                        .map(detail -> !detail.getCaption().isEmpty() ? detail.getCaption() : detail.getNumber())
                        .flatMap(Collection::stream).map(number -> number.getValue())
                        .filter(value -> value != null && !value.isBlank()).map(String::trim).distinct().forEach(parts::add);
                mods.getPart().stream().flatMap(part -> part.getDate().stream())
                        .map(date -> date.getValue()).filter(value -> value != null && !value.isBlank())
                        .map(String::trim).distinct().forEach(parts::add);
                title = String.join(", ", parts);
            }
            mods.getIdentifier().stream().filter(id -> id.getInvalid() == null)
                    .filter(id -> "doi".equalsIgnoreCase(id.getType()))
                    .map(id -> id.getValue()).filter(value -> value != null && !value.isBlank())
                    .map(String::trim).forEach(dois::add);
            for (LocationDefinition location : mods.getLocation()) {
                for (PhysicalLocationDefinition physicalLocation : location.getPhysicalLocation()) {
                    String authority = physicalLocation.getAuthority();
                    String value = physicalLocation.getValue();
                    if (value != null && !value.isBlank() && ("siglaADR".equalsIgnoreCase(authority)
                            || ((authority == null || authority.isBlank()) && value.trim().matches("[A-Z]{3}[0-9]{3}")))) {
                        siglas.add(physicalLocation.getValue().trim());
                    }
                }
            }
        }
        return new SolrLogMetadata(model, device == null || device.isBlank() ? Set.of() : Set.of(device), siglas,
                software == null || software.isBlank() ? Set.of() : Set.of(software), dois, urnNbns, title);
    }

    public static boolean isIssue(String model) {
        return Set.of("model:periodicalitem", "model:ndkperiodicalissue", "model:ndkeperiodicalissue",
                "model:oldprintperiodicalissue").contains(model == null ? "" : model);
    }
}
