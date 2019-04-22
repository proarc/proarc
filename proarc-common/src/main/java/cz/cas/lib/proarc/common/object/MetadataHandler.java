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
package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.fedora.DigitalObjectException;

/**
 * Handles digital object metadata.
 *
 * @param <T> type of metadata
 * @author Jan Pokorsky
 */
public interface MetadataHandler<T> {

    public static final String DESCRIPTION_DATASTREAM_ID = "BIBLIO_MODS";
    public static final String DESCRIPTION_DATASTREAM_LABEL = "Metadata Object Description";

    /**
     * Sets new description metadata.
     * <p>Return {@code null} from {@link DescriptionMetadata#getData() }
     * to create default metadata instance.
     *
     * @param data metadata holder
     * @param message log message
     * @throws DigitalObjectException failure
     */
    void setMetadata(DescriptionMetadata<T> data, String message, String typeRecord) throws DigitalObjectException;

    /**
     * Sets new description metadata as JSON text.
     * <p>Return {@code null} from {@link DescriptionMetadata#getData() }
     * to create default metadata instance.
     *
     * @param jsonData metadata holder
     * @param message log message
     * @throws DigitalObjectException failure
     */
    void setMetadataAsJson(DescriptionMetadata<String> jsonData, String message, String typeRecord) throws DigitalObjectException;

    /**
     * Sets new description metadata as XML text.
     * <p>Return {@code null} from {@link DescriptionMetadata#getData() }
     * to create default metadata instance.
     *
     * @param xmlData metadata holder
     * @param message log message
     * @throws DigitalObjectException failure
     */
    void setMetadataAsXml(DescriptionMetadata<String> xmlData, String message, String typeRecord) throws DigitalObjectException;

    DescriptionMetadata<T> getMetadata() throws DigitalObjectException;

    DescriptionMetadata<String> getMetadataAsXml() throws DigitalObjectException;

    /**
     * Gets description metadata as an object mappable to JSON.
     * @param <O> metadata type
     * @param mappingId mapping ID
     * @return tho object mappable to JSON
     * @throws DigitalObjectException failure
     */
    <O> DescriptionMetadata<O> getMetadataAsJsonObject(String mappingId) throws DigitalObjectException;

}
