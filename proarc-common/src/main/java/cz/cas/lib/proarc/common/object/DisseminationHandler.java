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
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;

/**
 * Read and writes contents in its raw form.
 */
public interface DisseminationHandler {

    /**
     * Gets contents as it is persisted.
     * @param httpRequest {@code null} or request to manage cached contents.
     * @return contents
     * @throws DigitalObjectException failure
     */
    Response getDissemination(Request httpRequest) throws DigitalObjectException;

    /**
     * Updates contents.
     * @param input contents
     * @param message update message
     * @throws DigitalObjectException failure
     */
    void setDissemination(DisseminationInput input, String message) throws DigitalObjectException;

    /**
     * Removes contents.
     * @param message delete message
     * @throws DigitalObjectException failure
     */
    void deleteDissemination(String message) throws DigitalObjectException;

}
