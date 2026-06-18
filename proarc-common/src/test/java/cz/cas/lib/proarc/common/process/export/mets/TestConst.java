/*
 * Copyright (C) 2014 Robert Simonovsky
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

package cz.cas.lib.proarc.common.process.export.mets;

import java.util.HashMap;

/**
 * Constatnts for ndkExport test
 *
 * @author Robert Simonovsky
 *
 */
public class TestConst {
    public static HashMap<String, String> parents = new HashMap<String, String>();

    static {
        parents.put("uuid:b46ab0eb-26af-11e3-88e3-001b63bd97ba", "uuid:2ad73b97-ef9d-429a-b3a5-65083fa4c317");
        parents.put("uuid:b46aff0c-26af-11e3-88e3-001b63bd97ba", "uuid:2ad73b97-ef9d-429a-b3a5-65083fa4c317");
        parents.put("uuid:b46aff0d-26af-11e3-88e3-001b63bd97ba", "uuid:2ad73b97-ef9d-429a-b3a5-65083fa4c317");
        parents.put("uuid:b46aff0e-26af-11e3-88e3-001b63bd97ba", "uuid:2ad73b97-ef9d-429a-b3a5-65083fa4c317");
        parents.put("uuid:b46aff0f-26af-11e3-88e3-001b63bd97ba", "uuid:2ad73b97-ef9d-429a-b3a5-65083fa4c317");
        parents.put("uuid:b46b2620-26af-11e3-88e3-001b63bd97ba", "uuid:2ad73b97-ef9d-429a-b3a5-65083fa4c317");
        parents.put("uuid:b46b2621-26af-11e3-88e3-001b63bd97ba", "uuid:2ad73b97-ef9d-429a-b3a5-65083fa4c317");
        parents.put("uuid:b46b2622-26af-11e3-88e3-001b63bd97ba", "uuid:2ad73b97-ef9d-429a-b3a5-65083fa4c317");
        parents.put("uuid:2ad73b97-ef9d-429a-b3a5-65083fa4c317", "uuid:90ca85a1-beb8-4b5d-a320-b4b7ac5c8df5");
        parents.put("uuid:90ca85a1-beb8-4b5d-a320-b4b7ac5c8df5", "uuid:3733b6e3-61ab-42fc-a437-964d143acc45");
        parents.put("uuid:2ff2dd0c-d438-4d95-940f-690ee0f44a4a", "uuid:44589055-9fad-4a9f-b6a8-75be399f332d");
        parents.put("uuid:44589055-9fad-4a9f-b6a8-75be399f332d", "uuid:1ccbf6c5-b22c-4d89-b42e-8cd14101a737");
    }
}
