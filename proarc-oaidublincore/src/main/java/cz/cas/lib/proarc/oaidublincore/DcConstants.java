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
package cz.cas.lib.proarc.oaidublincore;

/**
 * Constants of OAI DC schema.
 *
 * @author Jan Pokorsky
 */
public final class DcConstants {

    /** Namespace http://purl.org/dc/elements/1.1/. */
    public static final String NS_PURL = "http://purl.org/dc/elements/1.1/";
    /** Namespace http://www.openarchives.org/OAI/2.0/oai_dc/. */
    public static final String NS_OAIDC = "http://www.openarchives.org/OAI/2.0/oai_dc/";

    public static final String PREFIX_NS_OAIDC = "oai_dc";
    public static final String PREFIX_NS_PURL = "dc";

    public static final String DC = "dc";
    public static final String CONTRIBUTOR = "contributor";
    public static final String COVERAGE = "coverage";
    public static final String CREATOR = "creator";
    public static final String DATE = "date";
    public static final String DESCRIPTION = "description";
    public static final String FORMAT = "format";
    public static final String IDENTIFIER = "identifier";
    public static final String LANGUAGE = "language";
    public static final String PUBLISHER = "publisher";
    public static final String RELATION = "relation";
    public static final String RIGHTS = "rights";
    public static final String SOURCE = "source";
    public static final String SUBJECT = "subject";
    public static final String TITLE = "title";
    public static final String TYPE = "type";
    /** lang attribute of each element */
    public static final String LANG = "lang";
    /** The {@link ElementType } value name for JSON mapping. */
    public static final String VALUE = "value";

}
