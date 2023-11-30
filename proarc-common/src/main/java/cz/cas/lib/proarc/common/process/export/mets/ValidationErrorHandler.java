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

import java.util.ArrayList;
import java.util.List;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * Error handler for validation of XML documents
 *
 * @author Robert Simonovsky
 *
 */
public class ValidationErrorHandler implements ErrorHandler {
    private final List<String> validationErrors = new ArrayList<String>();

    /**
     * returns the list of errors found while validating an XML document
     *
     * @return
     */
    public List<String> getValidationErrors() {
        return validationErrors;
    }

    @Override
    public void warning(SAXParseException exception) throws SAXException {
    }

    @Override
    public void error(SAXParseException exception) throws SAXException {
        validationErrors.add("Column:" + exception.getColumnNumber() + " Line:" + exception.getLineNumber() + " " + exception.getMessage());
    }

    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        validationErrors.add("Column:" + exception.getColumnNumber() + " Line:" + exception.getLineNumber() + " " + exception.getMessage());
    }
}
