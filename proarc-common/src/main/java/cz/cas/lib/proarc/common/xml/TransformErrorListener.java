/*
 * Copyright (C) 2016 Jan Pokorsky
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
package cz.cas.lib.proarc.common.xml;

import java.util.ArrayList;
import java.util.List;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.TransformerException;

/**
 * Collects transformation errors. Reset the handler before each transformation.
 *
 * @author Jan Pokorsky
 */
public class TransformErrorListener implements ErrorListener {

    private final List<String> errors;

    public TransformErrorListener() {
        this.errors = new ArrayList<String>();
    }

    public void reset() {
        errors.clear();
    }

    public List<String> getErrors() {
        return errors;
    }

    @Override
    public void warning(TransformerException exception) throws TransformerException {
        errors.add(exception.getMessageAndLocation());
    }

    @Override
    public void error(TransformerException exception) throws TransformerException {
        errors.add(exception.getMessageAndLocation());
    }

    @Override
    public void fatalError(TransformerException exception) throws TransformerException {
        throw exception;
    }

}
