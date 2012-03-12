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
package cz.incad.pas.editor.server.rest;

import com.sun.jersey.api.json.JSONConfiguration;
import com.sun.jersey.api.json.JSONJAXBContext;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportBatch;
import cz.incad.pas.editor.server.rest.ImportResource.ImportBatchItemList;
import cz.incad.pas.editor.server.rest.ImportResource.ImportBatchList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.ext.ContextResolver;
import javax.ws.rs.ext.Provider;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

/**
 * {@link JSONConfiguration#natural() natural builder} does not create JSON arrays either from Collections or arrays.
 *
 * @author Jan Pokorsky
 */
@Provider
@Produces(MediaType.APPLICATION_JSON)
public class JSonContextResolver implements ContextResolver<JAXBContext> {

    private static final Class[] types = {
            SmartGwtResponse.class,
            ImportFolder.class,
            ImportBatchList.class,
            ImportBatch.class,
            ImportBatchItemList.class,
    };
    private static final Set<Class> setOfTypes = Collections.unmodifiableSet(new HashSet<Class>(Arrays.asList(types)));

    private final JSONJAXBContext context;

    public JSonContextResolver() throws JAXBException {
//        context = new JSONJAXBContext(JSONConfiguration.natural().build(), SmartGwtResponse.class, ImportFolder.class);
//        context = new JSONJAXBContext(JSONConfiguration.mappedJettison().build(), SmartGwtResponse.class, ImportFolder.class);
        // rootUnwrapping solves JSON marshaling issue when empty object results to "null" JSON string
        // with rootUnwrapping the result is e.g. {"batches":null} for ImportBatchList with empty list
        // see http://java.net/jira/browse/JERSEY-339 and
        context = new JSONJAXBContext(JSONConfiguration.mapped().rootUnwrapping(false).arrays("folder").build(), types);
    }


    @Override
    public JAXBContext getContext(Class<?> type) {
//        System.out.println("## getContext: " + type);
        return setOfTypes.contains(type) ? context : null;
    }

}
