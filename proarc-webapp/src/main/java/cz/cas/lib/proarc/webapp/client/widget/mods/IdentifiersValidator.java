/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.widget.mods;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.mods.IdentifierDataSource;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.logging.Logger;

/**
 * Validates list of identifier form items. It is possible to check
 * list of required identifier types.
 *
 * @author Jan Pokorsky
 */
final class IdentifiersValidator extends RepeatableItemValidator {

    private static final Logger LOG = Logger.getLogger(IdentifiersValidator.class.getName());
    private Set<String> requiredIdentifiers;

    /**
     * Constructs new instance.
     * 
     * @param i18n i18n
     * @param requiredIdentifiers required identifier types
     */
    public IdentifiersValidator(ClientMessages i18n, Collection<String> requiredIdentifiers) {
        this.requiredIdentifiers = (requiredIdentifiers == null)
                ? new HashSet<String>() : new HashSet<String>(requiredIdentifiers);
        this.requiredIdentifiers.add(IdentifierDataSource.TYPE_UUID);
    }

    @Override
    protected boolean condition(RecordList recordList) {
        boolean valid = true;
        StringBuilder typesMsg = new StringBuilder();
        LinkedHashMap<String, String> typeMap = IdentifierDataSource.getTypeValueMap(getI18n());
        for (String type : requiredIdentifiers) {
            Record r = recordList.find(IdentifierDataSource.FIELD_TYPE, type);
            if (r == null) {
                valid = false;
                String typeName = typeMap.get(type);
                typesMsg.append(", ").append(typeName == null ? type : typeName);
            }
        }
        if (typesMsg.length() > 0) {
            setErrorMessage(getI18n().Validation_RequiredIdentifiers_Msg(typesMsg.substring(2)));
        }
        return valid;
    }


}
