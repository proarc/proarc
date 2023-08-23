/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.urnnbn;

import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.NamePartDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.urnnbn.model.registration.OriginatorTypeType;
import cz.cas.lib.proarc.urnnbn.model.registration.PrimaryOriginator;
import cz.cas.lib.proarc.urnnbn.model.registration.Publication;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Jan Pokorsky
 */
public class ResolverUtils {

    static String getIdentifierValue(String type, ModsDefinition... mods) {
        for (ModsDefinition modsItem : mods) {
            String id = getIdentifierValue(type, modsItem);
            if (id != null) {
                return id;
            }
        }
        return null;
    }

    public static IdentifierDefinition getIdentifier(String type, ModsDefinition mods) {
        for (IdentifierDefinition identifierDefinition : mods.getIdentifier()) {
            if (type.equals(identifierDefinition.getType()) && isValid(identifierDefinition)) {
                return identifierDefinition;
            }
        }
        return null;
    }

    public static String getIdentifierValue(String type, ModsDefinition mods) {
        IdentifierDefinition identifier = getIdentifier(type, mods);
        return identifier == null ? null : identifier.getValue();
    }

    static String getTitlePartNumber(ModsDefinition mods) {
        for (TitleInfoDefinition ti : mods.getTitleInfo()) {
            for (StringPlusLanguage spl : ti.getPartNumber()) {
                String value = spl.getValue();
                if (value != null) {
                    return value;
                }
            }
        }
        return null;
    }

    static String getTitlePartName(ModsDefinition mods) {
        for (TitleInfoDefinition ti : mods.getTitleInfo()) {
            for (StringPlusLanguage spl : ti.getPartName()) {
                String value = spl.getValue();
                if (value != null) {
                    return value;
                }
            }
        }
        return null;
    }

    static String getTitle(ModsDefinition mods) {
        for (TitleInfoDefinition ti : mods.getTitleInfo()) {
            if (ti.getType() != null) {
                continue;
            }
            String value = getStringPlusLanguage(ti.getTitle());
            if (value != null) {
                return value;
            }
        }
        return null;
    }

    static TitleInfoDefinition getTitleInfo(ModsDefinition mods) {
        for (TitleInfoDefinition ti : mods.getTitleInfo()) {
            if (ti.getType() != null) {
                continue;
            }
            String value = getStringPlusLanguage(ti.getTitle());
            if (value != null) {
                return ti;
            }
        }
        return null;
    }

    static Publication getPublication(ModsDefinition... mods) {
        String dateIssued = null;
        String publisher = null;
        String place = null;
        for (ModsDefinition md : mods) {
            for (OriginInfoDefinition origin : md.getOriginInfo()) {
                if (dateIssued == null) {
                    dateIssued = getDate(origin.getDateIssued());
                }
                if (publisher == null) {
                    publisher = getStringPlusLanguage(origin.getPublisher());
                }
                if (place == null) {
                    place = getPlace(origin.getPlace());
                }
                if (dateIssued != null && publisher != null) {
                    break;
                }
            }
        }
        if (dateIssued != null || publisher != null || place != null) {
            Publication p = new Publication();
            p.setPlace(place);
            p.setPublisher(publisher);
            p.setYear(dateIssued);
            return p;
        } else {
            return null;
        }
    }

    static String getPlace(List<PlaceDefinition> places) {
        for (PlaceDefinition place : places) {
            for (PlaceTermDefinition placeTerm : place.getPlaceTerm()) {
                if (CodeOrText.TEXT == placeTerm.getType()) {
                    String value = placeTerm.getValue();
                    if (value != null) {
                        return value;
                    }
                }
            }
        }
        return null;
    }

    static String getStringPlusLanguage(List<? extends StringPlusLanguage> strings) {
        ArrayList<String> items = new ArrayList<String>();
        for (StringPlusLanguage string : strings) {
            String value = string.getValue();
            if (value != null) {
                items.add(value);
            }
        }
        return concat(items, ", ");
    }

    static String getDate(List<DateDefinition> dates) {
        for (DateDefinition date : dates) {
            if (date.getPoint() == null) {
                String value = date.getValue();
                if (value != null) {
                    return value;
                }
            }
        }
        return null;
    }

    static PrimaryOriginator getPrimaryOriginator(ModsDefinition... mods) {
        // mods:name[@type='personal' and @usage='primary']//mods:namePart[not(@type= 'date')]
        // mods:name[@type='corporate']
        // mods:name[@type='conference']
        PrimaryOriginator primaryOriginator = new PrimaryOriginator();
        for (ModsDefinition modsDefinition : mods) {
            String originator = getOriginator("personal", true, modsDefinition);
            primaryOriginator.setType(OriginatorTypeType.AUTHOR);
            if (originator == null) {
                originator = getOriginator("corporate", null, modsDefinition);
                primaryOriginator.setType(OriginatorTypeType.CORPORATION);
                if (originator == null) {
                    originator = getOriginator("conference", null, modsDefinition);
                    primaryOriginator.setType(OriginatorTypeType.EVENT);
                }
            }
            if (originator != null) {
                primaryOriginator.setValue(originator);
                return primaryOriginator;
            }
        }
        return null;
    }

    static String getOriginator(String type, Boolean isPrimary, ModsDefinition... mods) {
        ArrayList<NameDefinition> origNames = new ArrayList<NameDefinition>();
        for (ModsDefinition modsDefinition : mods) {
            for (NameDefinition nameDefinition : modsDefinition.getName()) {
                NameDefinition add = null;
                if (isPrimary == null) {
                    add = nameDefinition;
                } else if (isPrimary && "primary".equals(getNameUsage(nameDefinition))) {
                    add = nameDefinition;
                } else if (!isPrimary && !"primary".equals(getNameUsage(nameDefinition))) {
                    add = nameDefinition;
                }
                if (add != null && type.equals(nameDefinition.getType())) {
                    origNames.add(add);
                }
            }
            if (!origNames.isEmpty()) {
                break;
            }
        }
        return concat(names(origNames, false), "; ");
    }

    static String concat(List<String> items, String separator) {
        StringBuilder sb = new StringBuilder();
        for (String item : items) {
            sb.append(item).append(separator);
        }
        if (sb.length() > 0) {
            return sb.substring(0, sb.length() - separator.length());
        } else {
            return null;
        }
    }

    static List<String> names(List<NameDefinition> modsNames, boolean includeDate) {
        ArrayList<String> names = new ArrayList<String>();
        for (NameDefinition name : modsNames) {
            StringBuilder sbName = new StringBuilder();
            StringBuilder sbFamily = new StringBuilder();
            StringBuilder sbGiven = new StringBuilder();
            StringBuilder sbDate = new StringBuilder();
            for (NamePartDefinition namePart : name.getNamePart()) {
                String type = namePart.getType();
                if (type == null) {
                    sbName.append(namePart.getValue()).append(' ');
                } else if ("family".equals(type)) {
                    sbFamily.append(namePart.getValue()).append(' ');
                } else if ("given".equals(type)) {
                    sbGiven.append(namePart.getValue()).append(' ');
                } else if (includeDate && "date".equals(type)) {
                    sbDate.append(namePart.getValue()).append(", ");
                }
            }
            if (sbName.length() > 0) {
                sbName.deleteCharAt(sbName.length() - 1);
            }
            if (sbFamily.length() > 0) {
                if (sbName.length() > 0) {
                    sbName.append(", ");
                }
                sbName.append(sbFamily.substring(0, sbFamily.length() - 1));
            }
            if (sbGiven.length() > 0) {
                if (sbName.length() > 0) {
                    sbName.append(", ");
                }
                sbName.append(sbGiven.substring(0, sbGiven.length() - 1));
            }
            if (sbDate.length() > 0) {
                if (sbName.length() > 0) {
                    sbName.append(", ");
                }
                sbName.append(sbDate.substring(0, sbDate.length() - 2));
            }
            if (sbName.length() > 0) {
                names.add(sbName.toString().trim());
            }
        }
        return names;
    }

    static String getNameUsage(NameDefinition name) {
        if (name != null) {
            return String.valueOf(getFixedValue(name, "usage"));
        }
        return null;
    }

    static boolean isValid(IdentifierDefinition id) {
        return getFixedValue(id, "invalid") == null;
    }

    /**
     * Hack to get the real value of a fixed or default XML attribute.
     */
    static Object getFixedValue(Object obj, String fieldName) {
        try {
            Field field = obj.getClass().getDeclaredField(fieldName);
            field.setAccessible(true);
            return field.get(obj);
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
    }
}
