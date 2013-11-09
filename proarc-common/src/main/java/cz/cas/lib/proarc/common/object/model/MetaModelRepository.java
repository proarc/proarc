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
package cz.cas.lib.proarc.common.object.model;

import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;

/**
 * The repository of models of digital objects.
 *
 * <p>For now model metadata are hardcoded.
 *
 * @author Jan Pokorsky
 */
public final class MetaModelRepository {

    private static final MetaModelRepository INSTANCE = new MetaModelRepository();

//    private Collection<MetaModel> repository;

    public static MetaModelRepository getInstance() {
        return INSTANCE;
    }


    private MetaModelRepository() {
//        repository = new ArrayList<MetaModel>();
    }

    public Collection<MetaModel> find(Locale locale) {
        // for now it is read only repository
        String lang = locale.getLanguage();
        List<MetaModel> models = new ArrayList<MetaModel>();
        models.add(new MetaModel(
                "model:periodical", true, null,
                "cs".equals(lang) ? "Periodikum" : "Periodical",
                ModsCutomEditorType.EDITOR_PERIODICAL,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                "model:periodicalvolume", null, null,
                "cs".equals(lang) ? "Ročník" : "Periodical Volume",
                ModsCutomEditorType.EDITOR_PERIODICAL_VOLUME,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                "model:periodicalitem", null, null,
                "cs".equals(lang) ? "Výtisk" : "Periodical Item",
                ModsCutomEditorType.EDITOR_PERIODICAL_ISSUE,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                "model:monograph", true, null,
                "cs".equals(lang) ? "Monografie" : "Monograph",
                ModsCutomEditorType.EDITOR_MONOGRAPH,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                "model:monographunit", null, null,
                "cs".equals(lang) ? "Monografie - volná část" : "Monograph Unit",
                ModsCutomEditorType.EDITOR_MONOGRAPH_UNIT,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                "model:page", null, true,
                "cs".equals(lang) ? "Strana" : "Page",
                ModsCutomEditorType.EDITOR_PAGE,
                EnumSet.complementOf(EnumSet.of(DatastreamEditorType.CHILDREN))
                ));

        return models;
    }

    public MetaModel find(String model) {
        for (MetaModel metaModel : find(Locale.ENGLISH)) {
            if (metaModel.getPid().equals(model)) {
                return metaModel;
            }
        }
        return null;
    }
}
