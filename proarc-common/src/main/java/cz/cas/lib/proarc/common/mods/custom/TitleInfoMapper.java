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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.mods.custom;

import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

/**
 * {@code mods/titleInfo[@type = null]/title}.
 * {@code mods/titleInfo[@type = null]/subTitle}
 * {@code mods/titleInfo[@type = 'alternative' && @displayLabel = 'Klíčový název']/title}
 * {@code mods/titleInfo[@type = 'alternative']/title}
 * <p>
 * XXX: should there be titles:[{title:"", subTitle:""}]?
 *
 * @author Jan Pokorsky
 */
final class TitleInfoMapper {

    private static final String ATTR_ALTERNATIVE_TITLE_TYPE = "alternative";
    private static final String ATTR_KEY_TITLE_DISPLAYLABEL = "Klíčový název";

    private final ModsDefinition mods;
    private final ObjectFactory factory = new ObjectFactory();
    private TitleInfoDefinition titleNode;
    private TitleInfoDefinition keyTitleNode;
    private TitleInfoDefinition alternativeTitleNode;
    private boolean searched = false;
    private List<String> titles;
    private List<String> subtitles;
    private List<String> keyTitles;
    private List<String> alternativeTitles;

    public TitleInfoMapper(ModsDefinition mods) {
        this.mods = mods;
    }

    public void setTitles(List<String> newTitles, List<String> newSubtitles) {
        newTitles = MapperUtils.noNull(newTitles);
        newSubtitles = MapperUtils.noNull(newSubtitles);
        searchNodes();
        if (titleNode == null) {
            if (newTitles.isEmpty() && newSubtitles.isEmpty()) {
                // nothing to update
                return;
            }
            titleNode = factory.createTitleInfoDefinition();
            mods.getTitleInfo().add(titleNode);
        }
        updateTitles(titleNode, newTitles);
        List<StringPlusLanguage> subTitle = titleNode.getSubTitle();
        subTitle.clear();
        subTitle.addAll(MapperUtils.toStringPlusLanguage(newSubtitles));
    }

    public void setKeyTitles(List<String> updates) {
        updates = MapperUtils.noNull(updates);
        searchNodes();
        if (keyTitleNode == null) {
            if (updates.isEmpty()) {
                return;
            }
            keyTitleNode = factory.createTitleInfoDefinition();
            keyTitleNode.setType(ATTR_ALTERNATIVE_TITLE_TYPE);
            keyTitleNode.setDisplayLabel(ATTR_KEY_TITLE_DISPLAYLABEL);
            mods.getTitleInfo().add(keyTitleNode);
        }
        updateTitles(keyTitleNode, updates);
    }

    public void setAlternativeTitles(List<String> updates) {
        updates = MapperUtils.noNull(updates);
        searchNodes();
        if (alternativeTitleNode == null) {
            if (updates.isEmpty()) {
                return;
            }
            alternativeTitleNode = factory.createTitleInfoDefinition();
            alternativeTitleNode.setType(ATTR_ALTERNATIVE_TITLE_TYPE);
            mods.getTitleInfo().add(alternativeTitleNode);
        }
        updateTitles(alternativeTitleNode, updates);
    }

    private void updateTitles(TitleInfoDefinition node, List<String> titles) {
        List<StringPlusLanguage> oldies = node.getTitleStringPlusLanguage();
        oldies.clear();
        oldies.addAll(MapperUtils.toStringPlusLanguage(titles));
    }

    public List<String> getTitles() {
        searchNodes();
        readValues();
        return titles;
    }

    public List<String> getAlternativeTitles() {
        searchNodes();
        readValues();
        return alternativeTitles;
    }

    public List<String> getKeyTitles() {
        searchNodes();
        readValues();
        return keyTitles;
    }

    public List<String> getSubtitles() {
        searchNodes();
        readValues();
        return subtitles;
    }

    private void searchNodes() {
        if (searched) {
            return;
        }
        boolean foundTitles = false;
        boolean foundAlternatives = false;
        boolean foundKeyTitles = false;
        for (TitleInfoDefinition item : mods.getTitleInfo()) {
            if (foundTitles && foundKeyTitles && foundAlternatives) {
                break;
            }
            String type = item.getType();
            if (type == null) {
                if (foundTitles) {
                    continue;
                }
                titleNode = item;
                foundTitles = true;
            } else if (ATTR_ALTERNATIVE_TITLE_TYPE.equals(type) && ATTR_KEY_TITLE_DISPLAYLABEL.equals(item.getDisplayLabel())) {
                if (foundKeyTitles) {
                    continue;
                }
                keyTitleNode = item;
                foundKeyTitles = true;
            } else if (ATTR_ALTERNATIVE_TITLE_TYPE.equals(type)) {
                if (foundAlternatives) {
                    continue;
                }
                alternativeTitleNode = item;
                foundAlternatives = true;
            }
        }
        searched = true;
    }

    private void readValues() {
        assert searched;
        titles = toTitles(titleNode, ti -> ti.getTitleStringPlusLanguage());
        subtitles = toTitles(titleNode, ti -> ti.getSubTitle());
        keyTitles = toTitles(keyTitleNode, ti -> ti.getTitleStringPlusLanguage());
        alternativeTitles = toTitles(alternativeTitleNode, ti -> ti.getTitleStringPlusLanguage());
    }

    private static List<String> toTitles(TitleInfoDefinition node, Function<TitleInfoDefinition, List<StringPlusLanguage>> f) {
        List<String> values;
        if (node != null) {
            values = MapperUtils.toStringPlusLanguageValue(f.apply(node));
        } else {
            return new ArrayList<>();
        }
        return values;
    }

}
