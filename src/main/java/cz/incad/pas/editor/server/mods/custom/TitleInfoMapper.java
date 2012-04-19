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
package cz.incad.pas.editor.server.mods.custom;

import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.fi.muni.xkremser.editor.server.mods.TitleInfoType;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

/**
 * {@code mods/titleInfo[@type = null]/title}.
 * {@code mods/titleInfo[@type = null]/subTitle}
 * {@code mods/titleInfo[@type = 'alternative' && @displayLabel = 'Klíčový název']/title}
 * {@code mods/titleInfo[@type = 'alternative']/title}
 *
 * XXX: should there be titles:[{title:"", subTitle:""}]?
 *
 * @author Jan Pokorsky
 */
final class TitleInfoMapper {
    
    private static final String ATTR_ALTERNATIVE_TITLE_TYPE = "alternative";
    private static final String ATTR_KEY_TITLE_DISPLAYLABEL = "Klíčový název";

    private final ModsType mods;
    private final ObjectFactory factory = new ObjectFactory();
    private TitleInfoType titleNode;
    private TitleInfoType keyTitleNode;
    private TitleInfoType alternativeTitleNode;
    private boolean searched = false;
    private List<String> titles;
    private List<String> subtitles;
    private List<String> keyTitles;
    private List<String> alternativeTitles;

    public TitleInfoMapper(ModsType mods) {
        this.mods = mods;
    }

    public void setTitles(List<String> newTitles, List<String> newSubtitles) {
        newTitles = MapperUtils.noNull(newTitles);
        newSubtitles = MapperUtils.noNull(newSubtitles);
        searchNodes();
        if (titleNode == null) {
            if (newTitles.isEmpty() && newSubtitles.isEmpty()) {
                // nothing to update
                return ;
            }
            titleNode = factory.createTitleInfoType();
            MapperUtils.add(mods, titleNode);
        }
        List<JAXBElement<String>> group = titleNode.getTitleOrSubTitleOrPartNumber();
        List<JAXBElement<String>> oldies = MapperUtils.find(group,
                ObjectFactory._DetailTypeTitle_QNAME, ObjectFactory._BaseTitleInfoTypeSubTitle_QNAME);
        List<JAXBElement<String>> news = MapperUtils.toJaxb(newTitles, ObjectFactory._DetailTypeTitle_QNAME);
        news.addAll(MapperUtils.toJaxb(newSubtitles, ObjectFactory._BaseTitleInfoTypeSubTitle_QNAME));
        group.removeAll(oldies);
        group.addAll(news);
    }

    public void setKeyTitles(List<String> updates) {
        updates = MapperUtils.noNull(updates);
        searchNodes();
        if (keyTitleNode == null) {
            if (updates.isEmpty()) {
                return ;
            }
            keyTitleNode = factory.createTitleInfoType();
            keyTitleNode.setAtType(ATTR_ALTERNATIVE_TITLE_TYPE);
            keyTitleNode.setDisplayLabel(ATTR_KEY_TITLE_DISPLAYLABEL);
            MapperUtils.add(mods, keyTitleNode);
        }
        updateTitles(keyTitleNode, updates);
    }

    public void setAlternativeTitles(List<String> updates) {
        updates = MapperUtils.noNull(updates);
        searchNodes();
        if (alternativeTitleNode == null) {
            if (updates.isEmpty()) {
                return ;
            }
            alternativeTitleNode = factory.createTitleInfoType();
            alternativeTitleNode.setAtType(ATTR_ALTERNATIVE_TITLE_TYPE);
            MapperUtils.add(mods, alternativeTitleNode);
        }
        updateTitles(alternativeTitleNode, updates);
    }

    private void updateTitles(TitleInfoType node, List<String> titles) {
        List<JAXBElement<String>> group = node.getTitleOrSubTitleOrPartNumber();
        List<JAXBElement<String>> oldies = MapperUtils.find(group,
                ObjectFactory._DetailTypeTitle_QNAME);
        List<JAXBElement<String>> news = MapperUtils.toJaxb(titles, ObjectFactory._DetailTypeTitle_QNAME);
        group.removeAll(oldies);
        group.addAll(news);
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
            return ;
        }
        boolean foundTitles = false;
        boolean foundAlternatives = false;
        boolean foundKeyTitles = false;
        for (TitleInfoType item : MapperUtils.find(mods.getModsGroup(), TitleInfoType.class)) {
            if (foundTitles && foundKeyTitles && foundAlternatives) {
                break;
            }
            String type = item.getAtType();
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
        titles = toTitles(titleNode, ObjectFactory._DetailTypeTitle_QNAME);
        subtitles = toTitles(titleNode, ObjectFactory._BaseTitleInfoTypeSubTitle_QNAME);
        keyTitles = toTitles(keyTitleNode, ObjectFactory._DetailTypeTitle_QNAME);
        alternativeTitles = toTitles(alternativeTitleNode, ObjectFactory._DetailTypeTitle_QNAME);
    }

    private static List<String> toTitles(TitleInfoType node, QName name) {
        List<String> values;
        if (node != null) {
            values = toTitles(node.getTitleOrSubTitleOrPartNumber(), name);
        } else {
            return new ArrayList<String>();
        }
        return values;
    }

    private static List<String> toTitles(List<JAXBElement<String>> list, QName name) {
        List<String> result = new ArrayList<String>();
        for (JAXBElement<String> elm : list) {
            QName qname = elm.getName();
            if (name.equals(qname)) {
                result.add(elm.getValue());
            }
        }
        return result;
    }

}
