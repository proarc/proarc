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

import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper.IdentifierItem;
import cz.cas.lib.proarc.common.mods.custom.PageMapper.Page;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.Text;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Handles properties: identifiers, page number, page index, page type, note
 *
 * @author Jan Pokorsky
 */
public final class PageMapper implements Mapping.Mapper<Page> {

    private final IdentifierMapper identMap = new IdentifierMapper();

    @Override
    public Page map(ModsDefinition mods) {
        NodeLookup nlookup = new NodeLookup(mods);
        Page page = new Page();
        page.setIdentifiers(identMap.map(mods));
        page.setNumber(MapperUtils.toString(nlookup.getPageNumber(false)));
        page.setIndex(MapperUtils.toString(nlookup.getPageIndex(false)));

        Text note = nlookup.getNote(false);
        page.setNote(note == null ? null : note.getValue());

        PartDefinition part = nlookup.getPart(false);
        page.setType(part == null ? null : part.getType());
        return page;
    }

    @Override
    public ModsDefinition map(ModsDefinition mods, Page page) {
        identMap.map(mods, page.getIdentifiers());
        NodeLookup nlookup = new NodeLookup(mods);
        updateType(page, nlookup);
        updateIndex(page, nlookup);
        updateNumber(page, nlookup);
        updateNote(page, nlookup);
        new TypeOfResourceMapper().map(mods, TypeOfResourceMapper.Type.TEXT);

        return mods;
    }

    public void updatePage(ModsDefinition mods, String pageIndex, String pageNumber, String pageType) {
        Page page = map(mods);
        if (pageIndex != null) {
            page.setIndex(pageIndex.isEmpty() ? null : pageIndex);
        }
        if (pageNumber != null) {
            page.setNumber(pageNumber.isEmpty() ? null : pageNumber);
        }
        if (pageType != null) {
            page.setType(pageType.isEmpty() ? null : pageType);
        }
        map(mods, page);
    }

    private static void updateNote(Page page, NodeLookup nlookup) {
        if (page.getNote() != null) {
            nlookup.getNote(true).setValue(page.getNote());
        } else {
            Text note = nlookup.getNote(false);
            if (note != null) {
                note.setValue(null);
            }
        }
    }

    private static void updateNumber(Page page, NodeLookup nlookup) {
        if (page.getNumber() != null) {
            nlookup.getPageNumber(true).setValue(page.getNumber());
        } else {
            StringPlusLanguage pageNumber = nlookup.getPageNumber(false);
            if (pageNumber != null) {
                pageNumber.setValue(null);
            }
        }
    }

    private static void updateIndex(Page page, NodeLookup nlookup) {
        if (page.getIndex() != null) {
            nlookup.getPageIndex(true).setValue(page.getIndex());
        } else {
            StringPlusLanguage pageIndex = nlookup.getPageIndex(false);
            if (pageIndex != null) {
                pageIndex.setValue(null);
            }
        }
    }

    private static void updateType(Page page, NodeLookup nlookup) {
        if (page.getType() != null) {
            nlookup.getPart(true).setType(page.getType());
        } else {
            PartDefinition part = nlookup.getPart(false);
            if (part != null) {
                part.setType(null);
            }
        }
    }

    private static final class NodeLookup {

        private final ObjectFactory factory = new ObjectFactory();
        private ModsDefinition mods;
        private PartDefinition part;
        private DetailDefinition detailForNumber;
        private DetailDefinition detailForIndex;
        private StringPlusLanguage pageIndex;
        private StringPlusLanguage pageNumber;
        private Text note;

        public NodeLookup(ModsDefinition modsType) {
            this.mods = modsType;
        }

        public StringPlusLanguage getPageIndex(boolean create) {
            if (pageIndex == null) {
                if (getDetailForIndex(create, "pageIndex") != null) {
                    pageIndex = detailForIndex.getNumber().stream().findFirst().orElse(null);
                }
            }
            if (pageIndex == null && create) {
                pageIndex = factory.createStringPlusLanguage();
                detailForIndex.getNumber().add(0, pageIndex);
            }
            return pageIndex;
        }

        public StringPlusLanguage getPageNumber(boolean create) {
            if (pageNumber == null) {
                if (getDetailForNumber(create, "pageNumber") != null) {
                    pageNumber = detailForNumber.getNumber().stream().findFirst().orElse(null);
                }
            }
            if (pageNumber == null && create) {
                pageNumber = factory.createStringPlusLanguage();
                detailForNumber.getNumber().add(pageNumber);
            }
            return pageNumber;
        }

        public Text getNote(boolean create) {
            if (note == null) {
                if (getPart(create) != null) {
                    note = part.getText().stream().findFirst().orElse(null);
                }
            }
            if (note == null && create) {
                note = factory.createText();
                part.getText().add(note);
            }
            return note;
        }

        public PartDefinition getPart(boolean create) {
            if (part == null) {
                part = mods.getPart().stream().findFirst().orElse(null);
            }
            if (part == null && create) {
                part = factory.createPartDefinition();
                mods.getPart().add(part);
            }
            return part;
        }

        public DetailDefinition getDetailForNumber(boolean create, final String type) {
            detailForNumber = getDetail(create, type, detailForNumber);
            return detailForNumber;
        }

        public DetailDefinition getDetailForIndex(boolean create, final String type) {
            detailForIndex = getDetail(create, type, detailForIndex);
            return detailForIndex;
        }

        private DetailDefinition getDetail(boolean create, String type, DetailDefinition detail) {
            if (detail == null) {
                if (getPart(create) != null) {
                    detail = part.getDetail().stream().filter(d -> type.equals(d.getType()))
                            .findFirst().orElse(null);
                }
            }
            if (detail == null && create) {
                detail = factory.createDetailDefinition();
                detail.setType(type);
                part.getDetail().add(detail);
            }
            return detail;
        }
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Page {
        
        private List<IdentifierItem> identifiers;
        @XmlElement(name = ModsConstants.FIELD_PAGE_NUMBER)
        private String number;
        @XmlElement(name = ModsConstants.FIELD_PAGE_INDEX)
        private String index;
        @XmlElement(name = ModsConstants.FIELD_PAGE_TYPE)
        private String type;
        @XmlElement(name = ModsConstants.FIELD_NOTE)
        private String note;

        public Page() {
        }

        public List<IdentifierItem> getIdentifiers() {
            return identifiers;
        }

        public void setIdentifiers(List<IdentifierItem> identifiers) {
            this.identifiers = identifiers;
        }

        public String getIndex() {
            return index;
        }

        public void setIndex(String pageIndex) {
            this.index = pageIndex;
        }

        public String getNumber() {
            return number;
        }

        public void setNumber(String pageNumber) {
            this.number = pageNumber;
        }

        public String getType() {
            return type;
        }

        public void setType(String pageType) {
            this.type = pageType;
        }

        public String getNote() {
            return note;
        }

        public void setNote(String note) {
            this.note = note;
        }
    }
}
