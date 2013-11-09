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
import cz.fi.muni.xkremser.editor.server.mods.DetailType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.fi.muni.xkremser.editor.server.mods.PartType;
import cz.fi.muni.xkremser.editor.server.mods.UnstructuredText;
import java.util.List;
import javax.xml.bind.JAXBElement;
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
    public Page map(ModsType mods) {
        NodeLookup nlookup = new NodeLookup(mods);
        Page page = new Page();
        page.setIdentifiers(identMap.map(mods));
        page.setNumber(MapperUtils.toString(nlookup.getPageNumber(false)));
        page.setIndex(MapperUtils.toString(nlookup.getPageIndex(false)));

        UnstructuredText note = nlookup.getNote(false);
        page.setNote(note == null ? null : note.getValue());

        PartType part = nlookup.getPart(false);
        page.setType(part == null ? null : part.getType());
        return page;
    }

    @Override
    public ModsType map(ModsType mods, Page page) {
        identMap.map(mods, page.getIdentifiers());
        NodeLookup nlookup = new NodeLookup(mods);
        updateType(page, nlookup);
        updateIndex(page, nlookup);
        updateNumber(page, nlookup);
        updateNote(page, nlookup);
        new TypeOfResourceMapper().map(mods, TypeOfResourceMapper.Type.TEXT);

        return mods;
    }

    private static void updateNote(Page page, NodeLookup nlookup) {
        if (page.getNote() != null) {
            nlookup.getNote(true).setValue(page.getNote());
        } else {
            UnstructuredText note = nlookup.getNote(false);
            if (note != null) {
                note.setValue(null);
            }
        }
    }

    private static void updateNumber(Page page, NodeLookup nlookup) {
        if (page.getNumber() != null) {
            nlookup.getPageNumber(true).setValue(page.getNumber());
        } else {
            JAXBElement<String> pageNumber = nlookup.getPageNumber(false);
            if (pageNumber != null) {
                pageNumber.setValue(null);
            }
        }
    }

    private static void updateIndex(Page page, NodeLookup nlookup) {
        if (page.getIndex() != null) {
            nlookup.getPageIndex(true).setValue(page.getIndex());
        } else {
            JAXBElement<String> pageIndex = nlookup.getPageIndex(false);
            if (pageIndex != null) {
                pageIndex.setValue(null);
            }
        }
    }

    private static void updateType(Page page, NodeLookup nlookup) {
        if (page.getType() != null) {
            nlookup.getPart(true).setType(page.getType());
        } else {
            PartType part = nlookup.getPart(false);
            if (part != null) {
                part.setType(null);
            }
        }
    }

    private static final class NodeLookup {

        private final ObjectFactory factory = new ObjectFactory();
        private ModsType mods;
        private PartType part;
        private DetailType detailForNumber;
        private DetailType detailForIndex;
        private JAXBElement<String> pageIndex;
        private JAXBElement<String> pageNumber;
        private UnstructuredText note;

        public NodeLookup(ModsType modsType) {
            this.mods = modsType;
        }

        public JAXBElement<String> getPageIndex(boolean create) {
            if (pageIndex == null) {
                if (getDetailForIndex(create, "pageIndex") != null) {
                    pageIndex = MapperUtils.findFirst(detailForIndex.getNumberOrCaptionOrTitle(),
                            MapperUtils.<String>jaxbElementSelector(ObjectFactory._DetailTypeNumber_QNAME));
                }
            }
            if (pageIndex == null && create) {
                pageIndex = factory.createDetailTypeNumber(null);
                detailForIndex.getNumberOrCaptionOrTitle().add(0, pageIndex);
            }
            return pageIndex;
        }

        public JAXBElement<String> getPageNumber(boolean create) {
            if (pageNumber == null) {
                if (getDetailForNumber(create, "pageNumber") != null) {
                    pageNumber = MapperUtils.findFirst(detailForNumber.getNumberOrCaptionOrTitle(),
                            MapperUtils.<String>jaxbElementSelector(ObjectFactory._DetailTypeNumber_QNAME));
                }
            }
            if (pageNumber == null && create) {
                pageNumber = factory.createDetailTypeNumber(null);
                detailForNumber.getNumberOrCaptionOrTitle().add(pageNumber);
            }
            return pageNumber;
        }

        public UnstructuredText getNote(boolean create) {
            if (note == null) {
                if (getPart(create) != null) {
                    note = MapperUtils.findFirst(part.getDetailOrExtentOrDate(), UnstructuredText.class);
                }
            }
            if (note == null && create) {
                note = factory.createUnstructuredText();
                part.getDetailOrExtentOrDate().add(note);
            }
            return note;
        }

        public PartType getPart(boolean create) {
            if (part == null) {
                part = MapperUtils.findFirst(mods.getModsGroup(), PartType.class);
            }
            if (part == null && create) {
                part = factory.createPartType();
                MapperUtils.add(mods, part);
            }
            return part;
        }

        public DetailType getDetailForNumber(boolean create, final String type) {
            detailForNumber = getDetail(create, type, detailForNumber);
            return detailForNumber;
        }

        public DetailType getDetailForIndex(boolean create, final String type) {
            detailForIndex = getDetail(create, type, detailForIndex);
            return detailForIndex;
        }

        private DetailType getDetail(boolean create, String type, DetailType detail) {
            if (detail == null) {
                if (getPart(create) != null) {
                    detail = MapperUtils.findFirst(
                            MapperUtils.find(part.getDetailOrExtentOrDate(), DetailType.class),
                            MapperUtils.detailSelector(type));
                }
            }
            if (detail == null && create) {
                detail = factory.createDetailType();
                detail.setType(type);
                MapperUtils.add(part, detail);
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
