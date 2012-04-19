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

import cz.fi.muni.xkremser.editor.server.mods.BaseDateType;
import cz.fi.muni.xkremser.editor.server.mods.DetailType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.fi.muni.xkremser.editor.server.mods.PartType;
import cz.fi.muni.xkremser.editor.server.mods.UnstructuredText;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.server.mods.custom.IdentifierMapper.IdentifierItem;
import cz.incad.pas.editor.server.mods.custom.Mapping.Mapper;
import cz.incad.pas.editor.server.mods.custom.MapperUtils.Selector;
import cz.incad.pas.editor.server.mods.custom.PeriodicalIssueMapper.PeriodicalIssue;
import java.util.List;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Handles properties: identifiers, issue number, issue sorting number, date of issue, note
 *
 * @author Jan Pokorsky
 */
final class PeriodicalIssueMapper implements Mapper<PeriodicalIssue> {

    private final IdentifierMapper identMap = new IdentifierMapper();

    @Override
    public PeriodicalIssue map(ModsType mods) {
        NodeLookup nlookup = new NodeLookup(mods);
        PeriodicalIssue issue = new PeriodicalIssue();
        issue.setIdentifiers(identMap.map(mods));

        BaseDateType date = nlookup.getDate(false);
        issue.setIssueDate(date == null ? null : date.getValue());

        UnstructuredText note = nlookup.getNote(false);
        issue.setNote(note == null ? null : note.getValue());

        issue.setIssueNumber(MapperUtils.toString(nlookup.getNumber(false)));
        issue.setIssueSortingNumber(MapperUtils.toString(nlookup.getSortingNumber(false)));

        return issue;
    }

    @Override
    public ModsType map(ModsType mods, PeriodicalIssue issue) {
        NodeLookup nlookup = new NodeLookup(mods);
        identMap.map(mods, issue.getIdentifiers());

        updateNumber(issue, nlookup);
        updateSortingNumber(issue, nlookup);
        updateDate(issue, nlookup);
        updateNote(issue, nlookup);
        new TypeOfResourceMapper().map(mods, TypeOfResourceMapper.Type.TEXT);
        return mods;
    }

    private static void updateNumber(PeriodicalIssue issue, NodeLookup nlookup) {
        if (issue.getIssueNumber() != null) {
            nlookup.getNumber(true).setValue(issue.getIssueNumber());
        } else {
            JAXBElement<String> number = nlookup.getNumber(false);
            if (number != null) {
                number.setValue(null);
            }
        }
    }

    private static void updateSortingNumber(PeriodicalIssue issue, NodeLookup nlookup) {
        if (issue.getIssueSortingNumber() != null) {
            nlookup.getSortingNumber(true).setValue(issue.getIssueSortingNumber());
        } else {
            JAXBElement<String> sortingNumber = nlookup.getSortingNumber(false);
            if (sortingNumber != null) {
                sortingNumber.setValue(null);
            }
        }
    }

    private static void updateDate(PeriodicalIssue issue, NodeLookup nlookup) {
        if (issue.getIssueDate() != null) {
            nlookup.getDate(true).setValue(issue.getIssueDate());
        } else {
            BaseDateType date = nlookup.getDate(false);
            if (date != null) {
                date.setValue(null);
            }
        }
    }

    private static void updateNote(PeriodicalIssue issue, NodeLookup nlookup) {
        if (issue.getNote() != null) {
            nlookup.getNote(true).setValue(issue.getNote());
        } else {
            UnstructuredText note = nlookup.getNote(false);
            if (note != null) {
                note.setValue(null);
            }
        }
    }

    private static final class NodeLookup {

        private final ObjectFactory factory = new ObjectFactory();
        private ModsType mods;
        private PartType part;
        private DetailType detail;
        private BaseDateType date;
        private JAXBElement<String> number;
        private JAXBElement<String> sortingNumber;
        private UnstructuredText note;

        public NodeLookup(ModsType mods) {
            this.mods = mods;
        }

        public BaseDateType getDate(boolean create) {
            if (date == null) {
                if (getPart(create) != null) {
                    date = MapperUtils.findFirst(part.getDetailOrExtentOrDate(), BaseDateType.class);
                }
            }
            if (date == null && create) {
                date = factory.createBaseDateType();
                MapperUtils.add(part, date);
            }
            return date;
        }

        public JAXBElement<String> getNumber(boolean create) {
            if (number == null) {
                if (getDetail(create) != null) {
                    number = MapperUtils.findFirst(detail.getNumberOrCaptionOrTitle(),
                            MapperUtils.<String>jaxbElementSelector(ObjectFactory._DetailTypeNumber_QNAME));
                }
            }
            if (number == null && create) {
                number = factory.createDetailTypeNumber(null);
                detail.getNumberOrCaptionOrTitle().add(number);
            }
            return number;
        }

        public JAXBElement<String> getSortingNumber(boolean create) {
            if (sortingNumber == null) {
                if (getDetail(create) != null) {
                    sortingNumber = MapperUtils.findFirst(detail.getNumberOrCaptionOrTitle(),
                            MapperUtils.<String>jaxbElementSelector(ObjectFactory._DetailTypeCaption_QNAME));
                }
            }
            if (sortingNumber == null && create) {
                sortingNumber = factory.createDetailTypeCaption(null);
                detail.getNumberOrCaptionOrTitle().add(sortingNumber);
            }
            return sortingNumber;
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
                part = MapperUtils.findFirst(MapperUtils.find(mods.getModsGroup(), PartType.class), new Selector<PartType>(){

                    @Override
                    public boolean select(PartType item) {
                        return "PeriodicalIssue".equals(item.getType());
                    }
                });
            }
            if (part == null && create) {
                part = factory.createPartType();
                part.setType("PeriodicalIssue");
                MapperUtils.add(mods, part);
            }
            return part;
        }

        public DetailType getDetail(boolean create) {
            if (detail == null) {
                if (getPart(create) != null) {
                    detail = MapperUtils.findFirst(
                            MapperUtils.find(part.getDetailOrExtentOrDate(), DetailType.class),
                            MapperUtils.detailSelector("issue"));
                }
            }
            if (detail == null && create) {
                detail = factory.createDetailType();
                detail.setType("issue");
                MapperUtils.add(part, detail);
            }
            return detail;
        }

    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class PeriodicalIssue {

        @XmlElement(name = ModsCustomDataSource.FIELD_NOTE)
        private List<IdentifierItem> identifiers;
        @XmlElement(name = ModsCustomDataSource.FIELD_PER_ISSUE_DATE)
        private String issueDate;
        @XmlElement(name = ModsCustomDataSource.FIELD_PER_ISSUE_NUMBER)
        private String issueNumber;
        @XmlElement(name = ModsCustomDataSource.FIELD_PER_ISSUE_NUMBER_SORTING)
        private String issueSortingNumber;
        @XmlElement(name = ModsCustomDataSource.FIELD_NOTE)
        private String note;

        public PeriodicalIssue() {
        }

        public List<IdentifierItem> getIdentifiers() {
            return identifiers;
        }

        public void setIdentifiers(List<IdentifierItem> identifiers) {
            this.identifiers = identifiers;
        }

        public String getIssueDate() {
            return issueDate;
        }

        public void setIssueDate(String issueDate) {
            this.issueDate = issueDate;
        }

        public String getIssueNumber() {
            return issueNumber;
        }

        public void setIssueNumber(String issueNumber) {
            this.issueNumber = issueNumber;
        }

        public String getIssueSortingNumber() {
            return issueSortingNumber;
        }

        public void setIssueSortingNumber(String issueSortingNumber) {
            this.issueSortingNumber = issueSortingNumber;
        }

        public String getNote() {
            return note;
        }

        public void setNote(String note) {
            this.note = note;
        }
    }

}
