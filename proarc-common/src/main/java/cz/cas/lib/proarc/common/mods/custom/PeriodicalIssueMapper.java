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
import cz.cas.lib.proarc.common.mods.custom.Mapping.Mapper;
import cz.cas.lib.proarc.common.mods.custom.PeriodicalIssueMapper.PeriodicalIssue;
import cz.cas.lib.proarc.mods.DateDefinition;
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
 * Handles properties: identifiers, issue number, issue sorting number, date of issue, note
 *
 * @author Jan Pokorsky
 */
final class PeriodicalIssueMapper implements Mapper<PeriodicalIssue> {

    private final IdentifierMapper identMap = new IdentifierMapper();

    @Override
    public PeriodicalIssue map(ModsDefinition mods) {
        NodeLookup nlookup = new NodeLookup(mods);
        PeriodicalIssue issue = new PeriodicalIssue();
        issue.setIdentifiers(identMap.map(mods));

        DateDefinition date = nlookup.getDate(false);
        issue.setIssueDate(date == null ? null : date.getValue());

        Text note = nlookup.getNote(false);
        issue.setNote(note == null ? null : note.getValue());

        issue.setIssueNumber(MapperUtils.toString(nlookup.getNumber(false)));
        issue.setIssueSortingNumber(MapperUtils.toString(nlookup.getSortingNumber(false)));

        return issue;
    }

    @Override
    public ModsDefinition map(ModsDefinition mods, PeriodicalIssue issue) {
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
        // XXX issue 43: partial fix; number element will be removed!
        if (issue.getIssueSortingNumber() != null) {
            nlookup.getNumber(true).setValue(issue.getIssueSortingNumber());
//        if (issue.getIssueNumber() != null) {
//            nlookup.getNumber(true).setValue(issue.getIssueNumber());
        } else {
            StringPlusLanguage number = nlookup.getNumber(false);
            if (number != null) {
                number.setValue(null);
            }
        }
    }

    private static void updateSortingNumber(PeriodicalIssue issue, NodeLookup nlookup) {
        if (issue.getIssueSortingNumber() != null) {
            nlookup.getSortingNumber(true).setValue(issue.getIssueSortingNumber());
        } else {
            StringPlusLanguage sortingNumber = nlookup.getSortingNumber(false);
            if (sortingNumber != null) {
                sortingNumber.setValue(null);
            }
        }
    }

    private static void updateDate(PeriodicalIssue issue, NodeLookup nlookup) {
        if (issue.getIssueDate() != null) {
            nlookup.getDate(true).setValue(issue.getIssueDate());
        } else {
            DateDefinition date = nlookup.getDate(false);
            if (date != null) {
                date.setValue(null);
            }
        }
    }

    private static void updateNote(PeriodicalIssue issue, NodeLookup nlookup) {
        if (issue.getNote() != null) {
            nlookup.getNote(true).setValue(issue.getNote());
        } else {
            Text note = nlookup.getNote(false);
            if (note != null) {
                note.setValue(null);
            }
        }
    }

    private static final class NodeLookup {

        private final ObjectFactory factory = new ObjectFactory();
        private ModsDefinition mods;
        private PartDefinition part;
        private DetailDefinition detail;
        private DateDefinition date;
        private StringPlusLanguage number;
        private StringPlusLanguage sortingNumber;
        private Text note;

        public NodeLookup(ModsDefinition mods) {
            this.mods = mods;
        }

        public DateDefinition getDate(boolean create) {
            if (date == null) {
                if (getPart(create) != null) {
                    date = part.getDate().stream().findFirst().orElse(null);
                }
            }
            if (date == null && create) {
                date = factory.createDateDefinition();
                part.getDate().add(date);
            }
            return date;
        }

        public StringPlusLanguage getNumber(boolean create) {
            if (number == null) {
                if (getDetail(create) != null) {
                    number = detail.getNumber().stream().findFirst().orElse(null);
                }
            }
            if (number == null && create) {
                number = factory.createStringPlusLanguage();
                detail.getNumber().add(number);
            }
            return number;
        }

        public StringPlusLanguage getSortingNumber(boolean create) {
            if (sortingNumber == null) {
                if (getDetail(create) != null) {
                    sortingNumber = detail.getCaption().stream().findFirst().orElse(null);
                }
            }
            if (sortingNumber == null && create) {
                sortingNumber = factory.createStringPlusLanguage();
                detail.getCaption().add(sortingNumber);
            }
            return sortingNumber;
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
                part = mods.getPart().stream()
                        .filter(p -> "PeriodicalIssue".equals(p.getType()))
                        .findFirst().orElse(null);
            }
            if (part == null && create) {
                part = factory.createPartDefinition();
                part.setType("PeriodicalIssue");
                mods.getPart().add(part);
            }
            return part;
        }

        public DetailDefinition getDetail(boolean create) {
            if (detail == null) {
                if (getPart(create) != null) {
                    detail = part.getDetail().stream()
                            .filter(d -> "issue".equals(d.getType()))
                            .findFirst().orElse(null);
                }
            }
            if (detail == null && create) {
                detail = factory.createDetailDefinition();
                detail.setType("issue");
                part.getDetail().add(detail);
            }
            return detail;
        }

    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class PeriodicalIssue {

        @XmlElement(name = ModsConstants.FIELD_IDENTIFIERS)
        private List<IdentifierItem> identifiers;
        @XmlElement(name = ModsConstants.FIELD_PER_ISSUE_DATE)
        private String issueDate;
        @XmlElement(name = ModsConstants.FIELD_PER_ISSUE_NUMBER)
        private String issueNumber;
        @XmlElement(name = ModsConstants.FIELD_PER_ISSUE_NUMBER_SORTING)
        private String issueSortingNumber;
        @XmlElement(name = ModsConstants.FIELD_NOTE)
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
