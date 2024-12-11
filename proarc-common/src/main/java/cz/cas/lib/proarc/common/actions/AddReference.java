/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.common.actions;

import cz.cas.lib.proarc.common.xml.citation.Citation;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.xml.citation.CitationList;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.ExtentDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.NamePartDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.RelatedItemDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.StringReader;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

/**
 * @author Lukas Sykora
 */
public class AddReference {

    private static final Logger LOG = Logger.getLogger(AddReference.class.getName());

    private String pid;
    private Boolean structured;
    private String reference;
    private String model;

    public AddReference(Boolean structured, String reference) {
        this.structured = structured;
        this.reference = reference;
    }

    public Result toPid(String pid) {
        this.pid = pid;

        try {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            ProArcObject fo = dom.find(pid, null);
            this.model = new RelationEditor(fo).getModel();
            DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
            NdkMapper.Context context = new NdkMapper.Context(handler);
            NdkMapper mapper = NdkMapper.get(model);
            mapper.setModelId(model);

            XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                    MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                    MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
            ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
            ModsDefinition mods = modsStreamEditor.read();
            updateMods(mods);
            mapper.createMods(mods, context);
            modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);
            OaiDcType dc = mapper.toDc(mods, context);
            DcStreamEditor dcEditor = handler.objectMetadata();
            DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
            dcr.setDc(dc);
            dcEditor.write(handler, dcr, null);

            fo.setLabel(mapper.toLabel(mods));
            fo.flush();

            return new Result();
        } catch (Exception ex) {
            return new Result(ex.getMessage(), new DigitalObjectException(pid, ex));
        }
    }

    private void updateMods(ModsDefinition mods) {
        if (this.structured == null || !this.structured) {
            addUnstructuredReference(mods);
        } else {
            addStructuredReference(mods);
        }
    }

    private void addStructuredReference(ModsDefinition mods) {
        List<Citation> citations = createCitation();

        for (Citation citation : citations) {
            String newId = getNextReferenceId(mods.getRelatedItem());

            RelatedItemDefinition relatedItem = new RelatedItemDefinition();
            mods.getRelatedItem().add(relatedItem);
            relatedItem.setType("references");
            relatedItem.setID(newId);

            if (hasValue(citation.getArticle_title())) {
                TitleInfoDefinition titleInfo = new TitleInfoDefinition();
                relatedItem.getTitleInfo().add(titleInfo);
                StringPlusLanguage title = new StringPlusLanguage();
                titleInfo.getTitle().add(title);
                title.setValue(citation.getArticle_title());
            }

            if (hasValue(citation.getAuthor())) {
                String surname = getSurname(citation.getAuthor());
                String firstName = getFirstName(citation.getAuthor());

                NameDefinition name = new NameDefinition();
                relatedItem.getName().add(name);
                name.setType("personal");
                NamePartDefinition surnameNamepart = new NamePartDefinition();
                name.getNamePart().add(surnameNamepart);
                surnameNamepart.setType("family");
                surnameNamepart.setValue(surname);
                if (hasValue(firstName)) {
                    NamePartDefinition fisrtnameNamepart = new NamePartDefinition();
                    name.getNamePart().add(fisrtnameNamepart);
                    fisrtnameNamepart.setType("given");
                    fisrtnameNamepart.setValue(firstName);
                }
            }

            if (hasValue(citation.getDoi())) {
                IdentifierDefinition identifier = new IdentifierDefinition();
                relatedItem.getIdentifier().add(identifier);
                identifier.setType("doi");
                identifier.setValue(citation.getDoi());
            }

            if (hasValue(citation.getIsbn())) {
                IdentifierDefinition identifier = new IdentifierDefinition();
                relatedItem.getIdentifier().add(identifier);
                identifier.setType("isbn");
                identifier.setValue(citation.getIsbn());
            }

            if (hasValue(citation.getFirst_page())) {
                PartDefinition part = new PartDefinition();
                relatedItem.getPart().add(part);
                ExtentDefinition extent = new ExtentDefinition();
                extent.setUnit("pages");
                part.getExtent().add(extent);
                StringPlusLanguage startPage = new StringPlusLanguage();
                extent.setStart(startPage);
                startPage.setValue(citation.getFirst_page());
                if (hasValue(citation.getLast_page())) {
                    StringPlusLanguage lastPage = new StringPlusLanguage();
                    extent.setEnd(lastPage);
                    lastPage.setValue(citation.getLast_page());
                }
            }

            if (hasValue(citation.getJournal_title()) || hasValue(citation.getVolume_title()) || hasValue(citation.getVolume()) ||
                    hasValue(citation.getIssue()) || hasValue(citation.getcYear()) || hasValue(citation.getIssn())) {
                RelatedItemDefinition relatedItem2 = new RelatedItemDefinition();
                relatedItem2.setType("host");
                relatedItem.getRelatedItem().add(relatedItem2);
                if (hasValue(citation.getJournal_title())) {
                    TitleInfoDefinition titleInfo = new TitleInfoDefinition();
                    relatedItem2.getTitleInfo().add(titleInfo);
                    titleInfo.setOtherType("title");
                    StringPlusLanguage title = new StringPlusLanguage();
                    titleInfo.getTitle().add(title);
                    title.setValue(citation.getJournal_title());
                }
                if (hasValue(citation.getVolume_title())) {
                    TitleInfoDefinition titleInfo = new TitleInfoDefinition();
                    relatedItem2.getTitleInfo().add(titleInfo);
                    titleInfo.setOtherType("volume");
                    StringPlusLanguage title = new StringPlusLanguage();
                    titleInfo.getTitle().add(title);
                    title.setValue(citation.getVolume_title());
                }
                if (hasValue(citation.getVolume()) || hasValue(citation.getIssue())) {
                    PartDefinition part = new PartDefinition();
                    relatedItem2.getPart().add(part);
                    if (hasValue(citation.getVolume())) {
                        DetailDefinition detail = new DetailDefinition();
                        part.getDetail().add(detail);
                        detail.setType("volume");
                        StringPlusLanguage volume = new StringPlusLanguage();
                        detail.getNumber().add(volume);
                        volume.setValue(citation.getVolume());
                    }
                    if (hasValue(citation.getIssue())) {
                        DetailDefinition detail = new DetailDefinition();
                        part.getDetail().add(detail);
                        detail.setType("issue");
                        StringPlusLanguage issue = new StringPlusLanguage();
                        detail.getNumber().add(issue);
                        issue.setValue(citation.getIssue());
                    }
                }
                if (hasValue(citation.getcYear())) {
                    OriginInfoDefinition originInfo = new OriginInfoDefinition();
                    relatedItem2.getOriginInfo().add(originInfo);
                    DateDefinition date = new DateDefinition();
                    originInfo.getDateIssued().add(date);
                    date.setValue(citation.getcYear());
                }

                if (hasValue(citation.getIssn())) {
                    IdentifierDefinition identifier = new IdentifierDefinition();
                    relatedItem2.getIdentifier().add(identifier);
                    identifier.setType("issn");
                    identifier.setValue(citation.getIssn());
                }
            }
        }
    }

    private String getFirstName(String value) {
        if (value.contains(", ")) {
            return value.split(", ")[1];
        } else {
            return null;
        }
    }

    private String getSurname(String value) {
        if (value.contains(", ")) {
            return value.split(", ")[0];
        } else {
            return value;
        }
    }

    private boolean hasValue(String value) {
        return value != null && !value.isEmpty();
    }

    private List<Citation> createCitation() {
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(Citation.class);
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            StringReader reader = new StringReader(this.reference);
            return Collections.singletonList((Citation) unmarshaller.unmarshal(reader));
        } catch (JAXBException e) {
            try {
                JAXBContext jaxbContext = JAXBContext.newInstance(CitationList.class);
                Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
                StringReader reader = new StringReader(this.reference);
                CitationList citationList = (CitationList) unmarshaller.unmarshal(reader);
                return citationList.getCitations();
            } catch (JAXBException ex) {
                LOG.warning("Nepodarilo se vytvorit objekt ze zadaneho textu: " + this.reference);
                return null;
            }
        }
    }

    private void addUnstructuredReference(ModsDefinition mods) {
        String[] references = this.reference.split("\n");

        for (String reference : references) {
            RelatedItemDefinition relatedItemDefinition = new RelatedItemDefinition();
            relatedItemDefinition.setType("references");
            relatedItemDefinition.setID(getNextReferenceId(mods.getRelatedItem()));
            NoteDefinition noteDefinition = new NoteDefinition();
            noteDefinition.setType("source note");
            noteDefinition.setValue(reference.trim());
            relatedItemDefinition.getNote().add(noteDefinition);
            mods.getRelatedItem().add(relatedItemDefinition);
        }
    }

    private String getNextReferenceId(List<RelatedItemDefinition> relatedItems) {
        RelatedItemDefinition relatedItemDefinition = null;
        for (RelatedItemDefinition relatedItem : relatedItems) {
            if (relatedItem != null && relatedItem.getType() != null && "references".equals(relatedItem.getType())) {
                relatedItemDefinition = relatedItem;
            }
        }
        if (relatedItemDefinition != null) {
            String id = relatedItemDefinition.getID();
            try {
                int number = Integer.parseInt(id.replaceAll("[^0-9]", ""));
                number++;
                return "ref" + String.format("%04d", number);
            } catch (Exception ex) {
                // hodnota nelze rozparsovat, vraci se defaultni hodnota
                return "ref0001";
            }
        } else {
            return "ref0001";
        }
    }

    public class Result {

        private String status;
        private String pid;
        private DigitalObjectException ex;

        public Result(String updatedPid, DigitalObjectException e) {
            status = "error";
            pid = updatedPid;
            ex = e;
        }

        public Result() {
            status = "ok";
        }

        public String getStatus() {
            return status;
        }

        public String getPid() {
            return pid;
        }

        public DigitalObjectException getEx() {
            return ex;
        }
    }
}
