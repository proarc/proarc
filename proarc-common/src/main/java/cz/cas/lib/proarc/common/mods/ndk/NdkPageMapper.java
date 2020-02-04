/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.mods.ndk;

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.dublincore.DcUtils;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper.IdentifierItem;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.ModsWrapper;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.ExtentDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionNote;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.Text;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 *
 * @author Jan Pokorsky
 */
public class NdkPageMapper extends NdkMapper {

    private static final Logger LOG = Logger.getLogger(NdkPageMapper.class.getName());
    /** {@code /mods/part/detail@type} */
    private static final String NUMBER_TYPE_PAGE_INDEX = "pageIndex";
    /** {@code /mods/part/detail@type} */
    private static final String NUMBER_TYPE_PAGE_NUMBER = "pageNumber";
    /** {@code /mods/part@type} */
    public static final String PAGE_TYPE_NORMAL = "normalPage";
    /** A default value of {@code /mods/typeOfResource}. */
    public static final String RESOURCE_TYPE_TEXT = "text";

    public static ResourceBundle getPageTypeLabels(Locale locale) {
        return ResourceBundle.getBundle(
                BundleName.MODS_PAGE_TYPES.toString(),
                locale,
                ResourceBundle.Control.getNoFallbackControl(ResourceBundle.Control.FORMAT_PROPERTIES));
    }

    public static String getPageTypeLabel(String pageType, Locale locale) {
        if (pageType == null || pageType.isEmpty()) {
            pageType = NdkPageMapper.PAGE_TYPE_NORMAL;
        }
        try {
            return getPageTypeLabels(locale).getString(pageType);
        } catch (MissingResourceException ex) {
            LOG.warning("Missing page type resource for " + pageType + " locale " + locale);
            return pageType;
        }
    }

    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        TypeOfResourceDefinition textResource = null;
        for (TypeOfResourceDefinition typeOfResource : mods.getTypeOfResource()) {
            if (RESOURCE_TYPE_TEXT.equals(typeOfResource.getValue())) {
                textResource = typeOfResource;
            }
        }
        if (textResource == null) {
            textResource = new TypeOfResourceDefinition();
            textResource.setValue(RESOURCE_TYPE_TEXT);
            mods.getTypeOfResource().add(textResource);
        }
    }

    @Override
    public Page toJsonObject(ModsDefinition mods, Context ctx) {
        Page page = new Page();
        if (NdkPlugin.MODEL_NDK_PAGE.equals(super.getModelId())) {
                for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
                    page.setTitle(getTitleDefinitionValue(titleInfo, "title"));
                    page.setSubtitle(getTitleDefinitionValue(titleInfo, "subtitle"));
                }
                for (NoteDefinition noteDefinition : mods.getNote()) {
                    page.setNote(noteDefinition.getValue() == null ? "" : noteDefinition.getValue());
                }
                for (GenreDefinition genreDefinition : mods.getGenre()) {
                    page.setGenre(genreDefinition.getValue() == null ? "" : genreDefinition.getValue());
                }
                for (TypeOfResourceDefinition typeOfResourceDefinition : mods.getTypeOfResource()) {
                    page.setTypeOfResource(typeOfResourceDefinition.getValue() == null ? "" : typeOfResourceDefinition.getValue());
                }
                for (PhysicalDescriptionDefinition physicalDescription : mods.getPhysicalDescription()) {
                    page.setPhysicalDescription(getPhysicalDescriptionValue(physicalDescription));
                }
                for (PartDefinition partDefinition : mods.getPart()) {
                    page.setExtent(getExtentValue(partDefinition));
                }
        } else {
            List<PartDefinition> parts = mods.getPart();
            if (!parts.isEmpty()) {
                PartDefinition part = parts.get(0);
                List<Text> partTexts = part.getText();
                if (!partTexts.isEmpty()) {
                    Text partText = partTexts.get(0);
                    page.setPhysicalDescription(partText.getValue());
                }
            }
        }
        if (!mods.getPart().isEmpty()) {
            String pageIndex = null;
            String pageNumber = null;
            String pageType = null;
            for (PartDefinition part : mods.getPart()) {
                if (pageIndex == null) {
                    pageIndex = getNumber(getDetail(part.getDetail(), NUMBER_TYPE_PAGE_INDEX)); 
                }
                if (pageNumber == null) {
                    pageNumber = getNumber(getDetail(part.getDetail(), NUMBER_TYPE_PAGE_NUMBER));
                }
                if (pageType == null) {
                    if (part.getType() != null) {
                        pageType = part.getType();
                    }
                }
            }
            if (pageType == null) {
                pageType = PAGE_TYPE_NORMAL;
            }
            page.setType(pageType);
            page.setNumber(pageNumber);
            page.setIndex(pageIndex);
        } else {
            page.setType(PAGE_TYPE_NORMAL);
        }
        page.setIdentifiers(getIdentifierItems(mods.getIdentifier()));
        return page;
    }

    private String getExtentValue(PartDefinition partDefinition) {
        return !partDefinition.getExtent().isEmpty() ? partDefinition.getExtent().get(0).getStart().getValue() : "";
    }

    private String getPhysicalDescriptionValue(PhysicalDescriptionDefinition physicalDescription) {
        return !physicalDescription.getNote().isEmpty() ? physicalDescription.getNote().get(0).getValue() : "";
    }

    private String getTitleDefinitionValue(TitleInfoDefinition titleInfo, String value) {
        switch (value) {
            case "title":
                return !titleInfo.getTitle().isEmpty() ? titleInfo.getTitle().get(0).getValue() : "";
            case "subtitle":
                return !titleInfo.getSubTitle().isEmpty() ? titleInfo.getSubTitle().get(0).getValue() : "";
            default:
                return "";
        }
    }

    @Override
    public ModsDefinition fromJsonObject(ObjectMapper jsMapper, String json, Context ctx) throws IOException {
        Page page = jsMapper.readValue(json, Page.class);
        return toMods(page, ctx);
    }

    public ModsDefinition createPage(String pid, String pageIndex, String pageNumber, String pageType) {
        Context ctx = new Context(pid);
        return createPage(pageIndex, pageNumber, pageType, ctx);
    }

    public ModsDefinition createPage(String pageIndex, String pageNumber, String pageType, Context ctx) {
        Page page = new Page();
        page.setType(pageType);
        page.setIndex(pageIndex);
        page.setNumber(pageNumber);
        return toMods(page, ctx);
    }

    public ModsDefinition toMods(Page page, Context ctx) {
        ModsDefinition mods = new ModsDefinition();
        toMods(page, mods);
        // ensure the MODS is complete
        createMods(mods, ctx);
        return mods;
    }

    public ModsDefinition toMods(Page page, ModsDefinition mods) {
        String pageType = page.getType();

        String pageIndex = page.getIndex();
        String pageNumber = page.getNumber();
        String pageNote = page.getPhysicalDescription();

        if((pageType != null && !PAGE_TYPE_NORMAL.equals(pageType)) || pageIndex != null || pageNumber != null || pageNote != null) {
            PartDefinition part = new PartDefinition();

            if (pageType != null && !PAGE_TYPE_NORMAL.equals(pageType)) {
                part.setType(pageType);
            }

            mods.getPart().add(part);
            addDetailNumber(pageIndex, NUMBER_TYPE_PAGE_INDEX, part);
            addDetailNumber(pageNumber, NUMBER_TYPE_PAGE_NUMBER, part);

            if (NdkPlugin.MODEL_NDK_PAGE.equals(super.getModelId())) {
                setNdkPageMods(page, mods);
            } else {
                if (pageNote != null) {
                    Text text = new Text();
                    text.setValue(pageNote);
                    part.getText().add(text);
                }
            }
        }

        mods.getIdentifier().addAll(getIdentifierDefinition(page.getIdentifiers()));
        return mods;
    }




    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        Page page = toJsonObject(mods, ctx);
        if (NdkPlugin.MODEL_NDK_PAGE.equals(super.getModelId())) {
            if (page.getTitle() != null) {
                DcUtils.addTitle(dc, page.getTitle());
            }
            if (page.getSubtitle() != null) {
                DcUtils.addTitle(dc, page.getSubtitle());
            }
            if (page.getExtent() != null) {
                DcUtils.addElementType(dc.getCoverages(), page.getExtent());
            }
            if (page.getPhysicalDescription() != null) {
                DcUtils.addElementType(dc.getDescriptions(), page.getPhysicalDescription());
            }
        } else {
            if (page.getNumber() != null) {
                DcUtils.addTitle(dc, page.getNumber());
            }
            DcUtils.addElementType(dc.getTypes(), "text");
        }
        return dc;
    }

    @Override
    protected String createObjectLabel(ModsDefinition mods) {
        Page page = toJsonObject(mods, null);
        StringBuilder sb = new StringBuilder();
        if (page.getNumber() != null) {
            sb.append(page.getNumber());
        } else {
            sb.append('?');
        }
        if (page.getType() != null && !PAGE_TYPE_NORMAL.equalsIgnoreCase(page.getType())) {
            sb.append(", ").append(page.getType());
        }
        return sb.toString();
    }



    private DetailDefinition getDetail(List<DetailDefinition> details, String type) {
        for (DetailDefinition detail : details) {
            if (type.equals(detail.getType())) {
                return detail;
            }
        }
        return null;
    }

    private String getNumber(DetailDefinition detail) {
        if (detail != null) {
            List<StringPlusLanguage> numbers = detail.getNumber();
            if (!numbers.isEmpty()) {
                return numbers.get(0).getValue();
            }
        }
        return null;
    }

    private List<IdentifierItem> getIdentifierItems(List<IdentifierDefinition> ids) {
        List<IdentifierItem> iis = new ArrayList<>(ids.size());
        for (IdentifierDefinition id : ids) {
            iis.add(new IdentifierItem(id.getType(), id.getValue()));
        }
        return iis;
    }



    @XmlRootElement(name = "page")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Page extends ModsWrapper {

        private List<IdentifierItem> identifiers;
        @XmlElement(name = ModsConstants.FIELD_PAGE_NUMBER)
        private String number;
        @XmlElement(name = ModsConstants.FIELD_PAGE_NUMBER_SPLIT)
        private String pageNumber;
        @XmlElement(name = ModsConstants.FIELD_PAGE_INDEX)
        private String index;
        @XmlElement(name = ModsConstants.FIELD_PAGE_TYPE)
        private String type;
        @XmlElement(name = ModsConstants.FIELD_NOTE)
        private String physicalDescription;
        @XmlElement(name = ModsConstants.FIELD_PAGE_TITLE)
        private String title;
        @XmlElement(name = ModsConstants.FIELD_PAGE_SUBTITLE)
        private String subtitle;
        @XmlElement(name = ModsConstants.FIELD_PAGE_EXTENT)
        private String extent;
        @XmlElement(name = ModsConstants.FIELD_PAGE_NOTE)
        private String note;
        @XmlElement(name = ModsConstants.FIELD_PAGE_GENRE)
        private String genre;
        @XmlElement(name = ModsConstants.FIELD_PAGE_TYPEOFRESOURCE)
        private String typeOfResource;

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
            return number == null ? pageNumber : number;
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

        public String getPhysicalDescription() {
            return physicalDescription;
        }

        public void setPhysicalDescription(String physicalDescription) {
            this.physicalDescription = physicalDescription;
        }

        public String getTitle() {
            return title;
        }

        public void setTitle(String title) {
            this.title = title;
        }

        public String getSubtitle() {
            return subtitle;
        }

        public void setSubtitle(String subtitle) {
            this.subtitle = subtitle;
        }

        public String getExtent() {
            return extent;
        }

        public void setExtent(String extent) {
            this.extent = extent;
        }

        public String getGenre() {
            return genre;
        }

        public void setGenre(String genre) {
            this.genre = genre;
        }

        public String getTypeOfResource() {
            return typeOfResource;
        }

        public void setTypeOfResource(String typeOfResource) {
            this.typeOfResource = typeOfResource;
        }

        public String getPageNumber() {
            return pageNumber;
        }
    }

}
