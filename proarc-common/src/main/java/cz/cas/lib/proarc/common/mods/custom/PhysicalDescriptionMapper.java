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

import cz.cas.lib.proarc.common.mods.custom.ArrayMapper.ArrayItem;
import cz.cas.lib.proarc.common.mods.custom.ArrayMapper.ItemMapper;
import cz.cas.lib.proarc.mods.Extent;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionNote;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * pairs of {@code mods/physicalDescription/extenet}s.
 * <p/><b>NOTE: KNAV Kramerius 3 format</b>
 * @see <a href='https://github.com/ceskaexpedice/kramerius/blob/master/import-cmdtool/src/main/resources/model_periodical_MODS.xsl'>model_periodical_MODS.xsl</a>
 *
 * @author Jan Pokorsky
 */
final class PhysicalDescriptionMapper {

    private static final String PRESERVATION_STATE_OF_ART = "preservationStateOfArt";
    private static final String PRESERVATION_TREATMENT = "action";
    private static final String NO_PRESERVATION = new String();

    private final ArrayMapper<Object, ArrayItem> arrayMapper =
            new ArrayMapper<>(new PhysicalDescriptionItemMapper());

    public List<ArrayItem> map(ModsDefinition mods) {
        Optional<PhysicalDescriptionDefinition> pd = mods.getPhysicalDescription().stream().findFirst();
        if (!pd.isPresent()) {
            return new ArrayList<>();
        }
        ArrayList<Object> items = new ArrayList<>();
        items.addAll(pd.get().getExtent());
        items.addAll(pd.get().getNote());
        return arrayMapper.map(items);
    }

    public ModsDefinition mapPairs(ModsDefinition mods, List<ExtentPair> pairs) {
        return map(mods, pairs, NO_PRESERVATION, NO_PRESERVATION);
    }

    public ModsDefinition map(ModsDefinition mods, List<ExtentPair> pairs, String preservationTreatment, String preservationStateOfArt) {
        pairs = MapperUtils.noNull(pairs);
        List<ArrayItem> oldies = map(mods);
        List<ExtentItem> toExtents = toExtents(pairs);
        List<ArrayItem> news;
        if (NO_PRESERVATION.equals(preservationTreatment)) {
            List<ArrayItem> unknowns = filter(oldies, false, ExtentItem.class);
            news = MapperUtils.mergeList(toExtents, unknowns);
        } else {
            List<ArrayItem> unknowns = filter(oldies, false, ExtentItem.class, NoteItem.class);
            List<NoteItem> notes = MapperUtils.find(oldies, NoteItem.class);
            updateTreatmentItem(oldies, notes, preservationTreatment, PRESERVATION_TREATMENT);
            updateTreatmentItem(oldies, notes, preservationStateOfArt, PRESERVATION_STATE_OF_ART);
            news = MapperUtils.mergeList(toExtents, notes, unknowns);
        }

        return map(mods, news);
    }

    private void updateTreatmentItem(List<ArrayItem> oldies, List<NoteItem> notes, String treatment, String type) {
        if (treatment != null) {
            NoteItem ni = getPreservationItem(oldies, type);
            if (ni == null) {
                ni = new NoteItem(null, treatment, type);
                notes.add(ni);
            }
            ni.setValue(treatment);
            ni.ignore = false;
        } else {
            NoteItem ni = getPreservationItem(oldies, type);
            if (ni != null) {
                ni.setValue(treatment);
            }
        }
    }

    public ModsDefinition map(ModsDefinition mods, List<ArrayItem> items) {
        PhysicalDescriptionDefinition pd = mods.getPhysicalDescription().stream().findFirst().orElse(null);
        if (pd == null) {
            if (items.isEmpty()) {
                return mods;
            } else {
                pd = new PhysicalDescriptionDefinition();
                mods.getPhysicalDescription().add(pd);
            }
        }
        ArrayList<Object> oldies = new ArrayList<>();
        final List<Extent> extents = pd.getExtent();
        final List<PhysicalDescriptionNote> notes = pd.getNote();
        oldies.addAll(extents);
        oldies.addAll(notes);
        List<Object> updates = arrayMapper.map(items, oldies);
        extents.clear();
        notes.clear();
        updates.forEach(update -> {
            if (update instanceof Extent) {
                extents.add((Extent) update);
            } else if (update instanceof PhysicalDescriptionNote) {
                notes.add((PhysicalDescriptionNote) update);
            } else {
                throw new IllegalStateException(update.getClass().getName());
            }
        });

        return mods;
    }

    public static List<ExtentPair> toPairs(List<ArrayItem> items) {
        ArrayList<ExtentPair> pairs = new ArrayList<>();
        List<ExtentItem> extents = MapperUtils.find(items, ExtentItem.class);
        for (Iterator<ExtentItem> it = extents.iterator(); it.hasNext();) {
            ExtentItem item = it.next();
            ExtentPair pair = new ExtentPair(item.getValue(), item.getArrayIndex(), null, null);
            pairs.add(pair);
            if (it.hasNext()) {
                item = it.next();
                pair.setSize(item.getValue());
                pair.setSizeIndex(item.getArrayIndex());
            }
        }
        return pairs;
    }

    public static List<ExtentItem> toExtents(List<ExtentPair> pairs) {
        ArrayList<ExtentItem> items = new ArrayList<>(pairs.size() * 2);
        for (ExtentPair pair : pairs) {
            items.add(new ExtentItem(pair.getExtentIndex(), pair.getExtent()));
            items.add(new ExtentItem(pair.getSizeIndex(), pair.getSize()));
        }
        return items;
    }

    public static String getPreservationTreatment(List<ArrayItem> items) {
        NoteItem ni = getPreservationItem(items, PRESERVATION_TREATMENT);
        return ni != null ? ni.getValue() : null;
    }

    public static String getPreservationStateOfArt(List<ArrayItem> items) {
        NoteItem ni = getPreservationItem(items, PRESERVATION_STATE_OF_ART);
        return ni != null ? ni.getValue() : null;
    }

    private static NoteItem getPreservationItem(List<ArrayItem> items, String type) {
        for (ArrayItem item : items) {
            if (item instanceof NoteItem && type.equals(((NoteItem) item).getType())) {
                return (NoteItem) item;
            }
        }
        return null;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ExtentPair {
        
        @XmlElement(name = ModsConstants.FIELD_PHYSICAL_DESCRIPTIONS_EXTENT)
        private String extent;
        private Integer extentIndex;
        @XmlElement(name = ModsConstants.FIELD_PHYSICAL_DESCRIPTIONS_SIZE)
        private String size;
        private Integer sizeIndex;

        public ExtentPair() {
        }

        public ExtentPair(String extent, Integer extentIndex, String size, Integer sizeIndex) {
            this.extent = extent;
            this.extentIndex = extentIndex;
            this.size = size;
            this.sizeIndex = sizeIndex;
        }

        public String getExtent() {
            return extent;
        }

        public void setExtent(String extent) {
            this.extent = extent;
        }

        public Integer getExtentIndex() {
            return extentIndex;
        }

        public void setExtentIndex(Integer extentIndex) {
            this.extentIndex = extentIndex;
        }

        public String getSize() {
            return size;
        }

        public void setSize(String size) {
            this.size = size;
        }

        public Integer getSizeIndex() {
            return sizeIndex;
        }

        public void setSizeIndex(Integer sizeIndex) {
            this.sizeIndex = sizeIndex;
        }

        @Override
        public String toString() {
            return String.format("ExtentPair{extent: %s, extentIndex: %s, size: %s, sizeIndex: %s}",
                    extent, extentIndex, size, sizeIndex);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final ExtentPair other = (ExtentPair) obj;
            if ((this.extent == null) ? (other.extent != null) : !this.extent.equals(other.extent)) {
                return false;
            }
            if (this.extentIndex != other.extentIndex && (this.extentIndex == null || !this.extentIndex.equals(other.extentIndex))) {
                return false;
            }
            if ((this.size == null) ? (other.size != null) : !this.size.equals(other.size)) {
                return false;
            }
            if (this.sizeIndex != other.sizeIndex && (this.sizeIndex == null || !this.sizeIndex.equals(other.sizeIndex))) {
                return false;
            }
            return true;
        }

    }

    public static <T extends ArrayItem> List<T> filter(List<T> list, boolean include, Class<? extends ArrayItem>... types) {
        ArrayList<T> result = new ArrayList<>();
        for (T t : list) {
            boolean equals = false;
            for (Class<? extends ArrayItem> type : types) {
                equals = type == t.getClass();
                if (equals) {
                    break;
                }
            }
            if (include == true && include == equals || include == false && include == equals) {
                result.add(t);
            }
        }
        return result;
    }

    private static final class PhysicalDescriptionItemMapper implements ItemMapper<Object, ArrayItem> {

        private final ObjectFactory factory = new ObjectFactory();

        @Override
        public ArrayItem map(Object source) {
            if (source instanceof Extent) {
                ExtentItem result = new ExtentItem();
                result.setValue(((Extent) source).getValue());
                return result;
            } else if (source instanceof PhysicalDescriptionNote) {
                PhysicalDescriptionNote note = (PhysicalDescriptionNote) source;
                return new NoteItem(null, note.getValue(), note.getType());
            } else {
                return new UnkownItem();
            }
        }

        @Override
        public Object map(ArrayItem item, Object origin) {
            Object source = origin;
            if (origin == null) {
                if (item instanceof ExtentItem) {
                    source = factory.createExtent();
                } else if (item instanceof NoteItem) {
                    NoteItem noteItem = (NoteItem) item;
                    PhysicalDescriptionNote note = factory.createPhysicalDescriptionNote();
                    note.setType(noteItem.getType());
                    source = note;
                } else {
                    throw new IllegalStateException("unsupported array item: " + item.getClass());
                }
            }

            if (item instanceof ExtentItem) {
                ExtentItem extentItem = (ExtentItem) item;
                Extent extentSource = (Extent) source;
                // delete with empty string to prevent XML nil
                extentSource.setValue(extentItem.getValue() != null ? extentItem.getValue() : "");
            } else if (item instanceof NoteItem && !((NoteItem) item).ignore) {
                NoteItem noteItem = (NoteItem) item;
                PhysicalDescriptionNote noteType = (PhysicalDescriptionNote) source;
                noteType.setValue(noteItem.getValue());
            }
            return source;
        }

    }

    static final class UnkownItem implements ArrayItem {

        private Integer index;

        public UnkownItem() {
        }

        public UnkownItem(Integer index) {
            this.index = index;
        }

        @Override
        public Integer getArrayIndex() {
            return index;
        }

        @Override
        public void setArrayIndex(Integer index) {
            this.index = index;
        }

        @Override
        public String toString() {
            return String.format("UnkownItem{%s}", index);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final UnkownItem other = (UnkownItem) obj;
            return Objects.equals(this.index, other.index);
        }

    }

    public static class ExtentItem implements ArrayItem {

        private Integer index;
        private String value;

        public ExtentItem() {
        }

        public ExtentItem(Integer index, String value) {
            this.index = index;
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = MapperUtils.normalize(value);
        }

        @Override
        public Integer getArrayIndex() {
            return index;
        }

        @Override
        public void setArrayIndex(Integer index) {
            this.index = index;
        }

        @Override
        public String toString() {
            return String.format("ExtentItem{index: %s, value: %s}", index, value);
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final ExtentItem other = (ExtentItem) obj;
            return Objects.equals(this.value, other.value) && Objects.equals(this.index, other.index);
        }

    }

    public static class NoteItem implements ArrayItem {

        private Integer index;
        private String value;
        private String type;
        private boolean ignore = true;

        public NoteItem() {
        }

        public NoteItem(Integer index, String value, String type) {
            this.index = index;
            this.value = value;
            this.type = type;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }

        @Override
        public Integer getArrayIndex() {
            return index;
        }

        @Override
        public void setArrayIndex(Integer index) {
            this.index = index;
        }

        @Override
        public String toString() {
            return "NoteItem{" + "index=" + index + ", value=" + value + ", type=" + type + ", ignore=" + ignore + '}';
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final NoteItem other = (NoteItem) obj;
            return Objects.equals(this.value, other.value)
                    && Objects.equals(this.type, other.type)
                    && Objects.equals(this.index, other.index);
        }

    }

}
