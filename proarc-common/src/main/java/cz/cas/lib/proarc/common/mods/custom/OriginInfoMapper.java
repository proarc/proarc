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
import cz.cas.lib.proarc.common.mods.custom.OriginInfoMapper.PublisherItem.Role;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.IssuanceDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusSupplied;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Maps OriginInfoType to publishers, printers and frequency item.
 *
 * Kramerius 4 defines frequency as an independent originInfo element.
 * <p/>Aleph defines frequency inside the publisher's originInfo element.
 *
 * <p/>For now the implementation supports only Kramerius 4 layout.
 * 
 * {@code mods/originInfo[@transliteration == null]/frequency}
 *
 * @author Jan Pokorsky
 */
final class OriginInfoMapper {

    /**
     * @see <a href='http://www.loc.gov/standards/mods/userguide/origininfo.html#issuance'>Guidelines for Use</a>
     */
    public static final String ISSUANCE_CONTINUING = IssuanceDefinition.CONTINUING.value();
    public static final String ISSUANCE_MONOGRAPHIC = IssuanceDefinition.MONOGRAPHIC.value();

    private ArrayMapper<OriginInfoDefinition, OriginInfoItem> originsMap =
            new ArrayMapper<>(new OriginInfoItemMapper());

    public List<OriginInfoItem> map(ModsDefinition mods) {
        List<OriginInfoDefinition> origins = mods.getOriginInfo();
        return originsMap.map(origins);
    }

    public ModsDefinition map(ModsDefinition mods, List<PublisherItem> publishers, List<PublisherItem> printers, String issuance) {
        // useful for monographs without frequencies
        return mapImpl(mods, publishers, printers, null, issuance);
    }

    public ModsDefinition map(ModsDefinition mods, List<PublisherItem> publishers, List<PublisherItem> printers, List<String> frequencies, String issuance) {
        return mapImpl(mods, publishers, printers, MapperUtils.noNull(frequencies), issuance);
    }
    
    private ModsDefinition mapImpl(ModsDefinition mods, List<PublisherItem> publishers, List<PublisherItem> printers, List<String> frequencies, String issuance) {
        List<OriginInfoItem> oldItems = map(mods);
        List<PublisherItem> others = filter(oldItems, true, Role.OTHER);
        List<OriginInfoItem> origins = MapperUtils.<OriginInfoItem>mergeList(
                cast(MapperUtils.noNull(publishers), Role.PUBLISHER),
                cast(MapperUtils.noNull(printers), Role.PRINTER),
                others
                );
        if (frequencies != null || issuance != null) {
            PeriodicityItem periodicity = MapperUtils.findFirst(oldItems, PeriodicityItem.class);
            if (periodicity == null) {
                periodicity = new PeriodicityItem();
            }
            periodicity.setFrequencies(frequencies);
            periodicity.setIssuance(issuance);
            periodicity.ignoreMapping = false;
            origins.add(0, periodicity);
        }
        return map(mods, origins);
    }

    public ModsDefinition map(ModsDefinition mods, List<OriginInfoItem> origins) {
        List<OriginInfoDefinition> oldies = mods.getOriginInfo();
        List<OriginInfoDefinition> news = originsMap.map(origins, oldies);
        oldies.clear();
        oldies.addAll(news);
        return mods;
    }

    public static List<PublisherItem> filter(List<? extends OriginInfoItem> origins, boolean include, Role first, Role... rest) {
        return filter(origins, EnumSet.of(first, rest), include);
    }

    public static List<PublisherItem> filter(List<? extends OriginInfoItem> origins, Set<Role> filter, boolean include) {
        List<PublisherItem> result = new ArrayList<>();

        for (OriginInfoItem origin : origins) {
            if (!(origin instanceof PublisherItem)) {
                continue;
            }
            PublisherItem publisher = (PublisherItem) origin;
            boolean contains = filter.contains(publisher.getRole());
            if ((include == true && include == contains) || (include == false && include == contains)) {
                result.add(publisher);
            }
        }
        return result;
    }

    public static List<String> getFreqencies(List<? extends OriginInfoItem> origins) {
        PeriodicityItem item = MapperUtils.findFirst(origins, PeriodicityItem.class);
        return item != null ? item.getFrequencies() : Collections.<String>emptyList();
    }

    public static String getIssuance(List<? extends OriginInfoItem> origins) {
        PeriodicityItem item = MapperUtils.findFirst(origins, PeriodicityItem.class);
        return item != null ? item.getIssuance() : null;
    }

    /**
     * Ensures that items belongs to role.
     */
    private static List<PublisherItem> cast(List<PublisherItem> items, Role role) {
        for (PublisherItem item : items) {
            item.setRole(role);
        }
        return items;
    }

    private static final class OriginInfoItemMapper implements ItemMapper<OriginInfoDefinition, OriginInfoItem> {

        private ObjectFactory factory = new ObjectFactory();

        @Override
        public OriginInfoItem map(OriginInfoDefinition source) {
            String transliteration = source.getTransliteration();
            StringPlusLanguagePlusSupplied publisher = getPublisher(factory, source.getPublisher(), false);
            if (transliteration == null && publisher == null) {
                return readPeriodicity(source);
            } else {
                return readPublisher(source);
            }
        }
        
        private PeriodicityItem readPeriodicity(OriginInfoDefinition source) {
            PeriodicityItem result = new PeriodicityItem();
            List<String> frequencies = source.getFrequency().stream()
                    .map(spl -> spl.getValue())
                    .collect(Collectors.toList());
            Optional<IssuanceDefinition> issuance = source.getIssuance().stream().findFirst();
            result.setIssuance(issuance.isPresent()? issuance.get().value() : null);
            result.setFrequencies(frequencies);
            return result;
        }

        private PublisherItem readPublisher(OriginInfoDefinition source) {
            PublisherItem result = new PublisherItem();
            Role role = Role.fromText(source.getTransliteration());
            result.setRole(role);
            if (role == Role.OTHER) {
                return result;
            }

            Optional<StringPlusLanguagePlusSupplied> publisher = source.getPublisher().stream().findFirst();

            result.setName(publisher.isPresent() ? publisher.get().getValue() : null);

            DateDefinition date = getDate(factory, source, role, false);
            result.setDate(date == null ? null : date.getValue());

            PlaceTermDefinition place = getPlace(factory, source, false);
            result.setPlace(place == null ? null : place.getValue());

            return result;
        }

        @Override
        public OriginInfoDefinition map(OriginInfoItem item, OriginInfoDefinition origin) {
            if (item instanceof PublisherItem) {
                origin = writePublisher((PublisherItem) item, origin);
            } else if (item instanceof PeriodicityItem) {
                origin = writePeriodicity((PeriodicityItem) item, origin);
            }
            return origin;
        }
        
        private OriginInfoDefinition writePeriodicity(PeriodicityItem item, OriginInfoDefinition origin) {
            if (item.ignoreMapping) {
                return origin;
            }
            if (origin == null) {
                origin = factory.createOriginInfoDefinition();
            }
            origin.getFrequency().clear();
            origin.getIssuance().clear();
            for (String frequency : MapperUtils.noNull(item.getFrequencies())) {
                StringPlusLanguagePlusAuthority spa = factory.createStringPlusLanguagePlusAuthority();
                spa.setValue(frequency);
                origin.getFrequency().add(spa);
            }
            origin.getIssuance().add(IssuanceDefinition.fromValue(item.getIssuance()));
            return origin;
        }

        private OriginInfoDefinition writePublisher(PublisherItem item, OriginInfoDefinition origin) {
            OriginInfoDefinition source = origin;
            Role role = item.getRole();
            if (role == Role.OTHER) {
                return origin;
            }
            if (origin == null) {
                source = factory.createOriginInfoDefinition();
                source.setTransliteration(role.getText());
            }

            if (item.getName() != null) {
                StringPlusLanguagePlusSupplied publisher = getPublisher(factory, source.getPublisher(), true);
                publisher.setValue(item.getName());
            } else {
                StringPlusLanguagePlusSupplied publisher = getPublisher(factory, source.getPublisher(), false);
                source.getPublisher().remove(publisher);
            }

            if (item.getPlace() != null) {
                getPlace(factory, source, true).setValue(item.getPlace());
            } else {
                PlaceTermDefinition place = getPlace(factory, source, false);
                if (place != null) {
                    place.setValue(null);
                }
            }

            if (item.getDate() != null) {
                getDate(factory, source, role, true).setValue(item.getDate());
            } else {
                DateDefinition date = getDate(factory, source, role, false);
                if (date != null) {
                    date.setValue(null);
                }
            }

            return source;
        }

        private static StringPlusLanguagePlusSupplied getPublisher(ObjectFactory factory, List<StringPlusLanguagePlusSupplied> publishers, boolean create) {
            StringPlusLanguagePlusSupplied publisher = publishers.stream().findFirst().orElse(null);
            if (create && publisher == null) {
                publisher = factory.createStringPlusLanguagePlusSupplied();
                publishers.add(publisher);
            }
            return publisher;
        }

        private static DateDefinition getDate(ObjectFactory factory, OriginInfoDefinition oid, Role role, boolean create) {
            List<DateDefinition> dates = role == Role.PRINTER ? oid.getDateCreated() : oid.getDateIssued();
            DateDefinition date = dates.stream().findFirst().orElse(null);
            if (create && date == null) {
                date = factory.createDateDefinition();
                dates.add(date);
            }
            return date;
        }

        private static PlaceTermDefinition getPlace(ObjectFactory factory, OriginInfoDefinition oid, boolean create) {
            PlaceDefinition place = oid.getPlace().stream().findFirst().orElse(null);
            if (place == null) {
                if (create) {
                    place = factory.createPlaceDefinition();
                    oid.getPlace().add(place);
                } else {
                    return null;
                }
            }

            if (place.getPlaceTerm().isEmpty()) {
                if (create) {
                    place.getPlaceTerm().add(factory.createPlaceTermDefinition());
                } else {
                    return null;
                }
            }
            return place.getPlaceTerm().get(0);
        }

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static final class PublisherItem implements OriginInfoItem {

        @XmlElement(name = ModsConstants.FIELD_PRINTER_PUBLISHER_NAME)
        private String name;
        @XmlElement(name = ModsConstants.FIELD_PRINTER_PUBLISHER_DATE)
        private String date;
        @XmlElement(name = ModsConstants.FIELD_PRINTER_PUBLISHER_PLACE)
        private String place;
        private transient Role role;
        private Integer index;

        public PublisherItem() {
        }

        PublisherItem(Integer index, Role role, String name, String date, String place) {
            this.name = name;
            this.date = date;
            this.place = place;
            this.role = role;
            this.index = index;
        }

        public String getDate() {
            return date;
        }

        public void setDate(String date) {
            this.date = MapperUtils.normalize(date);
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = MapperUtils.normalize(name);
        }

        public String getPlace() {
            return place;
        }

        public void setPlace(String place) {
            this.place = MapperUtils.normalize(place);
        }

        public Role getRole() {
            return role;
        }

        public void setRole(Role role) {
            this.role = role;
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
            return String.format("PublisherItem{index: %s, name: %s, date: %s, place: %s, role: %s}",
                    index, name, date, place, role);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final PublisherItem other = (PublisherItem) obj;
            if ((this.name == null) ? (other.name != null) : !this.name.equals(other.name)) {
                return false;
            }
            if ((this.date == null) ? (other.date != null) : !this.date.equals(other.date)) {
                return false;
            }
            if ((this.place == null) ? (other.place != null) : !this.place.equals(other.place)) {
                return false;
            }
            if (this.role != other.role) {
                return false;
            }
            if (this.index != other.index && (this.index == null || !this.index.equals(other.index))) {
                return false;
            }
            return true;
        }

        public enum Role {

            PUBLISHER("publisher"), PRINTER("printer"), OTHER("PublisherMapper.Role.OTHER");

            private String text;

            private Role(String text) {
                this.text = text;
            }

            public String getText() {
                return text;
            }

            public static Role fromText(String s) {
                if (s == null) {
                    // default
                    return PUBLISHER;
                }
                for (Role role : values()) {
                    if (role.getText().equals(s)) {
                        return role;
                    }
                }
                return OTHER;
            }
        }
    }

    static final class PeriodicityItem implements OriginInfoItem {
        // transliteration == null
        private List<String> frequencies;
        private String issuance;
        private Integer index;
        private boolean ignoreMapping = true;

        public PeriodicityItem() {
        }

        public PeriodicityItem(Integer index, List<String> frequencies, String issuance) {
            this.frequencies = frequencies;
            this.index = index;
            this.issuance = issuance;
        }

        @Override
        public Integer getArrayIndex() {
            return index;
        }

        @Override
        public void setArrayIndex(Integer index) {
            this.index = index;
        }

        public List<String> getFrequencies() {
            return frequencies;
        }

        public void setFrequencies(List<String> frequencies) {
            this.frequencies = frequencies;
        }

        public String getIssuance() {
            return issuance;
        }

        public void setIssuance(String issuance) {
            this.issuance = issuance;
        }

        @Override
        public String toString() {
            return String.format("PeriodicityItem{index: %s, frequencies: %s, issuance: %s}", index, frequencies, issuance);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final PeriodicityItem other = (PeriodicityItem) obj;
            if (this.frequencies != other.frequencies && (this.frequencies == null || !this.frequencies.equals(other.frequencies))) {
                return false;
            }
            if ((this.issuance == null) ? (other.issuance != null) : !this.issuance.equals(other.issuance)) {
                return false;
            }
            if (this.index != other.index && (this.index == null || !this.index.equals(other.index))) {
                return false;
            }
            return true;
        }

    }

    public interface OriginInfoItem extends ArrayItem {
    }

}
