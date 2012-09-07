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

import cz.fi.muni.xkremser.editor.server.mods.AbstractType;
import cz.fi.muni.xkremser.editor.server.mods.AccessConditionType;
import cz.fi.muni.xkremser.editor.server.mods.BaseDateType;
import cz.fi.muni.xkremser.editor.server.mods.ClassificationType;
import cz.fi.muni.xkremser.editor.server.mods.DetailType;
import cz.fi.muni.xkremser.editor.server.mods.ExtensionType;
import cz.fi.muni.xkremser.editor.server.mods.ExtentType;
import cz.fi.muni.xkremser.editor.server.mods.GenreType;
import cz.fi.muni.xkremser.editor.server.mods.IdentifierType;
import cz.fi.muni.xkremser.editor.server.mods.LanguageType;
import cz.fi.muni.xkremser.editor.server.mods.LocationType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.NameType;
import cz.fi.muni.xkremser.editor.server.mods.NoteType;
import cz.fi.muni.xkremser.editor.server.mods.OriginInfoType;
import cz.fi.muni.xkremser.editor.server.mods.PartType;
import cz.fi.muni.xkremser.editor.server.mods.PhysicalDescriptionType;
import cz.fi.muni.xkremser.editor.server.mods.RecordInfoType;
import cz.fi.muni.xkremser.editor.server.mods.RelatedItemType;
import cz.fi.muni.xkremser.editor.server.mods.SubjectType;
import cz.fi.muni.xkremser.editor.server.mods.TableOfContentsType;
import cz.fi.muni.xkremser.editor.server.mods.TargetAudienceType;
import cz.fi.muni.xkremser.editor.server.mods.TitleInfoType;
import cz.fi.muni.xkremser.editor.server.mods.TypeOfResourceType;
import cz.fi.muni.xkremser.editor.server.mods.UnstructuredText;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

/**
 *
 * @author Jan Pokorsky
 */
public final class MapperUtils {

    private static final ClassComparator MODS_GROUP = new ClassComparator(
            TitleInfoType.class,
            NameType.class,
            TypeOfResourceType.class,
            GenreType.class,
            OriginInfoType.class,
            LanguageType.class,
            PhysicalDescriptionType.class,
            AbstractType.class,
            TableOfContentsType.class,
            TargetAudienceType.class,
            NoteType.class,
            SubjectType.class,
            ClassificationType.class,
            RelatedItemType.class,
            IdentifierType.class,
            LocationType.class,
            AccessConditionType.class,
            PartType.class,
            ExtensionType.class,
            RecordInfoType.class
        );

    private static final ClassComparator MODS_PART_GROUP = new ClassComparator(
            DetailType.class,
            ExtentType.class,
            BaseDateType.class,
            UnstructuredText.class
        );

    public static String normalize(String s) {
        if (s != null) {
            s = s.trim();
        }
        return s == null || s.isEmpty() ? null : s;
    }

    public static String toString(JAXBElement<String> textElement) {
        return textElement == null ? null : textElement.getValue();
    }

    public static boolean isEmpty(Collection<?> l) {
        return l == null || l.isEmpty();
    }

    public static List<JAXBElement<String>> toJaxb(List<String> values, QName name) {
        ArrayList<JAXBElement<String>> result = new ArrayList<JAXBElement<String>>(values.size());
        for (String value : values) {
            result.add( new JAXBElement<String>(name, String.class, value));
        }
        return result;
    }

    public static  <T> T findFirst(List<?> list, Class<T> type) {
        Iterator<T> it = find(list, type).iterator();
        return it.hasNext() ? it.next() : null;
    }

    public static <T> List<T> find(List<?> list, Class<T> type) {
        List<T> result = new ArrayList<T>();
        for (Object item : list) {
            if (type.isInstance(item)) {
                result.add(type.cast(item));
            }
        }
        return result;
    }

    public static <T> T findFirst(List<JAXBElement<?>> elms, Class<T> type, QName... names) {
        JAXBElement<?> elm = findFirst(elms, names);
        return elm == null ? null : type.cast(elm.getValue());
    }
    
    public static JAXBElement<?> findFirst(List<JAXBElement<?>> elms, QName... names) {
        for (JAXBElement<?> elm : elms) {
            QName elmName = elm.getName();
            for (QName name : names) {
                if (elmName.equals(name)) {
                    return elm;
                }
            }
        }
        return null;
    }

    public static <T> List<T> find(List<JAXBElement<?>> elms, Class<T> type, QName name) {
        List<T> result = new ArrayList<T>();
        for (JAXBElement<?> elm : elms) {
            QName elmName = elm.getName();
            if (elmName.equals(name)) {
                result.add(type.cast(elm.getValue()));
            }
        }
        return result;
    }

    public static List<JAXBElement<?>> findAny(List<JAXBElement<?>> elms, QName... names) {
        List<JAXBElement<?>> result = new ArrayList<JAXBElement<?>>();
        for (JAXBElement<?> elm : elms) {
            QName elmName = elm.getName();
            for (QName name : names) {
                if (elmName.equals(name)) {
                    result.add(elm);
                }
            }
        }
        return result;
    }
    
    public static <T> List<JAXBElement<T>> find(List<JAXBElement<T>> elms, QName... names) {
        List<JAXBElement<T>> result = new ArrayList<JAXBElement<T>>();
        for (JAXBElement<T> elm : elms) {
            QName elmName = elm.getName();
            for (QName name : names) {
                if (elmName.equals(name)) {
                    result.add(elm);
                }
            }
        }
        return result;
    }

    public static <T> T findFirst(List<T> list, Selector<T> selector) {
        for (T item : list) {
            if (selector.select(item)) {
                return item;
            }
        }
        return null;
    }

    public static <T> List<T> find(List<T> list, Selector<T> selector) {
        List<T> result = new ArrayList<T>();
        for (T item : list) {
            if (selector.select(item)) {
                result.add(item);
            }
        }
        return result;
    }

    public static <T> Selector<JAXBElement<T>> jaxbElementSelector(final QName name) {
        assert name != null;
        return new Selector<JAXBElement<T>>() {

            @Override
            public boolean select(JAXBElement<T> item) {
                return name.equals(item.getName());
            }
        };
    }

    public static Selector<DetailType> detailSelector(final String type) {
        return new Selector<DetailType>() {

            @Override
            public boolean select(DetailType item) {
                return type.equals(item.getType());
            }
        };
    }

    public static void remove(List<?> list, Class<?> type) {
        for (Iterator<?> it = list.iterator(); it.hasNext();) {
            Object item = it.next();
            if (type.isInstance(item)) {
                it.remove();
            }
        }
    }

    /**
     * Updates list with {@code updates} collection with respect to ModsType subelements order
     */
    public static <T> void update(List<Object> list, List<T> updates, Class<T> updateType) {
        remove(list, updateType);
        int index = 0;
        for (Object item : list) {
            if (MODS_GROUP.compare(updateType, item.getClass()) < 0) {
                break;
            }
            ++index;
        }
        list.addAll(index, updates);
    }

    public static <T> List<T> mergeList(List<? extends T> list1, List<? extends T> list2, List<? extends T>... rest) {
        int size = list1.size() + list2.size();
        for (List<? extends T> l : rest) {
            size += l.size();
        }
        List<T> result = new ArrayList<T>(size);
        result.addAll(list1);
        result.addAll(list2);
        for (List<? extends T> l : rest) {
            result.addAll(l);
        }
        return result;
    }

    public static void add(PartType part, Object o) {
        List<Object> group = part.getDetailOrExtentOrDate();
        int index = indexAfterLastClass(group, o, MODS_PART_GROUP);
        group.add(index, o);
    }
    
    public static void add(ModsType mods, Object o) {
        List<Object> group = mods.getModsGroup();
        int index = indexAfterLastClass(group, o, MODS_GROUP);
        group.add(index, o);
    }

    private static int indexAfterLastClass(List<Object> group, Object o, ClassComparator cmp) {
        int index = 0;
        for (Object item : group) {
            if (cmp.compare(o.getClass(), item.getClass()) < 0) {
                break;
            }
            ++index;
        }
        return index;
    }

    /**
     * Puts elements to expected order. It should be used for newly imported MODS
     * to ensure minimal modifications of later edited MODS.
     */
    public static void normalize(ModsType mods) {
        List<Object> modsGroup = mods.getModsGroup();

        Map<Class<?>, List<Object>> types = createTypesMap(MODS_GROUP.classesMap.keySet());
        types.put(TitleInfoType.class, null);
        for (Object item : modsGroup) {
            List<Object> items = types.get(item.getClass());
            if (items == null) {
                items = new ArrayList<Object>();
                types.put(item.getClass(), items);
            }
            items.add(item);
        }
        modsGroup.clear();
        for (Map.Entry<Class<?>, List<Object>> entry : types.entrySet()) {
            List<Object> list = entry.getValue();
            if (list != null) {
                modsGroup.addAll(list);
            }
        }
    }

    public static <T> List<T> noNull(List<T> list) {
        return noNull(list, false);
    }
    
    public static <T> List<T> noNull(List<T> list, boolean liveList) {
        return list != null
                ? list
                : liveList ? new ArrayList<T>() : Collections.<T>emptyList();
    }

    public static <T extends Collection<S>, S> T asNull(T collection) {
        return collection == null || collection.isEmpty() ? null : collection;
    }

    private static final class ClassComparator implements Comparator<Class<?>> {

//        private final Class[] classes;
        private final Map<Class<?>, Integer> classesMap;

        public ClassComparator(Class<?>... classes) {
//            this.classes = classes;
            this.classesMap = new LinkedHashMap<Class<?>, Integer>();
            for (int i = 0; i < classes.length; i++) {
                classesMap.put(classes[i], i);
            }
        }

        @Override
        public int compare(Class<?> o1, Class<?> o2) {
            if (o1 == o2) {
                return 0;
            } else {
                return indexOf(o1) - indexOf(o2);
            }
        }

        private int indexOf(Class<?> c) {
            Integer index = classesMap.get(c);
            if (index == null) {
                throw new IllegalStateException(String.valueOf(c));
            } else {
                return index;
            }
        }

    }

    private static Map<Class<?>, List<Object>> createTypesMap(Collection<Class<?>> types) {
        Map<Class<?>, List<Object>> result = new LinkedHashMap<Class<?>, List<Object>>();
        for (Class<?> type : types) {
            result.put(type, null);
        }
        return result;
    }

    public interface Selector<T> {
        boolean select(T item);
    }


}
