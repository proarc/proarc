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

import cz.cas.lib.proarc.mods.ObjectFactory;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import jakarta.xml.bind.JAXBElement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import javax.xml.namespace.QName;

/**
 *
 * @author Jan Pokorsky
 */
public final class MapperUtils {

    public static String normalize(String s) {
        if (s != null) {
            s = s.trim();
        }
        return s == null || s.isEmpty() ? null : s;
    }

    public static String toString(StringPlusLanguage textElement) {
        return textElement == null ? null : textElement.getValue();
    }

    public static String toString(JAXBElement<String> textElement) {
        return textElement == null ? null : textElement.getValue();
    }

    public static boolean isEmpty(Collection<?> l) {
        return l == null || l.isEmpty();
    }

    public static List<StringPlusLanguage> toStringPlusLanguage(List<String> strings) {
        ObjectFactory factory = new ObjectFactory();
        return strings.stream().map(s -> {
            StringPlusLanguage spl = factory.createStringPlusLanguage();
            spl.setValue(s);
            return spl;
        }).collect(Collectors.toList());
    }

    public static List<StringPlusLanguagePlusAuthority> toStringPlusLanguagePlusAuthority(List<String> strings) {
        ObjectFactory factory = new ObjectFactory();
        return strings.stream().map(s -> {
            StringPlusLanguagePlusAuthority spl = factory.createStringPlusLanguagePlusAuthority();
            spl.setValue(s);
            return spl;
        }).collect(Collectors.toList());
    }

    public static List<String> toStringPlusLanguageValue(List<? extends StringPlusLanguage> spls) {
        return spls.stream().map(spl -> spl.getValue()).collect(Collectors.toList());
    }

    public static List<JAXBElement<String>> toJaxb(List<String> values, QName name) {
        ArrayList<JAXBElement<String>> result = new ArrayList<>(values.size());
        for (String value : values) {
            result.add(new JAXBElement<>(name, String.class, value));
        }
        return result;
    }

    public static <T> T findFirst(List<?> list, Class<T> type) {
        Iterator<T> it = find(list, type).iterator();
        return it.hasNext() ? it.next() : null;
    }

    public static <T> List<T> find(List<?> list, Class<T> type) {
        List<T> result = new ArrayList<>();
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
        List<T> result = new ArrayList<>();
        for (JAXBElement<?> elm : elms) {
            QName elmName = elm.getName();
            if (elmName.equals(name)) {
                result.add(type.cast(elm.getValue()));
            }
        }
        return result;
    }

    public static List<JAXBElement<?>> findAny(List<JAXBElement<?>> elms, QName... names) {
        List<JAXBElement<?>> result = new ArrayList<>();
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
        List<JAXBElement<T>> result = new ArrayList<>();
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
        List<T> result = new ArrayList<>();
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

    public static void remove(List<?> list, Class<?> type) {
        for (Iterator<?> it = list.iterator(); it.hasNext(); ) {
            Object item = it.next();
            if (type.isInstance(item)) {
                it.remove();
            }
        }
    }

    public static <T> List<T> mergeList(List<? extends T> list1, List<? extends T> list2, List<? extends T>... rest) {
        int size = list1.size() + list2.size();
        for (List<? extends T> l : rest) {
            size += l.size();
        }
        List<T> result = new ArrayList<>(size);
        result.addAll(list1);
        result.addAll(list2);
        for (List<? extends T> l : rest) {
            result.addAll(l);
        }
        return result;
    }

    public static <T> List<T> noNull(List<T> list) {
        return noNull(list, false);
    }

    public static <T> List<T> noNull(List<T> list, boolean liveList) {
        return list != null
                ? list
                : liveList ? new ArrayList<>() : Collections.emptyList();
    }

    public static <T extends Collection<S>, S> T asNull(T collection) {
        return collection == null || collection.isEmpty() ? null : collection;
    }

    public interface Selector<T> {
        boolean select(T item);
    }

}
