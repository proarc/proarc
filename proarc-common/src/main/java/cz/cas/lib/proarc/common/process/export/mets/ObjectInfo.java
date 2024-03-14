package cz.cas.lib.proarc.common.process.export.mets;

import edu.harvard.hul.ois.xml.ns.jhove.PropertyType;
import edu.harvard.hul.ois.xml.ns.jhove.ValuesType;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.Set;

public class ObjectInfo {

    private String applicationName;
    private String applicationVersion;
    private String applicationCreationDate;
    private String objectCount;
    private BigInteger pageCount;
//    private String tableCount;
//    private String graphicCount;
    private BigInteger imageCount;
    private String language;
    private Set<String> fonts;
    private Set<String> filters;

    public ObjectInfo() {
    }

    public void createObjectInfoFromOutput(JHoveOutput output) {
        if (output != null && output.getBasicObjectInfo() != null) {
            for (ValuesType rootValues : output.getBasicObjectInfo().getValues()) {
                for (PropertyType rootProperty : rootValues.getProperty()) {
                    if ("Info".equals(rootProperty.getName())) {
                        if (!rootProperty.getValues().isEmpty()) {
                            for (PropertyType property : rootProperty.getValues().get(0).getProperty()) {
                                if ("Creator".equals(property.getName())) {
                                    this.applicationName = getPropertyValue(property);
                                }
                                if ("Producer".equals(property.getName())) {
                                    this.applicationVersion = getPropertyValue(property);
                                }
                                if ("CreationDate".equals(property.getName())) {
                                    this.applicationCreationDate = getPropertyValue(property);
                                }
                            }
                        }
                    }
                    if ("Objects".equals(rootProperty.getName())) {
                        this.objectCount = getPropertyValue(rootProperty);
                    }
                    if ("Pages".equals(rootProperty.getName())) {
                        this.pageCount = getPropertyCount(rootProperty);
                    }
                    if ("Images".equals(rootProperty.getName())) {
                        this.imageCount = getPropertyCount(rootProperty);
                    }
                    if ("DocumentCatalog".equals(rootProperty.getName())) {
                        if (!rootProperty.getValues().isEmpty()) {
                            for (PropertyType property : rootProperty.getValues().get(0).getProperty()) {
                                if ("Language".equals(property.getName())) {
                                    this.language = getPropertyValue(property);
                                }
                            }
                        }
                    }
                    if ("Fonts".equals(rootProperty.getName())) {
                        this.fonts = new HashSet<String>();
                        for (ValuesType typeValue : rootProperty.getValues()) {
                            for (PropertyType typeProperty : typeValue.getProperty()) {
                                for (ValuesType fontValue : typeProperty.getValues()) {
                                    for (PropertyType fontProporty : fontValue.getProperty()) {
                                        if ("Font".equals(fontProporty.getName())) {
                                            for (ValuesType fontsValue : fontProporty.getValues()) {
                                                for (PropertyType fontsProperty : fontsValue.getProperty()) {
                                                    if ("BaseFont".equals(fontsProperty.getName())) {
                                                        String value = getPropertyValue(fontsProperty);
                                                        if (value != null && !value.isEmpty()) {
                                                            this.fonts.add(value);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if ("Filters".equals(rootProperty.getName())) {
                        this.filters = new HashSet<String>();
                        for (ValuesType typeValue : rootProperty.getValues()) {
                            for (PropertyType typeProperty : typeValue.getProperty()) {
                                if ("FilterPipeline".equals(typeProperty.getName())) {
                                    String value = getPropertyValue(typeProperty);
                                    if (value != null && !value.isEmpty()) {
                                        this.filters.add(value);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    private BigInteger getPropertyCount(PropertyType property) {
        try {
            int size = property.getValues().get(0).getProperty().size();
            return BigInteger.valueOf(size);
        } catch (Throwable t) {
            return null;
        }
    }

    private String getPropertyValue(PropertyType property) {
        try {
            return property.getValues().get(0).getValue().get(0);
        } catch (Throwable t) {
            return null;
        }
    }

    public String getApplicationName() {
        return applicationName;
    }

    public String getApplicationVersion() {
        return applicationVersion;
    }

    public String getApplicationCreationDate() {
        return applicationCreationDate;
    }

    public String getObjectCount() {
        return objectCount;
    }

    public BigInteger getPageCount() {
        return pageCount;
    }

    public BigInteger getImageCount() {
        return imageCount;
    }

    public String getLanguage() {
        return language;
    }

    public Set<String> getFonts() {
        return fonts;
    }

    public Set<String> getFilters() {
        return filters;
    }
}
