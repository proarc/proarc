package cz.cas.lib.proarc.common.process.export.mets;

import edu.harvard.hul.ois.xml.ns.jhove.PropertyType;
import edu.harvard.hul.ois.xml.ns.jhove.ValuesType;

public class ObjectInfo {

    private String applicationName;
    private String applicationVersion;
    private String applicationCreationDate;

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
                                    this.applicationName = property.getValues().get(0).getValue().get(0);
                                }
                                if ("Producer".equals(property.getName())) {
                                    this.applicationVersion = property.getValues().get(0).getValue().get(0);
                                }
                                if ("CreationDate".equals(property.getName())) {
                                    this.applicationCreationDate = property.getValues().get(0).getValue().get(0);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    public String getApplicationName() {
        return applicationName;
    }

    public void setApplicationName(String applicationName) {
        this.applicationName = applicationName;
    }

    public String getApplicationVersion() {
        return applicationVersion;
    }

    public void setApplicationVersion(String applicationVersion) {
        this.applicationVersion = applicationVersion;
    }

    public String getApplicationCreationDate() {
        return applicationCreationDate;
    }

    public void setApplicationCreationDate(String applicationCreationDate) {
        this.applicationCreationDate = applicationCreationDate;
    }
}
