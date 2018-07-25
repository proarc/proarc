package cz.cas.lib.proarc.audiopremis;


import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

@XmlRegistry
public class AudioObjectFactory {
    private final static QName _NK_MANUFACTURER = new QName("info:lc/xmlns/premis-v2", "nkManufacturer");
    private final static QName _NK_SERIAL_NUMBER = new QName("info:lc/xmlns/premis-v2", "nkSerialNumber");
    private final static QName _NK_SETTINGS = new QName("info:lc/xmlns/premis-v2", "nkSettings");

    public AudioObjectFactory() {
    }
    @XmlElementDecl(namespace = "info:lc/xmlns/premis-v2", name = "nkManufacturer")
    public JAXBElement<NkManufacturerComplexType> createNkManufacturer(NkManufacturerComplexType value) {
        return new JAXBElement<NkManufacturerComplexType>(_NK_MANUFACTURER, NkManufacturerComplexType.class, null, value);
    }

    @XmlElementDecl(namespace = "info:lc/xmlns/premis-v2", name = "nkSettings")
    public JAXBElement<NkSettingsComplexType> createNkSetting(NkSettingsComplexType value) {
        return new JAXBElement<NkSettingsComplexType>(_NK_SETTINGS, NkSettingsComplexType.class, null, value);
    }
    @XmlElementDecl(namespace = "info:lc/xmlns/premis-v2", name = "nkSerialNumber")
    public JAXBElement<NkSerialNumberComplexType> createNkSerialNumber(NkSerialNumberComplexType value) {
        return new JAXBElement<NkSerialNumberComplexType>(_NK_SERIAL_NUMBER, NkSerialNumberComplexType.class, null, value);
    }


}
