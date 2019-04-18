package cz.cas.lib.proarc.audiopremis;


import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

@XmlRegistry
public class AudioObjectFactory {
    private final static QName _NK = new QName("info:ndk/xmlns/nk-v1", "nk");
    private final static QName _MANUFACTURER = new QName("info:ndk/xmlns/nk-v1", "manufacturer");
    private final static QName _SERIAL_NUMBER = new QName("info:ndk/xmlns/nk-v1", "serialNumber");
    private final static QName _SETTINGS = new QName("info:ndk/xmlns/nk-v1", "settings");

    public AudioObjectFactory() {
    }

    @XmlElementDecl(namespace = "info:ndk/xmlns/nk-v1", name = "nkSerialNumber")
    public JAXBElement<NkComplexType> createNk(NkComplexType value) {
        return new JAXBElement<NkComplexType>(_NK, NkComplexType.class, null, value);
    }

    @XmlElementDecl(namespace = "info:ndk/xmlns/nk-v1", name = "manufacturer")
    public JAXBElement<String> createManufacturer(String value) {
        return new JAXBElement<String>(_MANUFACTURER, String.class, null, value);
    }

    @XmlElementDecl(namespace = "info:ndk/xmlns/nk-v1", name = "serialNumber")
    public JAXBElement<String> createSerialNumber(String value) {
        return new JAXBElement<String>(_SERIAL_NUMBER, String.class, null, value);
    }

    @XmlElementDecl(namespace = "info:ndk/xmlns/nk-v1", name = "settings")
    public JAXBElement<String> createSettings(String value) {
        return new JAXBElement<String>(_SETTINGS, String.class, null, value);
    }

    public NkComplexType createNkComplexType(){
        return new NkComplexType();
    }
}
