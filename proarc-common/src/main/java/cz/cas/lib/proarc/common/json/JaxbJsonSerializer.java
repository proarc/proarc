package cz.cas.lib.proarc.common.json;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

public class JaxbJsonSerializer<T> extends JsonSerializer<T> {

//    @Override
//    public void serialize(T value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
//        if (value == null) {
//            gen.writeNull();
//            return;
//        }
//
//        try {
//            // JAXB → XML
//            JAXBContext context = JAXBContext.newInstance(value.getClass());
//            Marshaller marshaller = context.createMarshaller();
//
//            StringWriter writer = new StringWriter();
//            marshaller.marshal(value, writer);
//            String xmlString = writer.toString();
//
//            // XML → JsonNode
//            XmlMapper xmlMapper = new XmlMapper();
//            JsonNode root = xmlMapper.readTree(xmlString);
//
//            // → obecně: pokud root je ObjectNode a má jen jeden child, použijeme jeho obsah
//            if (root.isObject() && root.size() == 1) {
//                Iterator<Map.Entry<String, JsonNode>> fields = root.fields();
//                if (fields.hasNext()) {
//                    JsonNode firstChild = fields.next().getValue();
//
//                    // vytvoříme nový JsonNode wrapper s požadovaným názvem
//                    XmlMapper mapper = new XmlMapper(); // nebo ObjectMapper
//                    JsonNode wrapperNode = mapper.createObjectNode().set("ImageCaptureMetadata", firstChild);
//
//                    gen.writeTree(wrapperNode);
//                    return;
//                }
//            }
//
//            // fallback – pokud není ObjectNode s jedním childem, použij root
//            gen.writeTree(root);
//
//        } catch (JAXBException e) {
//            throw new IOException("Failed to serialize JAXB object to JSON", e);
//        }
//    }

//    private final String rootName;
//
//    public JaxbJsonSerializer(String rootName) {
//        this.rootName = rootName;
//    }
//
//    @Override
//    public void serialize(T value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
//        if (value == null) {
//            gen.writeNull();
//            return;
//        }
//
//        // obalíme JAXB objekt do Mapy s požadovaným root elementem
//        Map<String, Object> wrapper = new LinkedHashMap<>();
//        wrapper.put(rootName, value);
//
//        // použijeme ObjectMapper s JAXB modulem z JsonUtils
//        JsonUtils.defaultObjectMapper().writeValue(gen, wrapper);
//    }

    private final String rootName;
    private final ObjectMapper mapper;

    public JaxbJsonSerializer(String rootName, ObjectMapper mapper) {
        this.rootName = rootName;
        this.mapper = mapper;
    }

    @Override
    public void serialize(T value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        if (value == null) {
            gen.writeNull();
            return;
        }

        Map<String, Object> wrapper = new LinkedHashMap<>();
        wrapper.put(rootName, value);

        // Serializace přes ObjectMapper s podporou JAXB anotací
        mapper.writeValue(gen, wrapper);
    }
}
