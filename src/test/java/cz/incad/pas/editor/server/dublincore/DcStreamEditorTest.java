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
package cz.incad.pas.editor.server.dublincore;

import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.incad.pas.editor.server.fedora.LocalStorage;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
import cz.incad.pas.editor.server.mods.ModsUtils;
import cz.incad.pas.oaidublincore.OaiDcType;
import java.io.StringWriter;
import java.util.HashMap;
import javax.xml.bind.util.JAXBSource;
import javax.xml.transform.Transformer;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.custommonkey.xmlunit.SimpleNamespaceContext;
import org.custommonkey.xmlunit.XMLAssert;
import org.custommonkey.xmlunit.XMLUnit;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class DcStreamEditorTest {

    @Test
    public void testWriteDc() throws Exception {
        LocalStorage storage = new LocalStorage();
        LocalStorage.LocalObject local = storage.create();
        DcStreamEditor instance = new DcStreamEditor(local);
        DublinCoreRecord dcRecord = new DublinCoreRecord(new OaiDcType(), 0, local.getPid());
        instance.write(dcRecord);
        local.flush();

        // validate DC
//        String dc = DcUtils.toXml(dcRecord.getDc(), true);
//        System.out.println(dc);
//        SchemaFactory sfactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
//        URL dcXsdUrl = ObjectFactory.class.getResource("dc_oai.xsd");
//        assertNotNull(dcXsdUrl);
//        Schema dcXsd = sfactory.newSchema(dcXsdUrl);
//        dcXsd.newValidator().validate(new StreamSource(dc));

        DublinCoreRecord result = instance.read();
        assertNotNull(result);
        assertNotNull(result.getDc());
        assertEquals(local.getPid(), result.getPid());
    }

    @Test
    public void testWriteMods() throws Exception {
        LocalStorage storage = new LocalStorage();
        LocalStorage.LocalObject local = storage.create();
        ModsStreamEditor modsEditor = new ModsStreamEditor(local);
        ModsType mods = modsEditor.createPage(local.getPid(), "1", "[1]", "pageType");
        String model = "model:page";
        long timestamp = 0L;
        DcStreamEditor instance = new DcStreamEditor(local);
        instance.write(mods, model, timestamp);
        local.flush();

        DublinCoreRecord result = instance.read();
        assertNotNull(result);
        assertNotNull(result.getDc());
        assertEquals(local.getPid(), result.getPid());

        HashMap namespaces = new HashMap();
        namespaces.put("oai_dc", DcStreamEditor.DATASTREAM_FORMAT_URI);
        namespaces.put("dc", "http://purl.org/dc/elements/1.1/");
        XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(namespaces));
        String toXml = DcUtils.toXml(result.getDc(), true);
        XMLAssert.assertXpathExists("/oai_dc:dc/dc:title[text()='[1]']", toXml);
        XMLAssert.assertXpathExists("/oai_dc:dc/dc:identifier[text()='" + local.getPid() +"']", toXml);
        XMLAssert.assertXpathExists("/oai_dc:dc/dc:type[text()='" + model +"']", toXml);
    }

    @Test
    public void testTransformation() throws Exception {
        ModsType mods = ModsUtils.unmarshalModsType(new StreamSource(DcStreamEditorTest.class.getResource("periodical.xml").toExternalForm()));
        Transformer t = DcUtils.modsTransformer("model:periodical");
        JAXBSource jaxbSource = new JAXBSource(ModsUtils.defaultMarshaller(false),
                new ObjectFactory().createMods(mods));
        StringWriter dump = new StringWriter();
        t.transform(jaxbSource, new StreamResult(dump));
        String toXml = dump.toString();
//        System.out.println(toXml);

        HashMap namespaces = new HashMap();
        namespaces.put("oai_dc", DcStreamEditor.DATASTREAM_FORMAT_URI);
        namespaces.put("dc", "http://purl.org/dc/elements/1.1/");
        XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(namespaces));
        XMLAssert.assertXpathExists("/oai_dc:dc/dc:title[text()='main']", toXml);
        XMLAssert.assertXpathExists("/oai_dc:dc/dc:title[text()='key']", toXml);
        XMLAssert.assertXpathExists("/oai_dc:dc/dc:title[text()='alternative']", toXml);
        XMLAssert.assertXpathEvaluatesTo("3", "count(/oai_dc:dc/dc:title)", toXml);
        // XXX needs more test
    }
}
