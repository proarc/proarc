/*
 * Copyright (C) 2014 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.urnnbn;

import cz.cas.lib.proarc.mix.MixType;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.urnnbn.model.registration.DigitalDocument;
import cz.cas.lib.proarc.urnnbn.model.registration.Import;
import cz.cas.lib.proarc.urnnbn.model.registration.Monograph;
import cz.cas.lib.proarc.urnnbn.model.registration.MonographVolume;
import cz.cas.lib.proarc.urnnbn.model.registration.OtherEntity;
import cz.cas.lib.proarc.urnnbn.model.registration.PeriodicalIssue;
import cz.cas.lib.proarc.urnnbn.model.registration.PeriodicalIssue.TitleInfo;
import cz.cas.lib.proarc.urnnbn.model.registration.Publication;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.XMLConstants;
import javax.xml.bind.JAXB;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.util.JAXBSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;

/**
 * Creates intellectual entities from hierarchies of NDK digital objects to
 * register in the resolver.
 *
 * @author Jan Pokorsky
 */
public class NdkEntityFactory {

    private static final Logger LOG = Logger.getLogger(NdkEntityFactory.class.getName());
    private static final Level XML_DEBUG_LEVEL = Level.FINE;

    public Import createMultipartMonographImport(
            ModsDefinition titleMods,  ModsDefinition volumeMods,
            MixType mix, ErrorHandler status
            ) throws SAXException {

        MonographVolume m = new MonographVolume();
        m.setCcnb(ResolverUtils.getIdentifier("ccnb", volumeMods, titleMods));
        m.setDigitalBorn(false);
        m.setIsbn(ResolverUtils.getIdentifier("isbn", volumeMods, titleMods));
        //mods:name[@type='personal' and not(@usage='primary')]//mods:namePart[not(@type= 'date')]
        m.setOtherOriginator(ResolverUtils.getOriginator("personal", false, volumeMods, titleMods));
        m.setPrimaryOriginator(ResolverUtils.getPrimaryOriginator(volumeMods, titleMods));
        m.setPublication(ResolverUtils.getPublication(volumeMods, titleMods));

        // required
        MonographVolume.TitleInfo titleInfo = new MonographVolume.TitleInfo();
        // required
        titleInfo.setMonographTitle(ResolverUtils.getTitle(titleMods));
        // required
        titleInfo.setVolumeTitle(ResolverUtils.getTitlePartNumber(volumeMods));
        if (titleInfo.getVolumeTitle() == null) {
            titleInfo.setVolumeTitle(ResolverUtils.getTitlePartName(volumeMods));
        }
        m.setTitleInfo(titleInfo);

        Import imp = new Import();
        imp.setMonographVolume(m);
        DigitalDocument digitalDocument = new DigitalDocumentBuilder()
                .setUuid(ResolverUtils.getIdentifier("uuid", volumeMods))
                .setMix(mix)
                .build();
        imp.setDigitalDocument(digitalDocument);
        debugXml(imp);
        validate(imp, status);
        return imp;
    }

    public Import createMonographImport(
            ModsDefinition volumeMods, MixType mix, ErrorHandler status
            ) throws SAXException {

        Monograph m = new Monograph();
        m.setCcnb(ResolverUtils.getIdentifier("ccnb", volumeMods));
        m.setDigitalBorn(false);
        m.setIsbn(ResolverUtils.getIdentifier("isbn", volumeMods));
        //mods:name[@type='personal' and not(@usage='primary')]//mods:namePart[not(@type= 'date')]
        m.setOtherOriginator(ResolverUtils.getOriginator("personal", false, volumeMods));

        // mods:name[@type='personal' and @usage='primary']//mods:namePart[not(@type= 'date')]
        // mods:name[@type='corporate']
        // mods:name[@type='conference']
        // optional, type:required, value:optional
        m.setPrimaryOriginator(ResolverUtils.getPrimaryOriginator(volumeMods));
        m.setPublication(ResolverUtils.getPublication(volumeMods));

        // required
        Monograph.TitleInfo titleInfo = new Monograph.TitleInfo();
        TitleInfoDefinition modsTitle = ResolverUtils.getTitleInfo(volumeMods);
        if (modsTitle != null) {
            // required
            //mods:titleInfo/mods:partNumber
            //mods:titleInfo/mods:partName
            titleInfo.setTitle(ResolverUtils.getStringPlusLanguage(modsTitle.getTitle()));
            // optional
            titleInfo.setSubTitle(ResolverUtils.getStringPlusLanguage(modsTitle.getSubTitle()));
        }
        m.setTitleInfo(titleInfo);

        Import imp = new Import();
        imp.setMonograph(m);
        DigitalDocument digitalDocument = new DigitalDocumentBuilder()
                .setUuid(ResolverUtils.getIdentifier("uuid", volumeMods))
                .setMix(mix)
                .build();
        imp.setDigitalDocument(digitalDocument);
        debugXml(imp);
        validate(imp, status);
        return imp;
    }

    public Import createPeriodicalIssueImport(
            ModsDefinition titleMods, ModsDefinition volumeMods,
            ModsDefinition issueMods, MixType mix, ErrorHandler status
            ) throws SAXException {

        Import imp = new Import();
        PeriodicalIssue issue = new PeriodicalIssue();
        // optional
        issue.setCcnb(ResolverUtils.getIdentifier("ccnb", issueMods, volumeMods, titleMods));
        // optional
        issue.setDigitalBorn(false);
        // optional
//        issue.setDocumentType("???");
        // optional
        issue.setIssn(ResolverUtils.getIdentifier("issn", issueMods, volumeMods, titleMods));
        // optional; maxOccurs="1" !!! ???
//        issue.setOtherId("type:value");
        // optional
        // mods:name[@type='personal' and not(@usage='primary')]//mods:namePart[not(@type= 'date')]
        issue.setOtherOriginator(ResolverUtils.getOriginator("personal", false, issueMods, volumeMods, titleMods));

        // mods:name[@type='personal' and @usage='primary']//mods:namePart[not(@type= 'date')]
        // mods:name[@type='corporate']
        // mods:name[@type='conference']
        // optional, type:required, value:optional
        issue.setPrimaryOriginator(ResolverUtils.getPrimaryOriginator(issueMods, volumeMods, titleMods));

        // optional, place:optional, publisher:optional, year:optional
        issue.setPublication(ResolverUtils.getPublication(issueMods, volumeMods, titleMods));

        TitleInfo titleInfo = new TitleInfo();
        // required
        // mods:mods[@ID='MODS_ISSUE_XXXX']/mods:titleInfo/mods:partNumber
        // mods:mods[@ID='MODS_ISSUE_XXXX']/mods:titleInfo/mods:partName
        // mods:mods[@ID='MODS_ISSUE_XXXX']/mods:originInfo/mods:dateIssued
        titleInfo.setIssueTitle(ResolverUtils.getTitlePartNumber(issueMods));
        if (titleInfo.getIssueTitle() == null) {
            titleInfo.setIssueTitle(ResolverUtils.getTitlePartName(issueMods));
            if (titleInfo.getIssueTitle() == null) {
                Publication publication = ResolverUtils.getPublication(issueMods);
                String dateIssued = publication != null && publication.getYear() != null
                        ? publication.getYear() : null;
                titleInfo.setIssueTitle(dateIssued);
            }
        }
        // required
        // mods:mods[@ID='MODS_TITLE_0001']/mods:titleInfo/mods:title
        titleInfo.setPeriodicalTitle(ResolverUtils.getTitle(titleMods));
        // optional
        // mods:mods[@ID='MODS_VOLUME_0001']/mods:titleInfo/mods:partNumber
        titleInfo.setVolumeTitle(ResolverUtils.getTitlePartNumber(volumeMods));
        // required
        issue.setTitleInfo(titleInfo);
        imp.setPeriodicalIssue(issue);

        DigitalDocument digitalDocument = new DigitalDocumentBuilder()
                .setUuid(ResolverUtils.getIdentifier("uuid", issueMods))
                .setMix(mix)
                .build();
        imp.setDigitalDocument(digitalDocument);
        debugXml(imp);
        validate(imp, status);
        return imp;
    }

    public Import createCartographicImport(
            ModsDefinition titleMods, MixType mix, ErrorHandler status
            ) throws SAXException {

        return createOtherEntityImport(titleMods, "cartographic", mix, status);
    }

    public Import createSheetMusicImport(
            ModsDefinition titleMods, MixType mix, ErrorHandler status
            ) throws SAXException {

        return createOtherEntityImport(titleMods, "sheetmusic", mix, status);
    }

    /**
     *
     * @param titleMods
     * @param documentType optional cartographic,sheetmusic, ... see NDK
     * @param mix
     * @param status
     * @return
     * @throws SAXException
     */
    public Import createOtherEntityImport(
            ModsDefinition titleMods, String documentType, MixType mix, ErrorHandler status
            ) throws SAXException {

        OtherEntity entity = new OtherEntity();
        entity.setCcnb(ResolverUtils.getIdentifier("ccnb", titleMods));
        entity.setDigitalBorn(false);
        entity.setDocumentType(documentType);
        entity.setIsbn(ResolverUtils.getIdentifier("isbn", titleMods));
        //mods:name[@type='personal' and not(@usage='primary')]//mods:namePart[not(@type= 'date')]
        entity.setOtherOriginator(ResolverUtils.getOriginator("personal", false, titleMods));

        // mods:name[@type='personal' and @usage='primary']//mods:namePart[not(@type= 'date')]
        // mods:name[@type='corporate']
        // mods:name[@type='conference']
        // optional, type:required, value:optional
        entity.setPrimaryOriginator(ResolverUtils.getPrimaryOriginator(titleMods));
        entity.setPublication(ResolverUtils.getPublication(titleMods));

        // required
        OtherEntity.TitleInfo titleInfo = new OtherEntity.TitleInfo();
        TitleInfoDefinition modsTitle = ResolverUtils.getTitleInfo(titleMods);
        if (modsTitle != null) {
            // required
            //mods:titleInfo/mods:partNumber
            //mods:titleInfo/mods:partName
            titleInfo.setTitle(ResolverUtils.getStringPlusLanguage(modsTitle.getTitle()));
            // optional
            titleInfo.setSubTitle(ResolverUtils.getStringPlusLanguage(modsTitle.getSubTitle()));
        }
        entity.setTitleInfo(titleInfo);

        Import imp = new Import();
        imp.setOtherEntity(entity);
        DigitalDocument digitalDocument = new DigitalDocumentBuilder()
                .setUuid(ResolverUtils.getIdentifier("uuid", titleMods))
                .setMix(mix)
                .build();
        imp.setDigitalDocument(digitalDocument);
        debugXml(imp);
        validate(imp, status);
        return imp;
    }

    private void validate(Import imp, ErrorHandler status) throws SAXException {
        try {
            SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema schema = schemaFactory.newSchema(getClass().getResource("registration/digDocRegistration.xsd"));
            Validator validator = schema.newValidator();
            validator.setErrorHandler(status);
            validator.validate(new JAXBSource(JAXBContext.newInstance(Import.class), imp));
        } catch (IOException ex) {
            throw new SAXException(ex);
        } catch (JAXBException ex) {
            throw new SAXException(ex);
        }
    }

    private String toString(Import imp) {
        StringWriter dump = new StringWriter();
        try {
            JAXB.marshal(imp, dump);
        } catch (Exception e) {
            PrintWriter pw = new PrintWriter(dump);
            e.printStackTrace(pw);
            pw.close();
        }
        return dump.toString();
    }

    void debugXml(Import imp) {
        if (LOG.isLoggable(XML_DEBUG_LEVEL)) {
            LOG.log(XML_DEBUG_LEVEL, toString(imp));
        }
    }

}
