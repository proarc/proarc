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
import java.util.logging.Level;
import java.util.logging.Logger;
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
            MixType mix, ErrorHandler status, boolean oldPrint, String urnNbnValue
            ) throws SAXException {

        MonographVolume m = new MonographVolume();
        m.setCcnb(ResolverUtils.getIdentifierValue("ccnb", volumeMods, titleMods));
        m.setDigitalBorn(false);
        if (oldPrint) {
            m.setDocumentType("oldprint-monographVolume");
        }
        m.setIsbn(ResolverUtils.getIdentifierValue("isbn", volumeMods, titleMods));
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
                .setUuid(ResolverUtils.getIdentifierValue("uuid", volumeMods))
                .setMix(mix).setPreccessor(urnNbnValue)
                .build();
        imp.setDigitalDocument(digitalDocument);
        debugXml(imp);
        ResolverXmlUtils.validate(imp, status);
        return imp;
    }

    public Import createMonographImport(
            ModsDefinition volumeMods, MixType mix, ErrorHandler status, boolean eBorn, boolean oldPrint, String urnNbnValue
            ) throws SAXException {

        Monograph m = new Monograph();
        m.setCcnb(ResolverUtils.getIdentifierValue("ccnb", volumeMods));
        m.setDigitalBorn(eBorn);
        if (oldPrint) {
            m.setDocumentType("oldprint-monograph");
        }
        m.setIsbn(ResolverUtils.getIdentifierValue("isbn", volumeMods));
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
        DigitalDocument digitalDocument;
        if (eBorn) {
            digitalDocument = new DigitalDocumentBuilder().setUuid(ResolverUtils.getIdentifierValue("uuid", volumeMods)).setPreccessor(urnNbnValue).build();
        } else{
            digitalDocument = new DigitalDocumentBuilder().setUuid(ResolverUtils.getIdentifierValue("uuid", volumeMods)).setMix(mix).setPreccessor(urnNbnValue).build();
        }
        imp.setDigitalDocument(digitalDocument);
        debugXml(imp);
        ResolverXmlUtils.validate(imp, status);
        return imp;
    }

    public Import createMusicDocumentImport(ModsDefinition documentMods, ErrorHandler status, String urnNbnValue)
            throws SAXException {

        Monograph m = new Monograph();
        m.setOtherOriginator(ResolverUtils.getOriginator("personal", false, documentMods));
        m.setPrimaryOriginator(ResolverUtils.getPrimaryOriginator(documentMods));
        m.setPublication(ResolverUtils.getPublication(documentMods));

        // required
        Monograph.TitleInfo titleInfo = new Monograph.TitleInfo();
        TitleInfoDefinition modsTitle = ResolverUtils.getTitleInfo(documentMods);
        if (modsTitle != null) {
            titleInfo.setTitle(ResolverUtils.getStringPlusLanguage(modsTitle.getTitle()));
            titleInfo.setSubTitle(ResolverUtils.getStringPlusLanguage(modsTitle.getSubTitle()));
        }
        m.setTitleInfo(titleInfo);

        Import imp = new Import();
        imp.setMonograph(m);
        DigitalDocument digitalDocument;
        digitalDocument = new DigitalDocumentBuilder().setUuid(ResolverUtils.getIdentifierValue("uuid", documentMods)).setPreccessor(urnNbnValue).build();
        imp.setDigitalDocument(digitalDocument);
        debugXml(imp);
        ResolverXmlUtils.validate(imp, status);
        return imp;
    }


    public Import createPeriodicalIssueImport(
            ModsDefinition titleMods, ModsDefinition volumeMods,
            ModsDefinition issueMods, MixType mix, ErrorHandler status, boolean eBorn, String urnNbnValue
            ) throws SAXException {

        Import imp = new Import();
        PeriodicalIssue issue = new PeriodicalIssue();
        // optional
        issue.setCcnb(ResolverUtils.getIdentifierValue("ccnb", issueMods, volumeMods, titleMods));
        // optional
        issue.setDigitalBorn(eBorn);
        // optional
//        issue.setDocumentType("???");
        // optional
        issue.setIssn(ResolverUtils.getIdentifierValue("issn", issueMods, volumeMods, titleMods));
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

        DigitalDocument digitalDocument;
        if (eBorn) {
            digitalDocument = new DigitalDocumentBuilder().setUuid(ResolverUtils.getIdentifierValue("uuid", issueMods)).setPreccessor(urnNbnValue).build();
        } else{
            digitalDocument = new DigitalDocumentBuilder().setUuid(ResolverUtils.getIdentifierValue("uuid", issueMods)).setPreccessor(urnNbnValue).setMix(mix).build();
        }
        imp.setDigitalDocument(digitalDocument);
        debugXml(imp);
        ResolverXmlUtils.validate(imp, status);
        return imp;
    }

    public Import createCartographicImport(
            ModsDefinition titleMods, MixType mix, ErrorHandler status, String urnNbnValue
            ) throws SAXException {

        return createOtherEntityImport(titleMods, "cartographic", mix, status, false, urnNbnValue);
    }

    public Import createSheetMusicImport(
            ModsDefinition titleMods, MixType mix, ErrorHandler status, String urnNbnValue
            ) throws SAXException {

        return createOtherEntityImport(titleMods, "sheetmusic", mix, status, false, urnNbnValue);
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
            ModsDefinition titleMods, String documentType, MixType mix, ErrorHandler status, boolean eBorn,
            String urnNbnValue) throws SAXException {

        OtherEntity entity = new OtherEntity();
        entity.setCcnb(ResolverUtils.getIdentifierValue("ccnb", titleMods));
        entity.setDigitalBorn(eBorn);
        entity.setDocumentType(documentType);
        entity.setIsbn(ResolverUtils.getIdentifierValue("isbn", titleMods));
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

        DigitalDocument digitalDocument;
        if (eBorn) {
            digitalDocument = new DigitalDocumentBuilder().setUuid(ResolverUtils.getIdentifierValue("uuid", titleMods)).setPreccessor(urnNbnValue).build();
        } else{
            digitalDocument = new DigitalDocumentBuilder().setUuid(ResolverUtils.getIdentifierValue("uuid", titleMods)).setMix(mix).setPreccessor(urnNbnValue).build();
        }
        imp.setDigitalDocument(digitalDocument);
        debugXml(imp);
        ResolverXmlUtils.validate(imp, status);
        return imp;
    }

    void debugXml(Import imp) {
        if (LOG.isLoggable(XML_DEBUG_LEVEL)) {
            LOG.log(XML_DEBUG_LEVEL, ResolverXmlUtils.toString(imp));
        }
    }

}
