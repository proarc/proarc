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
package cz.cas.lib.proarc.common.urnnbn;

import cz.cas.lib.proarc.common.object.ndk.NdkClippingPlugin;
import cz.cas.lib.proarc.common.process.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.process.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.process.export.mets.ValidationErrorHandler;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DisseminationHandler;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.VisitorException;
import cz.cas.lib.proarc.common.object.ndk.DefaultNdkVisitor;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnStatusHandler.Status;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixType;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.urnnbn.NdkEntityFactory;
import cz.cas.lib.proarc.urnnbn.ResolverClient;
import cz.cas.lib.proarc.urnnbn.ResolverUtils;
import cz.cas.lib.proarc.urnnbn.model.registration.Import;
import cz.cas.lib.proarc.urnnbn.model.response.ErrorType;
import cz.cas.lib.proarc.urnnbn.model.response.RegistrarScopeIdentifier;
import cz.cas.lib.proarc.urnnbn.model.response.UrnNbn;
import java.io.File;
import java.io.InputStream;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.core.Response;
import org.apache.commons.io.FileUtils;
import org.xml.sax.SAXException;

/**
 * Walks down the NDK hierarchy and registers missing URN:NBN with {@link ResolverClient}.
 *
 * <p>It registers {@link NdkPlugin#MODEL_PERIODICALISSUE issue},
 * {@link NdkPlugin#MODEL_MONOGRAPHVOLUME monograph volume},
 * {@link NdkPlugin#MODEL_MONOGRAPHUNIT monograph unit},
 * {@link NdkPlugin#MODEL_CARTOGRAPHIC},
 * {@link NdkPlugin#MODEL_GRAPHIC},
 * {@link NdkPlugin#MODEL_SHEETMUSIC}.
 *
 * <p>Supplements {@link NdkPlugin#MODEL_PERIODICALSUPPLEMENT} and
 * {@link NdkPlugin#MODEL_MONOGRAPHSUPPLEMENT} are registered in case
 * they are not members of models listed above.
 *
 * <p>The visitor can start at any level of the hierarchy.
 *
 * @author Jan Pokorsky
 */
public class UrnNbnVisitor extends DefaultNdkVisitor<Void, UrnNbnContext> {

    private static final Set<String> REGISTERABLE_MODEL_IDS = new HashSet<String>(Arrays.asList(
            NdkPlugin.MODEL_PERIODICALISSUE,
            NdkPlugin.MODEL_PERIODICALSUPPLEMENT,
            NdkPlugin.MODEL_MONOGRAPHVOLUME,
            NdkPlugin.MODEL_MONOGRAPHUNIT,
            NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT,
            NdkPlugin.MODEL_ARTICLE,
            NdkPlugin.MODEL_CARTOGRAPHIC,
            NdkPlugin.MODEL_GRAPHIC,
            NdkPlugin.MODEL_SHEETMUSIC,
            NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME,
            NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT,
            NdkEbornPlugin.MODEL_ECHAPTER,
            NdkEbornPlugin.MODEL_EPERIODICALISSUE,
            NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT,
            NdkEbornPlugin.MODEL_EARTICLE,
            NdkEbornPlugin.MODEL_EPERIODICALVOLUME,
            NdkAudioPlugin.MODEL_MUSICDOCUMENT,
            NdkAudioPlugin.MODEL_PHONOGRAPH,
            OldPrintPlugin.MODEL_MONOGRAPHVOLUME,
            OldPrintPlugin.MODEL_MONOGRAPHUNIT,
            OldPrintPlugin.MODEL_SUPPLEMENT,
            OldPrintPlugin.MODEL_CARTOGRAPHIC,
            OldPrintPlugin.MODEL_SHEETMUSIC,
            OldPrintPlugin.MODEL_GRAPHICS,
            NdkClippingPlugin.MODEL_CLIPPING_UNIT
    ));
    private static final Logger LOG = Logger.getLogger(UrnNbnVisitor.class.getName());

    private NdkEntityFactory resolverEntities = new NdkEntityFactory();
    private DigitalObjectElement registeringObject;
//    private Set<String> registeredPids = new HashSet<String>();

    /**
     * The hierarchy path as a stack of visited nodes. Size of 1 means start of walking.
     * Be aware the visitor may start at any node of the hierarchy.
     *
     * @see #isEntryPoint()
     */
    private Deque<String> traversePath = new ArrayDeque<String>();

    public UrnNbnVisitor(DigitalObjectCrawler crawler) {
        super(crawler);
    }

    @Override
    public Void visit(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        try {
            traversePath.addFirst(elm.getPid());
            return super.visit(elm, p);
        } finally {
            traversePath.removeFirst();
        }
    }

    @Override
    public Void visitNdkPeriodicalIssue(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The issue under " + registeringObject.toLog());
            return null;
        }
        try {
            super.visitChildren(elm, p);
            registeringObject = elm;
            return processNdkPeriodicalIssue(elm, p);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkEPeriodicalIssue(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The ePeriodicalIssue under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processNdkEPeriodicalIssue(elm, p);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkPeriodicalSupplement(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            if (!NdkPlugin.MODEL_PERIODICALISSUE.equals(registeringObject.getModelId())) {
                // supplement under issue - ignore
                // invalid hierarchy
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                        "The supplement under " + registeringObject.toLog());
            }
            return null;
        }
        try {
            DigitalObjectElement parent = getCrawler().getParent(elm.getPid());
            String parentModelId = parent.getModelId();
            if (parent == DigitalObjectElement.NULL || NdkPlugin.MODEL_PERIODICALVOLUME.equals(parentModelId) || NdkPlugin.MODEL_PERIODICALISSUE.equals(parentModelId)) {
                try {
                    registeringObject = elm;
                    return processNdkPeriodicalIssue(elm, p);
                } finally {
                    registeringObject = null;
                }
            } else {
                // the visitor started on issue's supplement
                return visitEnclosingElement2Register(elm, p);
            }
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        }
    }

    @Override
    public Void visitNdkEPeriodicalSupplement(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            if (!NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(registeringObject.getModelId())) {
                // supplement under issue - ignore
                // invalid hierarchy
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                        "The supplement under " + registeringObject.toLog());
            }
            return null;
        }
        try {
            DigitalObjectElement parent = getCrawler().getParent(elm.getPid());
            String parentModelId = parent.getModelId();
            if (parent == DigitalObjectElement.NULL || NdkEbornPlugin.MODEL_EPERIODICALVOLUME.equals(parentModelId) || NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(parentModelId)) {
                try {
                    registeringObject = elm;
                    return processNdkEPeriodicalIssue(elm, p);
                } finally {
                    registeringObject = null;
                }
            } else {
                // the visitor started on issue's supplement
                return visitEnclosingElement2Register(elm, p);
            }
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        }
    }

    @Override
    public Void visitNdkMonographVolume(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The volume under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processNdkMonographVolumeOrUnitOrSupplement(elm, p);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkMonographUnit(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The unit under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processNdkMonographVolumeOrUnitOrSupplement(elm, p);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkEMonographVolume(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The eDocument volume under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processNdkEMonographVolumeOrSupplement(elm, p);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkMonographSupplement(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            if (!(NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(registeringObject.getModelId())
                    || NdkPlugin.MODEL_MONOGRAPHUNIT.equals(registeringObject.getModelId()))) {
                // supplement under monograph volume - ignore
                // invalid hierarchy
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                        "The supplement under " + registeringObject.toLog());
            }
            return null;
        }
        try {
            DigitalObjectElement parent = getCrawler().getParent(elm.getPid());
            if (parent == DigitalObjectElement.NULL || NdkPlugin.MODEL_MONOGRAPHTITLE.equals(parent.getModelId())) {
                try {
                    registeringObject = elm;
                    return processNdkMonographVolumeOrUnitOrSupplement(elm, p);
                } finally {
                    registeringObject = null;
                }
            } else {
                // the visitor started on volume's supplement
                return visitEnclosingElement2Register(elm, p);
            }
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        }
    }

    @Override
    public Void visitNdkEMonographSupplement(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            if (!(NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME.equals(registeringObject.getModelId()))) {
                // supplement under monograph volume - ignore
                // invalid hierarchy
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                        "The supplement under " + registeringObject.toLog());
            }
            return null;
        }
        try {
            DigitalObjectElement parent = getCrawler().getParent(elm.getPid());
            String parentModelId = parent.getModelId();
            if (parent == DigitalObjectElement.NULL || NdkEbornPlugin.MODEL_EMONOGRAPHTITLE.equals(parentModelId)) {
                try {
                    registeringObject = elm;
                    return processNdkEMonographVolumeOrSupplement(elm, p);
                } finally {
                    registeringObject = null;
                }
            } else {
                // the visitor started on issue's supplement
                return visitEnclosingElement2Register(elm, p);
            }
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        }
    }

    @Override
    public Void visitNdkCartographic(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The cartographic under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processOtherEntity(elm, "cartographic", p, null);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkClippingUnit(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The clipping unit under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processOtherEntity(elm, "clipping", p, null);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkGraphic(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The graphic under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processOtherEntity(elm, "graphic", p, null);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkSheetMusic(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The sheet music under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processOtherEntity(elm, "sheetmusic", p, null);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkArticle(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT, "The article under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            MixType mixType = searchMixElement(getParent(elm), p);
            return processOtherEntity(elm, "article", p, mixType);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkEArticle(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The eArticle under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processEBornOtherEntity(elm, "eArticle", p);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkChapter(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            return super.visitNdkChapter(elm, p);
        } else {
            return visitEnclosingElement2Register(elm, p);
        }
    }

    @Override
    public Void visitNdkEChapter(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The eChapter under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processEBornOtherEntity(elm, "eChapter", p);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitNdkPicture(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            return super.visitNdkPicture(elm, p);
        } else {
            return visitEnclosingElement2Register(elm, p);
        }
    }

    @Override
    public Void visitNdkPage(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject == null) {
            // unknown enclosing object to register
            return visitEnclosingElement2Register(elm, p);
        }
        MixType mix = getMix(elm, p);
        if (mix != null) {
            throw new StopOnFirstMixException(mix);
        }
        return null;
    }

    @Override
    public Void visitNdkMusicDocument(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The NDK Music Document under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processNdkMusicDocument(elm, p);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitOldPrintSheetmusic(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The oldprint sheet music under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processOtherEntity(elm, "oldprint-sheetmusic", p, null);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitOldPrintGraphics(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT, "The oldprint graphic under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            MixType mixType = searchMix(elm, p);
            return processOtherEntity(elm, "oldprint-picture", p, mixType);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitOldPrintChapter(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            return super.visitNdkChapter(elm, p);
        } else {
            return visitEnclosingElement2Register(elm, p);
        }
    }

    @Override
    public Void visitOldPrintCartographic(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The oldprint cartographic under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processOtherEntity(elm, "oldprint-cartographic", p, null);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitOldPrintMonographSupplement(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            if (!(OldPrintPlugin.MODEL_MONOGRAPHVOLUME.equals(registeringObject.getModelId()) || OldPrintPlugin.MODEL_MONOGRAPHUNIT.equals(registeringObject.getModelId()))) {
                // supplement under monograph volume - ignore
                // invalid hierarchy
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                        "The oldprint supplement under " + registeringObject.toLog());
            }
            return null;
        }
        try {
            DigitalObjectElement parent = getCrawler().getParent(elm.getPid());
            if (parent == DigitalObjectElement.NULL || OldPrintPlugin.MODEL_MONOGRAPHTITLE.equals(parent.getModelId())) {
                try {
                    registeringObject = elm;
                    return processOldPrintMonographVolumeOrUnitOrSupplement(elm, p);
                } finally {
                    registeringObject = null;
                }
            } else {
                // the visitor started on volume's supplement
                return visitEnclosingElement2Register(elm, p);
            }
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        }
    }

    @Override
    public Void visitOldPrintMonographVolume(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The oldprint volume under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processOldPrintMonographVolumeOrUnitOrSupplement(elm, p);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitOldPrintMonographUnit(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject != null) {
            // invalid hierarchy
            p.getStatus().error(elm, Status.UNEXPECTED_PARENT,
                    "The oldprint volume under " + registeringObject.toLog());
            return null;
        }
        try {
            registeringObject = elm;
            return processOldPrintMonographVolumeOrUnitOrSupplement(elm, p);
        } catch (DigitalObjectException ex) {
            throw new VisitorException(ex);
        } finally {
            registeringObject = null;
        }
    }

    @Override
    public Void visitOldPrintPage(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        if (registeringObject == null) {
            // unknown enclosing object to register
            return visitEnclosingElement2Register(elm, p);
        }
        MixType mix = getMix(elm, p);
        if (mix != null) {
            throw new StopOnFirstMixException(mix);
        }
        return null;
    }

    private Void processNdkMusicDocument(DigitalObjectElement elm, UrnNbnContext p) throws DigitalObjectException {
        final DigitalObjectHandler handler = elm.getHandler();
        final MetadataHandler<ModsDefinition> modsHandler = handler.<ModsDefinition>metadata();
        final DescriptionMetadata<ModsDefinition> documentDescription = modsHandler.getMetadata();
        ModsDefinition documentMods = documentDescription.getData();
        String urnnbnOldValue = ResolverUtils.getIdentifierValue("urnnbn", documentMods);
        if (p.isRegisterNewPid()) {
            if (urnnbnOldValue != null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN exists.", urnnbnOldValue);
                return null;
            }
            try {
                Import document;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                document = resolverEntities.createMusicDocumentImport(documentMods, xmlHandler, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(document, elm, p);
                updateModsWithUrnNbn(urnNbnResponse, documentMods, documentDescription, modsHandler, elm, p);
            } catch (SAXException ex) {
                // registration request not valid
                p.getStatus().error(elm, ex);
            }
        } else if (p.isInvalidateUrnNbn()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                invalidateUrnNbn(documentMods, documentDescription, modsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isCreateSuccessor()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                Import document;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                document = resolverEntities.createMusicDocumentImport(documentMods, xmlHandler, urnnbnOldValue);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                RegistrarScopeIdentifier registrarScopeIdentifier = removeUuidFromObject(urnnbnOldValue, elm, p);
                if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                    p.getStatus().warning(elm, Status.EXCEPTION,
                            "The resolver returns no UUID value! Check the server configuration.", elm.getPid());
                    return null;
                }
                if (!(registrarScopeIdentifier.getType() + ":" + registrarScopeIdentifier.getValue()).equals(elm.getPid())) {
                    p.getStatus().warning(elm, Status.WRONG_UUID_DELETED, "Wrong Uuid deleted (expected: " +
                            elm.getPid() + " get: " + registrarScopeIdentifier.getValue(), elm.getPid());
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(document, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, documentMods, documentDescription, modsHandler, elm, p);
            } catch (SAXException ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isRegisterAgainPid()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN does not exists.", urnnbnOldValue);
                return null;
            }
            try {
                Import document;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                document = resolverEntities.createMusicDocumentImport(documentMods, xmlHandler, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerAgainEntity(urnnbnOldValue, document, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, documentMods, documentDescription, modsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isUpdateCzidloRecord()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                updateCzidloRecord(urnnbnOldValue, documentMods, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        }
        return null;
    }

    private boolean isEntryPoint() {
        return traversePath.size() == 1;
    }

    private Void visitEnclosingElement2Register(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        // prevent cycles
        if (!isEntryPoint()) {
            return null;
        }
        DigitalObjectElement parent = searchEnclosingElement2Register(elm, p);
        if (parent != null) {
            return parent.accept(this, p);
        }
        return null;
    }

    private DigitalObjectElement searchEnclosingElement2Register(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        try {
            Iterator<DigitalObjectElement> path = getCrawler().getPath(elm.getPid()).iterator();
            while (path.hasNext()) {
                DigitalObjectElement pathItem = path.next();
                String modelId = pathItem.getModelId();
                if (REGISTERABLE_MODEL_IDS.contains(modelId)) {
                    return pathItem;
                }
            }
            return null;
        } catch (DigitalObjectNotFoundException ex) {
            throw new VisitorException(elm.getPid(), ex);
        }
    }

    private Void processNdkPeriodicalIssue(DigitalObjectElement elm, UrnNbnContext p)
            throws DigitalObjectException, VisitorException {

        final String pid = elm.getPid();
        final DigitalObjectHandler issueHandler = elm.getHandler();
        final MetadataHandler<ModsDefinition> issueModsHandler = issueHandler.<ModsDefinition>metadata();
        final DescriptionMetadata<ModsDefinition> issueDescription = issueModsHandler.getMetadata();
        ModsDefinition issueMods = issueDescription.getData();
        // check URNNBN exists
        String urnnbnOldValue = ResolverUtils.getIdentifierValue("urnnbn", issueMods);
        if (p.isRegisterNewPid()) {
            if (urnnbnOldValue != null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN exists.", urnnbnOldValue);
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            if (!path.hasNext()) {
                p.getStatus().error(elm, Status.MISSING_PARENT, "Requires Periodical Title or Volume as parent!");
                return null;
            }
            DigitalObjectElement titleElm = path.next();
            if (!NdkPlugin.MODEL_PERIODICAL.equals(titleElm.getModelId())) {
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                        "Requires Periodical Title as a root of the hierarchy instead of %s!",
                        titleElm.toLog()));
                return null;
            }
            ModsDefinition titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            ModsDefinition volumeMods = null;

            if (path.hasNext()) {
                DigitalObjectElement volumeElm = path.next();
                if (!NdkPlugin.MODEL_PERIODICALVOLUME.equals(volumeElm.getModelId())) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires Periodical Title or Volume as parent instead of %s!",
                            volumeElm.toLog()));
                    return null;
                }
                volumeMods = volumeElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }
            MixType mix = searchMix(elm, p);
            if (mix == null) {
                return null;
            }
            if (volumeMods == null) {
                volumeMods = new ModsDefinition();
            }
            try {
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                Import issueImport = resolverEntities.createPeriodicalIssueImport(
                        titleMods, volumeMods, issueMods, mix, xmlHandler, false, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(issueImport, elm, p);
                updateModsWithUrnNbn(urnNbnResponse, issueMods, issueDescription, issueModsHandler, elm, p);
            } catch (SAXException ex) {
                // registration request not valid
                p.getStatus().error(elm, ex);
            }
        } else if (p.isInvalidateUrnNbn()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                invalidateUrnNbn(issueMods, issueDescription, issueModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isCreateSuccessor()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            if (!path.hasNext()) {
                p.getStatus().error(elm, Status.MISSING_PARENT, "Requires Periodical Title or Volume as parent!");
                return null;
            }
            DigitalObjectElement titleElm = path.next();
            if (!NdkPlugin.MODEL_PERIODICAL.equals(titleElm.getModelId())) {
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                        "Requires Periodical Title as a root of the hierarchy instead of %s!",
                        titleElm.toLog()));
                return null;
            }
            ModsDefinition titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            ModsDefinition volumeMods = null;

            if (path.hasNext()) {
                DigitalObjectElement volumeElm = path.next();
                if (!NdkPlugin.MODEL_PERIODICALVOLUME.equals(volumeElm.getModelId())) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires Periodical Title or Volume as parent instead of %s!",
                            volumeElm.toLog()));
                    return null;
                }
                volumeMods = volumeElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }
            MixType mix = searchMix(elm, p);
            if (mix == null) {
                return null;
            }
            if (volumeMods == null) {
                volumeMods = new ModsDefinition();
            }
            try {
                Import document;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                document = resolverEntities.createPeriodicalIssueImport(titleMods, volumeMods, issueMods, mix, xmlHandler, false, urnnbnOldValue);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                RegistrarScopeIdentifier registrarScopeIdentifier = removeUuidFromObject(urnnbnOldValue, elm, p);
                if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                    p.getStatus().warning(elm, Status.EXCEPTION,
                            "The resolver returns no UUID value! Check the server configuration.", elm.getPid());
                    return null;
                }
                if (!(registrarScopeIdentifier.getType() + ":" + registrarScopeIdentifier.getValue()).equals(elm.getPid())) {
                    p.getStatus().warning(elm, Status.WRONG_UUID_DELETED, "Wrong Uuid deleted (expected: " +
                            elm.getPid() + " get: " + registrarScopeIdentifier.getValue(), elm.getPid());
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(document, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, issueMods, issueDescription, issueModsHandler, elm, p);
            } catch (SAXException ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isRegisterAgainPid()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN does not exists.", urnnbnOldValue);
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            if (!path.hasNext()) {
                p.getStatus().error(elm, Status.MISSING_PARENT, "Requires Periodical Title or Volume as parent!");
                return null;
            }
            DigitalObjectElement titleElm = path.next();
            if (!NdkPlugin.MODEL_PERIODICAL.equals(titleElm.getModelId())) {
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                        "Requires Periodical Title as a root of the hierarchy instead of %s!",
                        titleElm.toLog()));
                return null;
            }
            ModsDefinition titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            ModsDefinition volumeMods = null;

            if (path.hasNext()) {
                DigitalObjectElement volumeElm = path.next();
                if (!NdkPlugin.MODEL_PERIODICALVOLUME.equals(volumeElm.getModelId())) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires Periodical Title or Volume as parent instead of %s!",
                            volumeElm.toLog()));
                    return null;
                }
                volumeMods = volumeElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }
            MixType mix = searchMix(elm, p);
            if (mix == null) {
                return null;
            }
            if (volumeMods == null) {
                volumeMods = new ModsDefinition();
            }
            try {
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                Import issueImport = resolverEntities.createPeriodicalIssueImport(
                        titleMods, volumeMods, issueMods, mix, xmlHandler, false, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerAgainEntity(urnnbnOldValue, issueImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, issueMods, issueDescription, issueModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isUpdateCzidloRecord()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                updateCzidloRecord(urnnbnOldValue, issueMods, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        }
        return null;
    }

    private Void processNdkEPeriodicalIssue(DigitalObjectElement elm, UrnNbnContext p)
            throws DigitalObjectException, VisitorException {

        final String pid = elm.getPid();
        final DigitalObjectHandler issueHandler = elm.getHandler();
        final MetadataHandler<ModsDefinition> issueModsHandler = issueHandler.<ModsDefinition>metadata();
        final DescriptionMetadata<ModsDefinition> issueDescription = issueModsHandler.getMetadata();
        ModsDefinition issueMods = issueDescription.getData();
        // check URNNBN exists
        String urnnbnOldValue = ResolverUtils.getIdentifierValue("urnnbn", issueMods);
        if (p.isRegisterNewPid()) {
            if (urnnbnOldValue != null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN exists.", urnnbnOldValue);
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            if (!path.hasNext()) {
                p.getStatus().error(elm, Status.MISSING_PARENT, "Requires ePeriodical Title or Volume as parent!");
                return null;
            }
            DigitalObjectElement titleElm = path.next();
            if (!NdkEbornPlugin.MODEL_EPERIODICAL.equals(titleElm.getModelId())) {
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                        "Requires ePeriodical Title as a root of the hierarchy instead of %s!",
                        titleElm.toLog()));
                return null;
            }
            ModsDefinition titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            ModsDefinition volumeMods = null;

            if (path.hasNext()) {
                DigitalObjectElement volumeElm = path.next();
                if (!NdkEbornPlugin.MODEL_EPERIODICALVOLUME.equals(volumeElm.getModelId())) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires ePeriodical Volume as parent instead of %s!",
                            volumeElm.toLog()));
                    return null;
                }
                volumeMods = volumeElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }
            if (volumeMods == null) {
                volumeMods = new ModsDefinition();
            }
            try {
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                Import issueImport = resolverEntities.createPeriodicalIssueImport(
                        titleMods, volumeMods, issueMods, null, xmlHandler, true, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(issueImport, elm, p);
                updateModsWithUrnNbn(urnNbnResponse, issueMods, issueDescription, issueModsHandler, elm, p);
            } catch (SAXException ex) {
                // registration request not valid
                p.getStatus().error(elm, ex);
            }
        } else if (p.isInvalidateUrnNbn()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                invalidateUrnNbn(issueMods, issueDescription, issueModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isCreateSuccessor()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            if (!path.hasNext()) {
                p.getStatus().error(elm, Status.MISSING_PARENT, "Requires ePeriodical Title or Volume as parent!");
                return null;
            }
            DigitalObjectElement titleElm = path.next();
            if (!NdkEbornPlugin.MODEL_EPERIODICAL.equals(titleElm.getModelId())) {
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                        "Requires ePeriodical Title as a root of the hierarchy instead of %s!",
                        titleElm.toLog()));
                return null;
            }
            ModsDefinition titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            ModsDefinition volumeMods = null;

            if (path.hasNext()) {
                DigitalObjectElement volumeElm = path.next();
                if (!NdkEbornPlugin.MODEL_EPERIODICALVOLUME.equals(volumeElm.getModelId())) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires ePeriodical Volume as parent instead of %s!",
                            volumeElm.toLog()));
                    return null;
                }
                volumeMods = volumeElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }
            if (volumeMods == null) {
                volumeMods = new ModsDefinition();
            }
            try {
                Import document;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                document = resolverEntities.createPeriodicalIssueImport(
                        titleMods, volumeMods, issueMods, null, xmlHandler, true, urnnbnOldValue);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                RegistrarScopeIdentifier registrarScopeIdentifier = removeUuidFromObject(urnnbnOldValue, elm, p);
                if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                    p.getStatus().warning(elm, Status.EXCEPTION,
                            "The resolver returns no UUID value! Check the server configuration.", elm.getPid());
                    return null;
                }
                if (!(registrarScopeIdentifier.getType() + ":" + registrarScopeIdentifier.getValue()).equals(elm.getPid())) {
                    p.getStatus().warning(elm, Status.WRONG_UUID_DELETED, "Wrong Uuid deleted (expected: " +
                            elm.getPid() + " get: " + registrarScopeIdentifier.getValue(), elm.getPid());
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(document, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, issueMods, issueDescription, issueModsHandler, elm, p);
            } catch (SAXException ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isRegisterAgainPid()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN does not exists.", urnnbnOldValue);
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            if (!path.hasNext()) {
                p.getStatus().error(elm, Status.MISSING_PARENT, "Requires ePeriodical Title or Volume as parent!");
                return null;
            }
            DigitalObjectElement titleElm = path.next();
            if (!NdkEbornPlugin.MODEL_EPERIODICAL.equals(titleElm.getModelId())) {
                p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                        "Requires ePeriodical Title as a root of the hierarchy instead of %s!",
                        titleElm.toLog()));
                return null;
            }
            ModsDefinition titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            ModsDefinition volumeMods = null;

            if (path.hasNext()) {
                DigitalObjectElement volumeElm = path.next();
                if (!NdkEbornPlugin.MODEL_EPERIODICALVOLUME.equals(volumeElm.getModelId())) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires ePeriodical Volume as parent instead of %s!",
                            volumeElm.toLog()));
                    return null;
                }
                volumeMods = volumeElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }
            if (volumeMods == null) {
                volumeMods = new ModsDefinition();
            }
            try {
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                Import issueImport = resolverEntities.createPeriodicalIssueImport(
                        titleMods, volumeMods, issueMods, null, xmlHandler, true, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerAgainEntity(urnnbnOldValue, issueImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, issueMods, issueDescription, issueModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isUpdateCzidloRecord()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                updateCzidloRecord(urnnbnOldValue, issueMods, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        }
        return null;
    }

    private Void processNdkMonographVolumeOrUnitOrSupplement(DigitalObjectElement elm, UrnNbnContext p)
            throws DigitalObjectException, VisitorException {

        final String pid = elm.getPid();
        final DigitalObjectHandler volumeHandler = elm.getHandler();
        final MetadataHandler<ModsDefinition> volumeModsHandler = volumeHandler.<ModsDefinition>metadata();
        final DescriptionMetadata<ModsDefinition> volumeDescription = volumeModsHandler.getMetadata();
        ModsDefinition volumeMods = volumeDescription.getData();
        ModsDefinition titleMods = null;
        // check URNNBN exists
        String urnnbnOldValue = ResolverUtils.getIdentifierValue("urnnbn", volumeMods);
        if (p.isRegisterNewPid()) {
            if (urnnbnOldValue != null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN exists.", urnnbnOldValue);
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            DigitalObjectElement titleElm = null;
            if (path.hasNext()) {
                titleElm = path.next();
                if (!NdkPlugin.MODEL_MONOGRAPHTITLE.equals(titleElm.getModelId())) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires Multipart Monograph or nothing as parent instead of %s!",
                            titleElm.toLog()));
                    return null;
                }
                titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }

            MixType mix = searchMix(elm, p);
            if (mix == null) {
                return null;
            }

            try {
                Import monographImport;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                if (titleElm == null) {
                    monographImport = resolverEntities.createMonographImport(volumeMods, mix, xmlHandler, false, false, null);
                } else {
                    monographImport = resolverEntities.createMultipartMonographImport(titleMods, volumeMods, mix, xmlHandler, false, null);
                }
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(monographImport, elm, p);
                updateModsWithUrnNbn(urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (SAXException ex) {
                // registration request not valid
                p.getStatus().error(elm, ex);
            }
        } else if (p.isInvalidateUrnNbn()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                invalidateUrnNbn(volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isCreateSuccessor()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            DigitalObjectElement titleElm = null;
            if (path.hasNext()) {
                titleElm = path.next();
                if (!NdkPlugin.MODEL_MONOGRAPHTITLE.equals(titleElm.getModelId())) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires Multipart Monograph or nothing as parent instead of %s!",
                            titleElm.toLog()));
                    return null;
                }
                titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }

            MixType mix = searchMix(elm, p);
            if (mix == null) {
                return null;
            }
            try {
                Import monographImport;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                if (titleElm == null) {
                    monographImport = resolverEntities.createMonographImport(volumeMods, mix, xmlHandler, false, false, urnnbnOldValue);
                } else {
                    monographImport = resolverEntities.createMultipartMonographImport(titleMods, volumeMods, mix, xmlHandler, false, urnnbnOldValue);
                }
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                RegistrarScopeIdentifier registrarScopeIdentifier = removeUuidFromObject(urnnbnOldValue, elm, p);
                if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                    p.getStatus().warning(elm, Status.EXCEPTION,
                            "The resolver returns no UUID value! Check the server configuration.", elm.getPid());
                    return null;
                }
                if (!(registrarScopeIdentifier.getType() + ":" + registrarScopeIdentifier.getValue()).equals(elm.getPid())) {
                    p.getStatus().warning(elm, Status.WRONG_UUID_DELETED, "Wrong Uuid deleted (expected: " +
                            elm.getPid() + " get: " + registrarScopeIdentifier.getValue(), elm.getPid());
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(monographImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (SAXException ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isRegisterAgainPid()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN does not exists.", urnnbnOldValue);
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            DigitalObjectElement titleElm = null;
            if (path.hasNext()) {
                titleElm = path.next();
                if (!NdkPlugin.MODEL_MONOGRAPHTITLE.equals(titleElm.getModelId())) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires Multipart Monograph or nothing as parent instead of %s!",
                            titleElm.toLog()));
                    return null;
                }
                titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }

            MixType mix = searchMix(elm, p);
            if (mix == null) {
                return null;
            }

            try {
                Import monographImport;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                if (titleElm == null) {
                    monographImport = resolverEntities.createMonographImport(volumeMods, mix, xmlHandler, false, false, null);
                } else {
                    monographImport = resolverEntities.createMultipartMonographImport(titleMods, volumeMods, mix, xmlHandler, false, null);
                }
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerAgainEntity(urnnbnOldValue, monographImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isUpdateCzidloRecord()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                updateCzidloRecord(urnnbnOldValue, volumeMods, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        }
        return null;
    }

    private Void processOldPrintMonographVolumeOrUnitOrSupplement(DigitalObjectElement elm, UrnNbnContext p)
            throws DigitalObjectException, VisitorException {

        final String pid = elm.getPid();
        final DigitalObjectHandler volumeHandler = elm.getHandler();
        final MetadataHandler<ModsDefinition> volumeModsHandler = volumeHandler.<ModsDefinition>metadata();
        final DescriptionMetadata<ModsDefinition> volumeDescription = volumeModsHandler.getMetadata();
        ModsDefinition volumeMods = volumeDescription.getData();
        ModsDefinition titleMods = null;
        // check URNNBN exists
        String urnnbnOldValue = ResolverUtils.getIdentifierValue("urnnbn", volumeMods);
        if (p.isRegisterNewPid()) {
            if (urnnbnOldValue != null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN exists.", urnnbnOldValue);
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            DigitalObjectElement titleElm = null;
            if (path.hasNext()) {
                titleElm = path.next();
                if (!(OldPrintPlugin.MODEL_MONOGRAPHTITLE.equals(titleElm.getModelId()) || OldPrintPlugin.MODEL_CONVOLUTTE.equals(titleElm.getModelId()))) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires Multipart Monograph or Convolutte or nothing as parent instead of %s!",
                            titleElm.toLog()));
                    return null;
                }
                titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }

            MixType mix = searchMix(elm, p);
            if (mix == null) {
                return null;
            }

            try {
                Import monographImport;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                if (titleElm == null) {
                    monographImport = resolverEntities.createMonographImport(volumeMods, mix, xmlHandler, false, true, null);
                } else {
                    monographImport = resolverEntities.createMultipartMonographImport(titleMods, volumeMods, mix, xmlHandler, true, null);
                }
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(monographImport, elm, p);
                updateModsWithUrnNbn(urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (SAXException ex) {
                // registration request not valid
                p.getStatus().error(elm, ex);
            }
        } else if (p.isInvalidateUrnNbn()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                invalidateUrnNbn(volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isCreateSuccessor()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            DigitalObjectElement titleElm = null;
            if (path.hasNext()) {
                titleElm = path.next();
                if (!(OldPrintPlugin.MODEL_MONOGRAPHTITLE.equals(titleElm.getModelId()) || OldPrintPlugin.MODEL_CONVOLUTTE.equals(titleElm.getModelId()))) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires Multipart Monograph or Convolutte or nothing as parent instead of %s!",
                            titleElm.toLog()));
                    return null;
                }
                titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }

            MixType mix = searchMix(elm, p);
            if (mix == null) {
                return null;
            }
            try {
                Import monographImport;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                if (titleElm == null) {
                    monographImport = resolverEntities.createMonographImport(volumeMods, mix, xmlHandler, false, true, urnnbnOldValue);
                } else {
                    monographImport = resolverEntities.createMultipartMonographImport(titleMods, volumeMods, mix, xmlHandler, true, urnnbnOldValue);
                }
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                RegistrarScopeIdentifier registrarScopeIdentifier = removeUuidFromObject(urnnbnOldValue, elm, p);
                if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                    p.getStatus().warning(elm, Status.EXCEPTION,
                            "The resolver returns no UUID value! Check the server configuration.", elm.getPid());
                    return null;
                }
                if (!(registrarScopeIdentifier.getType() + ":" + registrarScopeIdentifier.getValue()).equals(elm.getPid())) {
                    p.getStatus().warning(elm, Status.WRONG_UUID_DELETED, "Wrong Uuid deleted (expected: " +
                            elm.getPid() + " get: " + registrarScopeIdentifier.getValue(), elm.getPid());
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(monographImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (SAXException ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isRegisterAgainPid()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN does not exists.", urnnbnOldValue);
                return null;
            }
            Iterator<DigitalObjectElement> path = getCrawler().getReversePath(pid).iterator();
            DigitalObjectElement titleElm = null;
            if (path.hasNext()) {
                titleElm = path.next();
                if (!(OldPrintPlugin.MODEL_MONOGRAPHTITLE.equals(titleElm.getModelId()) || OldPrintPlugin.MODEL_CONVOLUTTE.equals(titleElm.getModelId()))) {
                    p.getStatus().error(elm, Status.UNEXPECTED_PARENT, String.format(
                            "Requires Multipart Monograph or Convolutte or nothing as parent instead of %s!",
                            titleElm.toLog()));
                    return null;
                }
                titleMods = titleElm.getHandler().<ModsDefinition>metadata().getMetadata().getData();
            }

            MixType mix = searchMix(elm, p);
            if (mix == null) {
                return null;
            }

            try {
                Import monographImport;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                if (titleElm == null) {
                    monographImport = resolverEntities.createMonographImport(volumeMods, mix, xmlHandler, false, true, null);
                } else {
                    monographImport = resolverEntities.createMultipartMonographImport(titleMods, volumeMods, mix, xmlHandler, true, null);
                }
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerAgainEntity(urnnbnOldValue, monographImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isUpdateCzidloRecord()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                updateCzidloRecord(urnnbnOldValue, volumeMods, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        }
        return null;
    }

    private Void processNdkEMonographVolumeOrSupplement(DigitalObjectElement elm, UrnNbnContext p)
            throws DigitalObjectException, VisitorException {

        final String pid = elm.getPid();
        final DigitalObjectHandler volumeHandler = elm.getHandler();
        final MetadataHandler<ModsDefinition> volumeModsHandler = volumeHandler.<ModsDefinition>metadata();
        final DescriptionMetadata<ModsDefinition> volumeDescription = volumeModsHandler.getMetadata();
        ModsDefinition volumeMods = volumeDescription.getData();
        // check URNNBN exists
        String urnnbnOldValue = ResolverUtils.getIdentifierValue("urnnbn", volumeMods);
        if (p.isRegisterNewPid()) {
            if (urnnbnOldValue != null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN exists.", urnnbnOldValue);
                return null;
            }
            try {
                Import eMonographImport;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                eMonographImport = resolverEntities.createMonographImport(volumeMods, null, xmlHandler, true, false, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(eMonographImport, elm, p);
                updateModsWithUrnNbn(urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (SAXException ex) {
                // registration request not valid
                p.getStatus().error(elm, ex);
            }
        } else if (p.isInvalidateUrnNbn()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                invalidateUrnNbn(volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
            return null;
        } else if (p.isCreateSuccessor()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                Import eMonographImport;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                eMonographImport = resolverEntities.createMonographImport(volumeMods, null, xmlHandler, true, false, urnnbnOldValue);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                RegistrarScopeIdentifier registrarScopeIdentifier = removeUuidFromObject(urnnbnOldValue, elm, p);
                if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                    p.getStatus().warning(elm, Status.EXCEPTION,
                            "The resolver returns no UUID value! Check the server configuration.", elm.getPid());
                    return null;
                }
                if (!(registrarScopeIdentifier.getType() + ":" + registrarScopeIdentifier.getValue()).equals(elm.getPid())) {
                    p.getStatus().warning(elm, Status.WRONG_UUID_DELETED, "Wrong Uuid deleted (expected: " +
                            elm.getPid() + " get: " + registrarScopeIdentifier.getValue(), elm.getPid());
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(eMonographImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (SAXException ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isRegisterAgainPid()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN does not exists.", urnnbnOldValue);
                return null;
            }
            try {
                Import eMonographImport;
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                eMonographImport = resolverEntities.createMonographImport(volumeMods, null, xmlHandler, true, false, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerAgainEntity(urnnbnOldValue, eMonographImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isUpdateCzidloRecord()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                updateCzidloRecord(urnnbnOldValue, volumeMods, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        }
        return null;
    }

    private Void processOtherEntity(DigitalObjectElement elm, String entityType, UrnNbnContext p, MixType mix)
            throws DigitalObjectException, VisitorException {

        final DigitalObjectHandler volumeHandler = elm.getHandler();
        final MetadataHandler<ModsDefinition> volumeModsHandler = volumeHandler.<ModsDefinition>metadata();
        final DescriptionMetadata<ModsDefinition> volumeDescription = volumeModsHandler.getMetadata();
        ModsDefinition volumeMods = volumeDescription.getData();
        // check URNNBN exists
        String urnnbnOldValue = ResolverUtils.getIdentifierValue("urnnbn", volumeMods);
        if (p.isRegisterNewPid()) {
            if (urnnbnOldValue != null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN exists.", urnnbnOldValue);
                return null;
            }
            if (mix == null) {
                mix = searchMix(elm, p);
                if (mix == null) {
                    return null;
                }
            }

            try {
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                Import entityImport = resolverEntities.createOtherEntityImport(volumeMods, entityType, mix, xmlHandler, false, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(entityImport, elm, p);
                updateModsWithUrnNbn(urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (SAXException ex) {
                // registration request not valid
                p.getStatus().error(elm, ex);
            }
        } else if (p.isInvalidateUrnNbn()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                invalidateUrnNbn(volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isCreateSuccessor()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            if (mix == null) {
                mix = searchMix(elm, p);
                if (mix == null) {
                    return null;
                }
            }
            try {
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                Import entityImport = resolverEntities.createOtherEntityImport(volumeMods, entityType, mix, xmlHandler, false, urnnbnOldValue);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                RegistrarScopeIdentifier registrarScopeIdentifier = removeUuidFromObject(urnnbnOldValue, elm, p);
                if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                    p.getStatus().warning(elm, Status.EXCEPTION,
                            "The resolver returns no UUID value! Check the server configuration.", elm.getPid());
                    return null;
                }
                if (!(registrarScopeIdentifier.getType() + ":" + registrarScopeIdentifier.getValue()).equals(elm.getPid())) {
                    p.getStatus().warning(elm, Status.WRONG_UUID_DELETED, "Wrong Uuid deleted (expected: " +
                            elm.getPid() + " get: " + registrarScopeIdentifier.getValue(), elm.getPid());
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(entityImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (SAXException ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isRegisterAgainPid()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN does not exists.", urnnbnOldValue);
                return null;
            }
            if (mix == null) {
                mix = searchMix(elm, p);
                if (mix == null) {
                    return null;
                }
            }

            try {
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                Import entityImport = resolverEntities.createOtherEntityImport(volumeMods, entityType, mix, xmlHandler, false, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerAgainEntity(urnnbnOldValue, entityImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isUpdateCzidloRecord()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                updateCzidloRecord(urnnbnOldValue, volumeMods, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        }
        return null;
    }

    private Void processEBornOtherEntity(DigitalObjectElement elm, String entityType, UrnNbnContext p)
            throws DigitalObjectException, VisitorException {

        final DigitalObjectHandler volumeHandler = elm.getHandler();
        final MetadataHandler<ModsDefinition> volumeModsHandler = volumeHandler.<ModsDefinition>metadata();
        final DescriptionMetadata<ModsDefinition> volumeDescription = volumeModsHandler.getMetadata();
        ModsDefinition volumeMods = volumeDescription.getData();
        // check URNNBN exists
        String urnnbnOldValue = ResolverUtils.getIdentifierValue("urnnbn", volumeMods);
        if (p.isRegisterNewPid()) {
            if (urnnbnOldValue != null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN exists.", urnnbnOldValue);
                return null;
            }

            try {
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                Import entityImport = resolverEntities.createOtherEntityImport(volumeMods, entityType, null, xmlHandler, true, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(entityImport, elm, p);
                updateModsWithUrnNbn(urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (SAXException ex) {
                // registration request not valid
                p.getStatus().error(elm, ex);
            }
        } else if (p.isInvalidateUrnNbn()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                invalidateUrnNbn(volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isCreateSuccessor()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                Import entityImport = resolverEntities.createOtherEntityImport(volumeMods, entityType, null, xmlHandler, true, urnnbnOldValue);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                RegistrarScopeIdentifier registrarScopeIdentifier = removeUuidFromObject(urnnbnOldValue, elm, p);
                if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                    p.getStatus().warning(elm, Status.EXCEPTION,
                            "The resolver returns no UUID value! Check the server configuration.", elm.getPid());
                    return null;
                }
                if (!(registrarScopeIdentifier.getType() + ":" + registrarScopeIdentifier.getValue()).equals(elm.getPid())) {
                    p.getStatus().warning(elm, Status.WRONG_UUID_DELETED, "Wrong Uuid deleted (expected: " +
                            elm.getPid() + " get: " + registrarScopeIdentifier.getValue(), elm.getPid());
                    return null;
                }
                UrnNbn urnNbnResponse = registerEntity(entityImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (SAXException ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isRegisterAgainPid()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_EXISTS, "URN:NBN does not exists.", urnnbnOldValue);
                return null;
            }
            try {
                ValidationErrorHandler xmlHandler = new ValidationErrorHandler();
                Import entityImport = resolverEntities.createOtherEntityImport(volumeMods, entityType, null, xmlHandler, true, null);
                if (!validateEntity(xmlHandler, elm, p)) {
                    return null;
                }
                UrnNbn urnNbnResponse = registerAgainEntity(urnnbnOldValue, entityImport, elm, p);
                updateModsWithSuccessorUrnNbn(urnnbnOldValue, urnNbnResponse, volumeMods, volumeDescription, volumeModsHandler, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        } else if (p.isUpdateCzidloRecord()) {
            if (urnnbnOldValue == null) {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return null;
            }
            try {
                updateCzidloRecord(urnnbnOldValue, volumeMods, elm, p);
            } catch (Exception ex) {
                p.getStatus().error(elm, ex);
            }
        }
        return null;
    }

    private boolean validateEntity(ValidationErrorHandler xmlHandler, DigitalObjectElement elm, UrnNbnContext p) {
        List<String> xmlErrors = xmlHandler.getValidationErrors();
        if (!xmlErrors.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            for (String xmlError : xmlErrors) {
                sb.append(xmlError).append('\n');
            }
            p.getStatus().error(elm, Status.XML_REQUEST_NOT_VALID, sb.toString());
            return false;
        }
        return true;
    }

    private UrnNbn registerEntity(Import entity, DigitalObjectElement elm, UrnNbnContext p) {
        try {
            cz.cas.lib.proarc.urnnbn.model.response.Response response = p.getClient().registerObject(entity);
            ErrorType error = response.getError();
            if (error != null) {
                // remote registration failed
                p.getStatus().error(elm, Status.EXCEPTION, error.getCode() + ": " + error.getMessage());
                LOG.log(Level.SEVERE, "{0}: {1}: {2}",
                        new Object[]{elm, error.getCode(), error.getMessage()});
                return null;
            }
            UrnNbn urnNbn = response.getUrnNbn();
            if (urnNbn == null || urnNbn.getValue() == null) {
                p.getStatus().error(elm, Status.EXCEPTION,
                        "The resolver returns no URN:NBN value! Check the server configuration.");
            }
            return urnNbn;
        } catch (Exception ex) {
            // unexpected remote registration failure
            p.getStatus().error(elm, ex);
            return null;
        }
    }

    private RegistrarScopeIdentifier removeUuidFromObject(String urnnbn, DigitalObjectElement elm, UrnNbnContext p) {
        try {
            cz.cas.lib.proarc.urnnbn.model.response.Response response = p.getClient().removeUuidCzidloRecord(urnnbn);
            ErrorType error = response.getError();
            if (error != null) {
                // remote registration failed
                p.getStatus().error(elm, Status.EXCEPTION, error.getCode() + ": " + error.getMessage());
                LOG.log(Level.SEVERE, "{0}: {1}: {2}",
                        new Object[]{elm, error.getCode(), error.getMessage()});
                return null;
            }
            RegistrarScopeIdentifier id = response.getId();
            return id;
        } catch (Exception ex) {
            // unexpected remote registration failure
            p.getStatus().error(elm, ex);
            return null;
        }
    }

    private UrnNbn registerAgainEntity(String urnNbnOldValue, Import entity, DigitalObjectElement elm, UrnNbnContext p) {
        try {
            cz.cas.lib.proarc.urnnbn.model.response.Response response = p.getClient().deactivateUrnNbnValue(urnNbnOldValue);
            ErrorType error = response.getError();
            if (error != null) {
                // remote deactivation failed
                LOG.log(Level.SEVERE, "{0}: {1}: {2}", new Object[]{elm, error.getCode(), error.getMessage()});
                if ("INCORRECT_URN_NBN_STATE".equals(error.getCode().name())) {
                    if (error.getMessage().endsWith("FREE")) {
                        p.getStatus().error(elm, Status.EXCEPTION, urnNbnOldValue + " was never used");
                    } else {
                        p.getStatus().error(elm, Status.EXCEPTION, "Already deactivated " + urnNbnOldValue);
                    }
                } else {
                    p.getStatus().error(elm, Status.EXCEPTION, error.getCode() + ": " + error.getMessage());
                }
                return null;
            }
            UrnNbn urnNbn = response.getUrnNbn();
            if (urnNbn == null || urnNbn.getValue() == null) {
                p.getStatus().error(urnNbnOldValue, Status.EXCEPTION,
                        "The resolver returns no URN:NBN value! Check the server configuration.");
            } else {
                if ("DEACTIVATED".equals(urnNbn.getStatus())) {
                    response = p.getClient().removeUuidCzidloRecord(urnNbnOldValue);
                    error = response.getError();
                    if (error != null) {
                        // remote registration failed
                        p.getStatus().error(elm, Status.EXCEPTION, error.getCode() + ": " + error.getMessage());
                        LOG.log(Level.SEVERE, "{0}: {1}: {2}",
                                new Object[]{elm, error.getCode(), error.getMessage()});
                        return null;
                    }
                    RegistrarScopeIdentifier registrarScopeIdentifier = response.getId();
                    if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                        p.getStatus().warning(elm, Status.EXCEPTION, "The resolver returns no UUID value! Check the server configuration.", urnNbnOldValue);
                    } else {
                        response = p.getClient().registerObject(entity);
                        error = response.getError();
                        if (error != null) {
                            // remote registration failed
                            p.getStatus().error(elm, Status.EXCEPTION, error.getCode() + ": " + error.getMessage());
                            LOG.log(Level.SEVERE, "{0}: {1}: {2}",
                                    new Object[]{elm, error.getCode(), error.getMessage()});
                            return null;
                        }
                        urnNbn = response.getUrnNbn();
                        if (urnNbn == null || urnNbn.getValue() == null) {
                            p.getStatus().error(elm, Status.EXCEPTION,
                                    "The resolver returns no URN:NBN value! Check the server configuration.");
                        }
                        return urnNbn;
                    }
                } else {
                    p.getStatus().error(elm, Status.EXCEPTION, urnNbn.getStatus() + ":" + urnNbn.getValue());
                }
            }
        } catch (Exception ex) {
            // unexpected remote registration failure
            p.getStatus().error(elm, ex);
        }
        return null;
    }

    private void updateCzidloRecord(String urnNbnValue, ModsDefinition elmMods, DigitalObjectElement elm, UrnNbnContext p) {
        try {
            String identifier = p.getUpdateCzidloRecordIdentifier();
            if ("isbn".equalsIgnoreCase(identifier) || "issn".equalsIgnoreCase(identifier) || "ccnb".equalsIgnoreCase(identifier) || "urnnbn".equalsIgnoreCase(identifier)) {
                p.getStatus().error(elm, Status.EXCEPTION, String.format("Identifier \"%s\" can not be updated", identifier));
                return;
            }
            if ("delete".equalsIgnoreCase(p.getUpdateCzidloRecordOperation())) {
                cz.cas.lib.proarc.urnnbn.model.response.Response response = p.getClient().removeIdentifierCzidloRecord(urnNbnValue, identifier);
                ErrorType error = response.getError();
                if (error != null) {
                    // remote registration failed
                    p.getStatus().error(elm, Status.EXCEPTION, error.getCode() + ": " + error.getMessage());
                    LOG.log(Level.SEVERE, "{0}: {1}: {2}",
                            new Object[]{elm, error.getCode(), error.getMessage()});
                    return;
                }
                RegistrarScopeIdentifier registrarScopeIdentifier = response.getId();
                if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                    p.getStatus().warning(elm, Status.EXCEPTION, "The resolver returns no value! Check the server configuration.", urnNbnValue);
                } else {
                    p.getStatus().ok(elm, urnNbnValue, String.format("Deleted \"%s\" from \"%s\".", identifier, urnNbnValue));
                }
                return;
            } else if ("update".equalsIgnoreCase(p.getUpdateCzidloRecordOperation())) {
                IdentifierDefinition identifierDefinition = ResolverUtils.getIdentifier(identifier, elmMods);
                if (identifierDefinition != null) {
                    cz.cas.lib.proarc.urnnbn.model.response.Response response = p.getClient().updateCzidloRecord(urnNbnValue, identifier, identifierDefinition.getValue());
                    ErrorType error = response.getError();
                    if (error != null) {
                        // remote registration failed
                        p.getStatus().error(elm, Status.EXCEPTION, error.getCode() + ": " + error.getMessage());
                        LOG.log(Level.SEVERE, "{0}: {1}: {2}",
                                new Object[]{elm, error.getCode(), error.getMessage()});
                        return;
                    }
                    RegistrarScopeIdentifier registrarScopeIdentifier = response.getId();
                    if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                        p.getStatus().warning(elm, Status.EXCEPTION, "The resolver returns no value! Check the server configuration.", urnNbnValue);
                    } else {
                        p.getStatus().ok(elm, urnNbnValue,
                                String.format("Updated \"%s\":\"%s\" for \"%s\".", identifier, identifierDefinition.getValue(), urnNbnValue));
                    }
                    return;
                } else {
                    p.getStatus().warning(elm, Status.EXCEPTION, String.format("\"%s\" does not exists", identifier), elm.getPid());
                }
            } else {
                p.getStatus().error(elm, Status.EXCEPTION, String.format("Unsupported operation %s", p.getUpdateCzidloRecordOperation()));
                return;
            }
        } catch (Exception ex) {
            // unexpected remote registration failure
            p.getStatus().error(elm, ex);
        }
        return;
    }

    private void updateModsWithUrnNbn(UrnNbn urnNbn,
                                      ModsDefinition elmMods,
                                      DescriptionMetadata<ModsDefinition> elmDescription,
                                      MetadataHandler<ModsDefinition> elmModsHandler,
                                      DigitalObjectElement elm, UrnNbnContext p
    ) {

        if (urnNbn == null) {
            return;
        }
        String urnnbn = urnNbn.getValue();
        if (urnnbn == null) {
            p.getStatus().error(elm, Status.EXCEPTION, "Missing URN:NBN in the resolver response!");
            return;
        }
        try {
            IdentifierDefinition urnNbnId = new IdentifierDefinition();
            urnNbnId.setType("urnnbn");
            urnNbnId.setValue(urnnbn);
            DigitalObjectHandler objectHandler = elm.getHandler();
            elmMods.getIdentifier().add(urnNbnId);
            elmDescription.setData(elmMods);
            elmModsHandler.setMetadata(elmDescription, "URN:NBN registration", NdkMetadataHandler.OPERATION_URNNBN);
            objectHandler.commit();
            p.getStatus().ok(elm, urnnbn);
        } catch (Exception ex) {
            // fatal error, stop further registrations!
            throw new IllegalStateException(elm.getPid() + ": Cannot write URN:NBN " + urnnbn, ex);
        }
    }

    private void updateModsWithSuccessorUrnNbn(String urnNbnValueOld, UrnNbn urnNbn,
                                               ModsDefinition elmMods,
                                               DescriptionMetadata<ModsDefinition> elmDescription,
                                               MetadataHandler<ModsDefinition> elmModsHandler,
                                               DigitalObjectElement elm, UrnNbnContext p
    ) {
        if (urnNbn == null) {
            return;
        }
        String urnnbn = urnNbn.getValue();
        if (urnnbn == null) {
            p.getStatus().error(elm, Status.EXCEPTION, "Missing URN:NBN in the resolver response!");
            return;
        }
        try {
            IdentifierDefinition urnNbnId = new IdentifierDefinition();
            urnNbnId.setType("urnnbn");
            urnNbnId.setValue(urnnbn);
            elmMods.getIdentifier().add(urnNbnId);

            IdentifierDefinition urnNbnIdOld = ResolverUtils.getIdentifierWithValue("urnnbn", urnNbnValueOld, elmMods);
            if (urnNbnIdOld != null) {
                urnNbnIdOld.setInvalid("yes");
            } else {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
                return;
            }
            DigitalObjectHandler objectHandler = elm.getHandler();
            elmDescription.setData(elmMods);
            elmModsHandler.setMetadata(elmDescription, "URN:NBN created successor", NdkMetadataHandler.OPERATION_URNNBN);
            objectHandler.commit();
            p.getStatus().ok(elm, urnnbn, urnNbnValueOld + " --> " + urnnbn);
        } catch (Exception ex) {
            // fatal error, stop further registrations!
            throw new IllegalStateException(elm.getPid() + "/" + urnNbnValueOld + ": Cannot create Successor URN:NBN " + urnnbn, ex);
        }

    }

    private void invalidateUrnNbn(ModsDefinition elmMods, DescriptionMetadata<ModsDefinition> elmDescription,
                                  MetadataHandler<ModsDefinition> elmModsHandler, DigitalObjectElement elm, UrnNbnContext p) {
        try {
            IdentifierDefinition identifierDefinition = ResolverUtils.getIdentifier("urnnbn", elmMods);
            if (identifierDefinition != null) {
                identifierDefinition.setInvalid("yes");
                DigitalObjectHandler objectHandler = elm.getHandler();
                elmDescription.setData(elmMods);
                elmModsHandler.setMetadata(elmDescription, "Invalidate URN:NBN", NdkMetadataHandler.OPERATION_URNNBN);
                objectHandler.commit();
                p.getStatus().ok(elm, "Invalid " + identifierDefinition.getValue());
            } else {
                p.getStatus().warning(elm, Status.URNNBN_DONT_EXISTS, "URN:NBN does not exists", elm.getPid());
            }
        } catch (Exception ex) {
            throw new IllegalStateException(elm.getPid() + ": Cannot invalidate URN:NBN", ex);
        }

    }

    private MixType searchMix(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        try {
            visitChildren(elm, p);
            // no page found
            p.getStatus().error(elm, Status.NO_PAGE_FOUND, "No technical metadata!");
            return null;
        } catch (StopOnFirstMixException ex) {
            return ex.getMix();
        }
    }

    private MixType searchMixElement(DigitalObjectElement elm, UrnNbnContext p) throws VisitorException {
        try {
            visitChildrenOnlyIf(elm, p, NdkPlugin.MODEL_PAGE, NdkPlugin.MODEL_NDK_PAGE);
            p.getStatus().error(elm, Status.NO_PAGE_FOUND, "No technical metadata!");
            return null;
        } catch (StopOnFirstMixException ex) {
            return ex.getMix();
        }
    }

    MixType getMix(DigitalObjectElement elm, UrnNbnContext p) {
        try {
            MixType mix = MixEditor.ndkArchival(elm.getHandler().getFedoraObject()).read();
            if (mix != null) {
                return mix;
            }
        } catch (DigitalObjectException ex) {
            throw new IllegalStateException(ex);
        }
        String pid = elm.getPid();
        Object entity = getPageImageStream(elm, p.getStatus(), BinaryEditor.NDK_ARCHIVAL_ID);
        if (entity != null) {
            JhoveContext jhoveCtx = p.getJhoveContext();
            File temp = null;
            try {
                File page = null;
                if (entity instanceof InputStream) {
                    page = temp = new File(jhoveCtx.getConfigFolder(), pid);
                    FileUtils.copyInputStreamToFile((InputStream) entity, page);
                } else if (entity instanceof File) {
                    page = (File) entity;
                } else {
                    throw new VisitorException("Unsupported entity: " + entity.getClass());
                }
                Mix mix = JhoveUtility.getMix(page, jhoveCtx, null, null, null).getMix();
                return mix;
            } catch (Exception ex) {
                throw new IllegalStateException(pid, ex);
            } finally {
                FileUtils.deleteQuietly(temp);
            }
        }
        return null;
    }

    Object getPageImageStream(DigitalObjectElement elm, UrnNbnStatusHandler status, String... streamIds) {
        // XXX replace with object profile check
        for (String streamId : streamIds) {
            Object stream = getPageImageStream(elm.getHandler(), streamId);
            if (stream != null) {
                return stream;
            } else {
                String msg = String.format("Missing expected datastream %s in \n%s", streamId, elm.toLog());
                status.warning(registeringObject, Status.MISSING_DATASTREAM, msg, null);
            }
        }
        return null;
    }

    Object getPageImageStream(DigitalObjectHandler pageObject, String streamId) {
        try {
            DisseminationHandler acDissemination = pageObject.dissemination(streamId);
            Response acResponse = acDissemination.getDissemination(null);
            if (acResponse.getStatus() == Response.Status.OK.getStatusCode()) {
                Object entity = acResponse.getEntity();
                return entity;
            } else if (acResponse.getStatus() == Response.Status.NOT_FOUND.getStatusCode()) {
                // XXX try another stream or page?
            } else {

            }
            return null;
        } catch (Exception ex) {
            throw new IllegalStateException(pageObject.getFedoraObject().getPid(), ex);
        }
    }

    /**
     * Throw the exception to stop processing other children when MIX is found.
     */
    private static class StopOnFirstMixException extends VisitorException {

        private MixType mix;

        public StopOnFirstMixException(MixType mix) {
            this.mix = mix;
        }

        public MixType getMix() {
            return mix;
        }
    }

}
