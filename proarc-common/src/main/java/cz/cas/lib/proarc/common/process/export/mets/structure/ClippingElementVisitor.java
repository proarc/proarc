package cz.cas.lib.proarc.common.process.export.mets.structure;

import cz.cas.lib.proarc.common.object.ndk.NdkClippingPlugin;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.mets.DivType;
import java.io.File;
import java.util.List;

/** Builds one NDK 0.1 fund-unit package from a collection branch. */
public class ClippingElementVisitor extends MetsElementVisitor {

    @Override
    public void insertIntoMets(IMetsElement element) throws MetsExportException {
        IMetsElement collection = element.getMetsContext().getRootElement();
        if (!Const.CLIPPING_COLLECTION.equals(collection.getElementType())) {
            throw new MetsExportException(element.getOriginalPid(), "A fund unit must belong to a clipping collection", false, null);
        }
        requireUuid(collection);
        requireGenre(collection, Const.GENRE_CLIPPING_COLLECTION);
        mainObjectModel = element.getModel().replace("info:fedora/", "");
        mainObjectPid = element.getOriginalPid();
        element.getMetsContext().getFileList().clear();
        mets = prepareMets(element);
        initHeader(element);
        String model = element.getModel().replace("info:fedora/", "");
        if (NdkClippingPlugin.MODEL_CLIPPING_UNIT.equals(model)) {
            mets.setTYPE("Clipping");
        } else if (NdkClippingPlugin.MODEL_CLIPPING_INDEX.equals(model)) {
            mets.setTYPE("Clipping index");
        } else if (NdkClippingPlugin.MODEL_CARD_INDEX.equals(model)) {
            mets.setTYPE("Card index");
        } else {
            throw new MetsExportException(element.getOriginalPid(), "Expected a clipping or card fund unit", false, null);
        }

        DivType collectionDiv = addLogicalDiv(null, collection);
        IMetsElement unit;
        List<IMetsElement> collectionChildren = collection.getChildren();
        if (collectionChildren.size() != 1) {
            throw new MetsExportException(element.getOriginalPid(), "Expected one fund unit or directory in the package", false, null);
        }
        IMetsElement child = collectionChildren.get(0);
        if (Const.CLIPPING_DIRECTORY.equals(child.getElementType())) {
            requireUuid(child);
            requireGenre(child, Const.GENRE_CLIPPING_DIRECTORY);
            DivType directoryDiv = addLogicalDiv(collectionDiv, child);
            if (child.getChildren().size() != 1) {
                throw new MetsExportException(element.getOriginalPid(), "Expected one fund unit in the directory package", false, null);
            }
            unit = child.getChildren().get(0);
            addLogicalDiv(directoryDiv, unit);
        } else {
            unit = child;
            addLogicalDiv(collectionDiv, unit);
        }
        if (!Const.CLIPPING_UNIT.equals(unit.getElementType())) {
            throw new MetsExportException(unit.getOriginalPid(), "Expected a clipping or card fund unit", false, null);
        }
        requireUuid(unit);
        String requiredGenre = NdkClippingPlugin.MODEL_CLIPPING_UNIT.equals(model)
                ? Const.GENRE_CLIPPING_UNIT
                : NdkClippingPlugin.MODEL_CLIPPING_INDEX.equals(model)
                ? Const.GENRE_CLIPPING_INDEX : Const.GENRE_CARD_INDEX;
        requireGenre(unit, requiredGenre);
        if (unit.getChildren().isEmpty()) {
            throw new MetsExportException(unit.getOriginalPid(), "A fund unit must have at least one page", false, null);
        }

        element.getMetsContext().setPackageID(MetsUtils.getPackageID(unit, element.getIgnoreMissingUrnNbn()));
        DivType physicalUnit = new DivType();
        physicalUnit.setTYPE(Const.CLIPPING_UNIT);
        physicalUnit.setID("PHYSICAL_" + unit.getElementID());
        physicalUnit.setLabel3(unit.getLabel());
        physicalUnit.getDMDID().add(unit.getModsMetsElement());
        physicalStruct.setDiv(physicalUnit);

        int pageIndex = 1;
        for (IMetsElement page : unit.getChildren()) {
            if (!Const.PAGE.equals(page.getElementType())) {
                throw new MetsExportException(page.getOriginalPid(), "Expected page under fund unit", false, null);
            }
            requireUuid(page);
            insertPage(physicalUnit, page, pageCounter++, unit, pageIndex++);
            if (page.getModsMetsElement() != null) {
                physicalUnit.getDiv().get(physicalUnit.getDiv().size() - 1)
                        .getDMDID().add(page.getModsMetsElement());
            }
        }
        if (element.getMetsContext().getPackageDir() == null) {
            element.getMetsContext().setPackageDir(createPackageDir(element));
        }
        File output = new File(element.getMetsContext().getPackageDir(),
                "mets_" + MetsUtils.removeNonAlpabetChars(element.getMetsContext().getPackageID()) + ".xml");
        saveMets(mets, output, element);
    }

    private DivType addLogicalDiv(DivType parent, IMetsElement element) {
        addDmdSec(element);
        DivType div = new DivType();
        div.setTYPE(element.getElementType());
        div.setID(element.getElementID());
        div.setLabel3(element.getLabel());
        div.getDMDID().add(element.getModsMetsElement());
        if (parent == null) {
            logicalStruct.setDiv(div);
        } else {
            parent.getDiv().add(div);
        }
        return div;
    }

    private void requireUuid(IMetsElement element) throws MetsExportException {
        String uuid = element.getModsIdentifiers().get(Const.UUID);
        if (uuid == null || uuid.isBlank()) {
            throw new MetsExportException(element.getOriginalPid(), "UUID is required at every fund level", false, null);
        }
    }

    private void requireGenre(IMetsElement element, String genre) throws MetsExportException {
        String xpath = "boolean(*[local-name()='mods']/*[local-name()='genre' and text()='" + genre + "'])";
        if (!"true".equals(MetsUtils.xPathEvaluateString(element.getModsStream(), xpath))) {
            throw new MetsExportException(element.getOriginalPid(), "Required MODS genre: " + genre, false, null);
        }
    }
}
