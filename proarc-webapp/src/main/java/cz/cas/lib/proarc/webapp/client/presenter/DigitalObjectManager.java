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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.presenter;

import com.google.gwt.core.client.Callback;
import com.google.gwt.place.shared.PlaceController;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.Offline;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.IconMenuButton;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.CopyObjectAction;
import cz.cas.lib.proarc.webapp.client.action.DeleteAction;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectEditAction;
import cz.cas.lib.proarc.webapp.client.action.FoxmlViewAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.TreeExpandAction;
import cz.cas.lib.proarc.webapp.client.action.UrnNbnAction;
import cz.cas.lib.proarc.webapp.client.action.administration.GenerateMasterCopyAction;
import cz.cas.lib.proarc.webapp.client.action.administration.RestoreAction;
import cz.cas.lib.proarc.webapp.client.action.administration.UpdateAllObjectsAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.bdm.ChangeBdmArticleToNdkEArticleAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.clippings.ChangeClippingsTitleToNdkMonographTitleAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.clippings.ChangeClippingsVolumeToNdkMonographVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.k4.ChangeK4MonographToNdkMonographVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.k4.ChangeK4MonographUnitToNdkMonographUnitAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.k4.ChangeK4MonographUnitToNdkMonographVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.k4.ChangeK4PeriodicalIssueToNdkPeriodicalIssueAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.k4.ChangeK4PeriodicalToNdkPeriodicalAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.k4.ChangeK4PeriodicalVolumeToNdkPeriodicalVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkArticleToNdkEArticleAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkCartographicToOldprintCartographicAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkChapterToOldprintChapterAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkMonographSupplementToOldprintMonographSupplementAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkMonographTitleToClippingsTitleAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkMonographTitleToNdkMonographVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkMonographUnitToNdkMonographVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkMonographVolumeToClippingsVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkMonographVolumeToNdkMonographTitleAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkMonographVolumeToNdkMonographUnitAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkMonographVolumeToOldprintMonographVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkMusicsheetToOldprintMusicsheetAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkPeriodicalIssueToNdkEPeriodicalIssueAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkPeriodicalSupplementToNdkEPeriodicalSupplementAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkPeriodicalToNdkEPeriodicalAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkPeriodicalVolumeToNdkEPeriodicalVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndk.ChangeNdkPictureToOldprintGraphicAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndke.ChangeNdkEArticleToBdmArticleAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndke.ChangeNdkEArticleToNdkArticleAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndke.ChangeNdkEPeriodicalIssueToNdkPeriodicalIssueAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndke.ChangeNdkEPeriodicalSupplementToNdkPeriodicalSupplementAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndke.ChangeNdkEPeriodicalToNdkPeriodicalAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.ndke.ChangeNdkEPeriodicalVolumeToNdkPeriodicalVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintCartographicToNdkCartographicAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintChapterToNdkChapterAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintGraphicToNdkPictureAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintGraphicToOldPrintMonographVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintMonographSupplementToNdkMonographSupplementAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintMonographUnitToOldPrintMonographVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintMonographVolumeToNdkMonographVolumeAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintMonographVolumeToOldPrintGraphicAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintMonographVolumeToOldPrintMonographUnitAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintMonographVolumeToOldPrintMusicSheetAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.oldprint.ChangeOldprintMusicsheetToNdkMusicsheetAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.page.ChangeNdkPageToPageAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.page.ChangeNdkPageToSttPageAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.page.ChangePageToNdkPageAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.page.ChangePageToSttPageAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.page.ChangeSttPageToNdkPageAction;
import cz.cas.lib.proarc.webapp.client.action.administration.changeModels.page.ChangeSttPageToPageAction;
import cz.cas.lib.proarc.webapp.client.action.administration.indexObjects.IndexAllObjectsAction;
import cz.cas.lib.proarc.webapp.client.action.administration.lockModels.LockObjectAction;
import cz.cas.lib.proarc.webapp.client.action.administration.lockModels.UnlockObjectAction;
import cz.cas.lib.proarc.webapp.client.action.administration.updateModels.UpdateNdkArticleAction;
import cz.cas.lib.proarc.webapp.client.action.administration.updateModels.UpdateNdkPageAction;
import cz.cas.lib.proarc.webapp.client.action.administration.updateModels.UpdateOldprintPageAction;
import cz.cas.lib.proarc.webapp.client.action.export.ArchiveExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.ArchiveOldPrintExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.CejshExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.ChronicleExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.CrossrefExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.DataStreamExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.DesaExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.KWISExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.KrameriusExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.NdkExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.NdkOldPrintExportAction;
import cz.cas.lib.proarc.webapp.client.action.export.NdkSipExportAction;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectSearchView;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectTreeView;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import java.util.LinkedHashMap;

/**
 * The component allows to search digital objects and perform actions on
 * search results.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectManager {

    public static final String LAST_SELECTED_MODEL_TAG = "DigitalObjectManager_LastSelectedModel";

    private final ClientMessages i18n;
    private final PlaceController places;
    private final VLayout widget;
    private final DigitalObjectSearchView foundView;
    private final DigitalObjectTreeView treeView;
    private FoxmlViewAction foxmlAction;
    private ArchiveExportAction archiveExportAction;
    private ArchiveOldPrintExportAction archiveOldPrintExportAction;
    private KrameriusExportAction krameriusExportAction;
    private KWISExportAction kwisExportAction;
    private NdkExportAction ndkExportAction;
    private NdkSipExportAction ndkSipExportAction;
    private ChronicleExportAction chronicleExportAction;
    private NdkOldPrintExportAction ndkOldPrintExportAction;
    private CejshExportAction cejshExportAction;
    private CrossrefExportAction crossrefExportAction;
    private DesaExportAction desaDownloadAction;
    private DesaExportAction desaExportAction;
    private DataStreamExportAction fullDataStreamExportAction;
    private DataStreamExportAction rawDataStreamExportAction;
    private DataStreamExportAction ndkUserDataStreamExportAction;
    private DeleteAction deleteAction;
    private RestoreAction restoreAction;
    private UpdateAllObjectsAction updateAllObjectsAction;
    private IndexAllObjectsAction indexAllObjectsAction;
    private DigitalObjectEditAction ocrEditAction;
    private DigitalObjectEditAction noteEditAction;
    private DigitalObjectEditAction modsEditAction;
    private DigitalObjectEditAction parentEditAction;
    private DigitalObjectEditAction mediaEditAction;
    private DigitalObjectEditAction childrenEditAction;
    private DigitalObjectEditAction atmEditAction;
    private DigitalObjectEditAction technicalMetadataAction;
    private UrnNbnAction registerUrnNbnAction;
    private CopyObjectAction copyObjectAction;
    private LockObjectAction lockObjectAction;
    private UnlockObjectAction unlockObjectAction;
    private GenerateMasterCopyAction generateMasterCopyAction;
    private ChangePageToNdkPageAction changePageToNdkPageAction;
    private ChangeNdkPageToPageAction changeNdkPageToPageAction;
    private ChangePageToSttPageAction changePageToSttPageAction;
    private ChangeSttPageToPageAction changeSttPageToPageAction;
    private ChangeSttPageToNdkPageAction changeSttPageToNdkPageAction;
    private ChangeNdkPageToSttPageAction changeNdkPageToSttPageAction;
    private ChangeClippingsVolumeToNdkMonographVolumeAction changeClippingsVolumeToNdkMonographVolumeAction;
    private ChangeClippingsTitleToNdkMonographTitleAction changeClippingsTitleToNdkMonographTitleAction;
    private ChangeNdkMonographVolumeToClippingsVolumeAction changeNdkMonographVolumeToClippingsVolumeAction;
    private ChangeNdkMonographVolumeToNdkMonographTitleAction changeNdkMonographVolumeToNdkMonographTitleAction;
    private ChangeNdkMonographVolumeToNdkMonographUnitAction changeNdkMonographVolumeToNdkMonographUnitAction;
    private ChangeNdkMonographTitleToClippingsTitleAction changeNdkMonographTitleToClippingsTitleAction;
    private ChangeNdkMonographTitleToNdkMonographVolumeAction changeNdkMonographTitleToNdkMonographVolumeAction;
    private ChangeNdkMonographUnitToNdkMonographVolumeAction changeNdkMonographUnitToNdkMonographVolumeAction;
    private ChangeK4PeriodicalIssueToNdkPeriodicalIssueAction changeK4PeriodicalIssueToNdkPeriodicalIssueAction;
    private ChangeK4PeriodicalVolumeToNdkPeriodicalVolumeAction changeK4PeriodicalVolumeToNdkPeriodicalVolumeAction;
    private ChangeK4MonographToNdkMonographVolumeAction changeK4MonographToNdkMonographVolumeAction;
    private ChangeK4MonographUnitToNdkMonographVolumeAction changeK4MonographUnitToNdkMonographVolumeAction;
    private ChangeK4MonographUnitToNdkMonographUnitAction changeK4MonographUnitToNdkMonographUnitAction;
    private ChangeK4PeriodicalToNdkPeriodicalAction changeK4PeriodicalToNdkPeriodicalAction;
    private ChangeNdkCartographicToOldprintCartographicAction changeNdkCartographicToOldprintCartographicAction;
    private ChangeNdkChapterToOldprintChapterAction changeNdkChapterToSttChapterAction;
    private ChangeNdkMonographSupplementToOldprintMonographSupplementAction changeNdkMonographSupplementToOldprintMonographSupplementAction;
    private ChangeNdkMonographVolumeToOldprintMonographVolumeAction changeNdkMonographVolumeToOldprintMonographVolumeAction;
    private ChangeNdkMusicsheetToOldprintMusicsheetAction changeNdkMusicsheetToOldprintMusicsheetAction;
    private ChangeNdkPictureToOldprintGraphicAction changeNdkPictureToOldprintGraphicAction;
    private ChangeOldprintChapterToNdkChapterAction changeOldprintChapterToNdkChapterAction;
    private ChangeOldprintGraphicToNdkPictureAction changeOldprintGraphicToNdkPictureAction;
    private ChangeOldprintCartographicToNdkCartographicAction changeOldprintMapToNdkMapAction;
    private ChangeOldprintMonographVolumeToNdkMonographVolumeAction changeOldprintMonographToNdkMonographAction;
    private ChangeOldprintMonographVolumeToOldPrintMusicSheetAction changeOldprintMonographVolumeToOldPrintMusicSheetAction;
    private ChangeOldprintMonographVolumeToOldPrintGraphicAction changeOldprintMonographVolumeToOldPrintGraphicAction;
    private ChangeOldprintMonographVolumeToOldPrintMonographUnitAction changeOldprintMonographVolumeToOldPrintMonographUnitAction;
    private ChangeOldprintMonographUnitToOldPrintMonographVolumeAction changeOldprintMonographUnitToOldPrintMonographVolumeAction;
    private ChangeOldprintGraphicToOldPrintMonographVolumeAction changeOldprintGraphicToOldprintMonographVolumeAction;
    private ChangeOldprintMusicsheetToNdkMusicsheetAction changeOldprintMusicsheetToNdkMusicsheetAction;
    private ChangeOldprintMonographSupplementToNdkMonographSupplementAction changeOldprintSupplementToNdkSupplementAction;
    private ChangeNdkPeriodicalToNdkEPeriodicalAction changeNdkPeriodicalToNdkEPeriodicalAction;
    private ChangeNdkPeriodicalVolumeToNdkEPeriodicalVolumeAction changeNdkPeriodicalVolumeToNdkEPeriodicalVolumeAction;
    private ChangeNdkPeriodicalIssueToNdkEPeriodicalIssueAction changeNdkPeriodicalIssueToNdkEPeriodicalIssueAction;
    private ChangeNdkPeriodicalSupplementToNdkEPeriodicalSupplementAction changeNdkPeriodicalSupplementToNdkEPeriodicalSupplementAction;
    private ChangeNdkArticleToNdkEArticleAction changeNdkArticleToNdkEArticleAction;
    private ChangeBdmArticleToNdkEArticleAction changeBdmArticleToNdkEArticleAction;
    private ChangeNdkEPeriodicalToNdkPeriodicalAction changeNdkEPeriodicalToNdkPeriodicalAction;
    private ChangeNdkEPeriodicalVolumeToNdkPeriodicalVolumeAction changeNdkEPeriodicalVolumeToNdkPeriodicalVolumeAction;
    private ChangeNdkEPeriodicalIssueToNdkPeriodicalIssueAction changeNdkEPeriodicalIssueToNdkPeriodicalIssueAction;
    private ChangeNdkEPeriodicalSupplementToNdkPeriodicalSupplementAction changeNdkEPeriodicalSupplementToNdkPeriodicalSupplementAction;
    private ChangeNdkEArticleToNdkArticleAction changeNdkEArticleToNdkArticleAction;
    private ChangeNdkEArticleToBdmArticleAction changeNdkEArticleToBdmArticleAction;
    private UpdateNdkArticleAction updateNdkArticleAction;
    private UpdateNdkPageAction updateNdkPageAction;
    private UpdateOldprintPageAction updateOldprintPageAction;
    private TreeExpandAction expandTreeAction;
    private boolean initialized;

    public DigitalObjectManager(ClientMessages i18n, PlaceController places) {
        this.i18n = i18n;
        this.places = places;

        Label lblHeader = new Label();
        String title = ClientUtils.format("<b>%s</b>", i18n.DigitalObjectManager_Title());
        lblHeader.setContents(title);
        lblHeader.setAutoHeight();
        lblHeader.setPadding(4);
        lblHeader.setStyleName(Editor.CSS_PANEL_DESCRIPTION_TITLE);

        widget = new VLayout(4);
        widget.setWidth100();
        widget.setHeight100();

        foundView = new DigitalObjectSearchView(i18n, LAST_SELECTED_MODEL_TAG);
        foundView.getGrid().setSelectionType(SelectionStyle.MULTIPLE);
        final ActionSource listSource = new ActionSource(foundView);
        foundView.getGrid().addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                final Record[] selectedRecords = foundView.getSelection();
                int selectedRecordNumber = selectedRecords != null ? selectedRecords.length : 0;
                listSource.fireEvent();
                if (selectedRecordNumber == 1) {
                    String pid = selectedRecords[0].getAttribute(RelationDataSource.FIELD_PID);
                    treeView.setRoot(pid);
                } else if (selectedRecordNumber == 0) {
                    treeView.setRoot(null);
                }
            }
        });

        treeView = new DigitalObjectTreeView(i18n);
        treeView.getTree().setSelectionType(SelectionStyle.MULTIPLE);
        final ActionSource treeSource = new ActionSource(treeView);
        treeView.getTree().addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                treeSource.fireEvent();
            }
        });

        widget.addMember(lblHeader);
        Canvas foundViewWidget = foundView.asWidget();
        foundViewWidget.setShowResizeBar(true);
        widget.addMember(foundViewWidget);
        widget.addMember(treeView.asWidget());
        createActions();
        initToolbar(foundView.getToolbar(), listSource, MenuType.FOUND_VIEW);
        initToolbar(treeView.getToolbar(), treeSource, MenuType.TREE_VIEW);
        initContextMenu(foundView.getGrid().getContextMenu(), listSource);
        initContextMenu(treeView.getTree().getContextMenu(), treeSource);
        Actions.fixListGridContextMenu(foundView.getGrid());
        Actions.fixListGridContextMenu(treeView.getTree());
    }

    public void init() {
        if (initialized) {
            return;
        }
        initialized = true;
        fetchModels(false);
        foundView.onShow();
        treeView.setRoot(null);
    }

    private void fetchModels(final boolean reload) {
        MetaModelDataSource.getModels(reload, new Callback<ResultSet, Void>() {

            @Override
            public void onFailure(Void reason) {
            }

            @Override
            public void onSuccess(ResultSet modelResultSet) {
                LinkedHashMap<?, ?> valueMap = ClientUtils.getValueMap(modelResultSet,
                        MetaModelDataSource.FIELD_PID, MetaModelDataSource.FIELD_DISPLAY_NAME);
                treeView.setModels(valueMap);
                foundView.setModels(valueMap);
                if (!reload) {
                    //issue #499
                    Object previousId = Offline.get(LAST_SELECTED_MODEL_TAG);
                    if (previousId != null) {
                        foundView.setFilterModel(previousId);
                    } else if (!valueMap.isEmpty()) {
                        // init the view filter with the first modelId on first show
                        Object firstModel = valueMap.keySet().iterator().next();
                        foundView.setFilterModel(firstModel);
                    }
                    Object previousSort = Offline.get(LAST_SELECTED_MODEL_TAG + "_sort");
                    if (previousSort != null) {
                        foundView.setSort(previousSort);
                    } else {
                        foundView.setSort("asc");
                    }
                    foundView.refresh();
                }
            }
        });
    }

    public VLayout getUI() {
        return widget;
    }

    private void createActions() {
        archiveExportAction = new ArchiveExportAction(i18n);
        archiveOldPrintExportAction = new ArchiveOldPrintExportAction(i18n);
        foxmlAction = new FoxmlViewAction(i18n);
        krameriusExportAction = new KrameriusExportAction(i18n);
        kwisExportAction = new KWISExportAction(i18n);
        ndkExportAction = new NdkExportAction(i18n);
        ndkOldPrintExportAction = new NdkOldPrintExportAction(i18n);
        ndkSipExportAction = new NdkSipExportAction(i18n);
        chronicleExportAction = new ChronicleExportAction(i18n);
        cejshExportAction = new CejshExportAction(i18n);
        crossrefExportAction = new CrossrefExportAction(i18n);
        desaExportAction = DesaExportAction.export(i18n);
        desaDownloadAction = DesaExportAction.download(i18n);
        fullDataStreamExportAction = DataStreamExportAction.full(i18n);
        rawDataStreamExportAction = DataStreamExportAction.raw(i18n);
        ndkUserDataStreamExportAction = DataStreamExportAction.ndkUser(i18n);
        deleteAction = new DeleteAction(DigitalObjectDataSource.createDeletable(),
                DigitalObjectDataSource.createDeleteOptionsForm(), i18n);
        restoreAction = new RestoreAction(DigitalObjectDataSource.createRestorable(), i18n);
        updateAllObjectsAction = new UpdateAllObjectsAction(i18n);
        indexAllObjectsAction = new IndexAllObjectsAction(i18n);
        ocrEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabOcr_Title(), DatastreamEditorType.OCR, i18n);
        noteEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabNote_Title(),
                i18n.ImportBatchItemEditor_TabNote_Hint(),
                null,
                DatastreamEditorType.NOTE, places);
        modsEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabMods_Title(), DatastreamEditorType.MODS, i18n);
        parentEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_ParentAction_Title(), DatastreamEditorType.PARENT, i18n);
        mediaEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_MediaAction_Title(),
                i18n.DigitalObjectEditor_MediaAction_Hint(),
                null,
                DatastreamEditorType.MEDIA, places);
        childrenEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_ChildrenAction_Title(),
                i18n.DigitalObjectEditor_ChildrenAction_Hint(),
                null,
                DatastreamEditorType.CHILDREN, places);
        atmEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_AdministrationAction_Title(),
                i18n.DigitalObjectEditor_AdministrationAction_Hint(),
                null,
                DatastreamEditorType.ATM, places);
        technicalMetadataAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_TabTechnical_Title(),
                i18n.DigitalObjectEditor_TabTechnical_Hint(),
                null,
                DatastreamEditorType.TECHNICAL,
                new DigitalObjectEditAction.AcceptFilter(false, false),
                places);
        registerUrnNbnAction = new UrnNbnAction(i18n);
        copyObjectAction = new CopyObjectAction(i18n);
        lockObjectAction = new LockObjectAction(i18n);
        unlockObjectAction = new UnlockObjectAction(i18n);
        generateMasterCopyAction = new GenerateMasterCopyAction(i18n);
        changePageToNdkPageAction = new ChangePageToNdkPageAction(i18n);
        changeNdkPageToPageAction = new ChangeNdkPageToPageAction(i18n);
        changePageToSttPageAction = new ChangePageToSttPageAction(i18n);
        changeSttPageToPageAction = new ChangeSttPageToPageAction(i18n);
        changeSttPageToNdkPageAction = new ChangeSttPageToNdkPageAction(i18n);
        changeNdkPageToSttPageAction = new ChangeNdkPageToSttPageAction(i18n);
        changeClippingsVolumeToNdkMonographVolumeAction = new ChangeClippingsVolumeToNdkMonographVolumeAction(i18n);
        changeClippingsTitleToNdkMonographTitleAction = new ChangeClippingsTitleToNdkMonographTitleAction(i18n);
        changeNdkMonographVolumeToClippingsVolumeAction = new ChangeNdkMonographVolumeToClippingsVolumeAction(i18n);
        changeNdkMonographVolumeToNdkMonographTitleAction = new ChangeNdkMonographVolumeToNdkMonographTitleAction(i18n);
        changeNdkMonographVolumeToNdkMonographUnitAction = new ChangeNdkMonographVolumeToNdkMonographUnitAction(i18n);
        changeNdkMonographTitleToClippingsTitleAction = new ChangeNdkMonographTitleToClippingsTitleAction(i18n);
        changeNdkMonographTitleToNdkMonographVolumeAction = new ChangeNdkMonographTitleToNdkMonographVolumeAction(i18n);
        changeNdkMonographUnitToNdkMonographVolumeAction = new ChangeNdkMonographUnitToNdkMonographVolumeAction(i18n);
        changeK4PeriodicalIssueToNdkPeriodicalIssueAction = new ChangeK4PeriodicalIssueToNdkPeriodicalIssueAction(i18n);
        changeK4PeriodicalVolumeToNdkPeriodicalVolumeAction = new ChangeK4PeriodicalVolumeToNdkPeriodicalVolumeAction(i18n);
        changeK4MonographToNdkMonographVolumeAction = new ChangeK4MonographToNdkMonographVolumeAction(i18n);
        changeK4MonographUnitToNdkMonographVolumeAction = new ChangeK4MonographUnitToNdkMonographVolumeAction(i18n);
        changeK4MonographUnitToNdkMonographUnitAction = new ChangeK4MonographUnitToNdkMonographUnitAction(i18n);
        changeK4PeriodicalToNdkPeriodicalAction = new ChangeK4PeriodicalToNdkPeriodicalAction(i18n);
        changeNdkCartographicToOldprintCartographicAction = new ChangeNdkCartographicToOldprintCartographicAction(i18n);
        changeNdkChapterToSttChapterAction = new ChangeNdkChapterToOldprintChapterAction(i18n);
        changeNdkMonographSupplementToOldprintMonographSupplementAction = new ChangeNdkMonographSupplementToOldprintMonographSupplementAction(i18n);
        changeNdkMonographVolumeToOldprintMonographVolumeAction = new ChangeNdkMonographVolumeToOldprintMonographVolumeAction(i18n);
        changeNdkMusicsheetToOldprintMusicsheetAction = new ChangeNdkMusicsheetToOldprintMusicsheetAction(i18n);
        changeNdkPictureToOldprintGraphicAction = new ChangeNdkPictureToOldprintGraphicAction(i18n);
        changeOldprintChapterToNdkChapterAction = new ChangeOldprintChapterToNdkChapterAction(i18n);
        changeOldprintGraphicToNdkPictureAction = new ChangeOldprintGraphicToNdkPictureAction(i18n);
        changeOldprintMapToNdkMapAction = new ChangeOldprintCartographicToNdkCartographicAction(i18n);
        changeOldprintMonographToNdkMonographAction = new ChangeOldprintMonographVolumeToNdkMonographVolumeAction(i18n);
        changeOldprintMusicsheetToNdkMusicsheetAction = new ChangeOldprintMusicsheetToNdkMusicsheetAction(i18n);
        changeOldprintSupplementToNdkSupplementAction = new ChangeOldprintMonographSupplementToNdkMonographSupplementAction(i18n);
        changeOldprintMonographVolumeToOldPrintGraphicAction = new ChangeOldprintMonographVolumeToOldPrintGraphicAction(i18n);
        changeOldprintMonographVolumeToOldPrintMonographUnitAction = new ChangeOldprintMonographVolumeToOldPrintMonographUnitAction(i18n);
        changeOldprintMonographUnitToOldPrintMonographVolumeAction = new ChangeOldprintMonographUnitToOldPrintMonographVolumeAction(i18n);
        changeOldprintMonographVolumeToOldPrintMusicSheetAction = new ChangeOldprintMonographVolumeToOldPrintMusicSheetAction(i18n);
        changeNdkPeriodicalToNdkEPeriodicalAction = new ChangeNdkPeriodicalToNdkEPeriodicalAction(i18n);
        changeNdkPeriodicalVolumeToNdkEPeriodicalVolumeAction = new ChangeNdkPeriodicalVolumeToNdkEPeriodicalVolumeAction(i18n);
        changeNdkPeriodicalIssueToNdkEPeriodicalIssueAction = new ChangeNdkPeriodicalIssueToNdkEPeriodicalIssueAction(i18n);
        changeNdkPeriodicalSupplementToNdkEPeriodicalSupplementAction = new ChangeNdkPeriodicalSupplementToNdkEPeriodicalSupplementAction(i18n);
        changeNdkArticleToNdkEArticleAction = new ChangeNdkArticleToNdkEArticleAction(i18n);
        changeBdmArticleToNdkEArticleAction = new ChangeBdmArticleToNdkEArticleAction(i18n);
        changeNdkEPeriodicalToNdkPeriodicalAction = new ChangeNdkEPeriodicalToNdkPeriodicalAction(i18n);
        changeNdkEPeriodicalVolumeToNdkPeriodicalVolumeAction = new ChangeNdkEPeriodicalVolumeToNdkPeriodicalVolumeAction(i18n);
        changeNdkEPeriodicalIssueToNdkPeriodicalIssueAction = new ChangeNdkEPeriodicalIssueToNdkPeriodicalIssueAction(i18n);
        changeNdkEPeriodicalSupplementToNdkPeriodicalSupplementAction = new ChangeNdkEPeriodicalSupplementToNdkPeriodicalSupplementAction(i18n);
        changeNdkEArticleToNdkArticleAction = new ChangeNdkEArticleToNdkArticleAction(i18n);
        changeNdkEArticleToBdmArticleAction = new ChangeNdkEArticleToBdmArticleAction(i18n);

        updateNdkArticleAction = new UpdateNdkArticleAction(i18n);
        updateNdkPageAction = new UpdateNdkPageAction(i18n);
        updateOldprintPageAction = new UpdateOldprintPageAction(i18n);
        expandTreeAction = new TreeExpandAction(
                i18n,
                treeView);
    }

    /**
     * export (Kramerius, Datastream), edit(MODS, Hierarchy), delete, view (Datastream)
     */
    private void initToolbar(ToolStrip toolbar, ActionSource actionSource, MenuType menuType) {
        final AbstractAction exportMenuAction = new AbstractAction(
                i18n.ExportsAction_Title(), "[SKIN]/actions/save.png", null) {

            @Override
            public boolean accept(ActionEvent event) {
                Object[] selection = Actions.getSelection(event);
                return selection != null && selection.length > 0;
            }

            @Override
            public void performAction(ActionEvent event) {
                // choose default action iff supported
            }
        };
        IconMenuButton btnExport = Actions.asIconMenuButton(exportMenuAction, actionSource);
        Menu menuExport = Actions.createMenu();
        menuExport.addItem(Actions.asMenuItem(archiveExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(archiveOldPrintExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(krameriusExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(ndkExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(ndkSipExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(chronicleExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(ndkOldPrintExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(cejshExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(crossrefExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(desaExportAction, actionSource, true));
        menuExport.addItem(Actions.asMenuItem(desaDownloadAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(fullDataStreamExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(rawDataStreamExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(ndkUserDataStreamExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(kwisExportAction, actionSource, false));
        btnExport.setMenu(menuExport);


        final AbstractAction administrationMenuAction = new AbstractAction(
                i18n.AdministrationAction_Title(), "[SKIN]/headerIcons/settings_Over.png", null) {

            @Override
            public boolean accept(ActionEvent event) {
                if (!(Editor.getInstance().hasPermission("proarc.permission.admin") ||
                        Editor.getInstance().hasPermission(UserRole.ROLE_SUPERADMIN) ||
                        Editor.getInstance().hasPermission(UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION) ||
                        Editor.getInstance().hasPermission(UserRole.PERMISSION_RUN_UPDATE_MODEL_FUNCTION) ||
                        Editor.getInstance().hasPermission(UserRole.PERMISSION_RUN_LOCK_OBJECT_FUNCTION) ||
                        Editor.getInstance().hasPermission(UserRole.PERMISSION_RUN_UNLOCK_OBJECT_FUNCTION) ||
                        Editor.getInstance().hasPermission(UserRole.PERMISSION_IMPORT_TO_CATALOG_FUNCTION))) {
                    return false;
                } else {
                    Object[] selection = Actions.getSelection(event);
                    return selection != null && selection.length >= 0;
                }
            }

            @Override
            public void performAction(ActionEvent event) {
                // choose default action iff supported
            }
        };

        IconMenuButton btnAdministration = Actions.asIconMenuButton(administrationMenuAction, actionSource);
        Menu menuAdministration = Actions.createMenu();
        menuAdministration.addItem(Actions.asMenuItem(restoreAction, actionSource, false));
        //menuAdministration.addItem(Actions.asMenuItem(generateMasterCopyAction, actionSource, false));
        //menuAdministration.addItem(Actions.asMenuItem(generateMasterCopyAction, actionSource, false));
        menuAdministration.addItem(new MenuItemSeparator());
        menuAdministration.addItem(Actions.asMenuItem(lockObjectAction, actionSource, false));
        menuAdministration.addItem(Actions.asMenuItem(unlockObjectAction, actionSource, false));
        menuAdministration.addItem(new MenuItemSeparator());
        menuAdministration.addItem(initChangePageMenu(actionSource));
        menuAdministration.addItem(initChangeNdkModelsMenu(actionSource));
        menuAdministration.addItem(initChangeNdkEModelsMenu(actionSource));
        menuAdministration.addItem(initChangeK4ModelsMenu(actionSource));
        menuAdministration.addItem(initChangeOldprintModelsMenu(actionSource));
        menuAdministration.addItem(initChangeBdmModelsMenu(actionSource));
        //menuAdministration.addItem(Actions.asMenuItem(changeClippingsVolumeToNdkMonographVolumeAction, actionSource, false));
        //menuAdministration.addItem(Actions.asMenuItem(changeClippingsTitleToNdkMonographTitleAction, actionSource, false));
        menuAdministration.addItem(new MenuItemSeparator());
        menuAdministration.addItem(Actions.asMenuItem(updateNdkArticleAction, actionSource, false));
        menuAdministration.addItem(Actions.asMenuItem(updateNdkPageAction, actionSource, false));
        menuAdministration.addItem(Actions.asMenuItem(updateOldprintPageAction, actionSource, false));
        menuAdministration.addItem(new MenuItemSeparator());
        menuAdministration.addItem(Actions.asMenuItem(updateAllObjectsAction, actionSource, false));
        menuAdministration.addItem(Actions.asMenuItem(indexAllObjectsAction, actionSource, true));
        btnAdministration.setMenu(menuAdministration);


        toolbar.addMember(Actions.asIconButton(new RefreshAction(i18n),
                new RefreshableView((Refreshable) actionSource.getSource())));
        toolbar.addSeparator();
        toolbar.addMember(Actions.asIconButton(modsEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(noteEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(parentEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(mediaEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(ocrEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(childrenEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(atmEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(technicalMetadataAction, actionSource));
        toolbar.addSeparator();
        toolbar.addMember(Actions.asIconButton(foxmlAction, actionSource));
        toolbar.addMember(btnExport);
        toolbar.addMember(Actions.asIconButton(deleteAction, actionSource));
        toolbar.addMember(Actions.asIconButton(registerUrnNbnAction, actionSource));
        toolbar.addMember(Actions.asIconButton(copyObjectAction, actionSource));
        toolbar.addMember(btnAdministration);
        if (menuType == MenuType.TREE_VIEW) {
            toolbar.addMember(Actions.asIconButton(expandTreeAction, actionSource));
        }
    }

    private MenuItem initChangeOldprintModelsMenu(ActionSource actionSource) {
        MenuItem changeNdkModels = new MenuItem(i18n.ChangeOldPrintModels());
        Menu changeSttModelsMenu = Actions.createMenu();
        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintChapterToNdkChapterAction, actionSource, false));
        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintGraphicToNdkPictureAction, actionSource, false));
        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintMapToNdkMapAction, actionSource, false));
        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintMonographToNdkMonographAction, actionSource, false));
//        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintMonographVolumeToOldPrintGraphicAction, actionSource, false));
//        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintMonographVolumeToOldPrintMusicSheetAction, actionSource, false));
//        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintGraphicToOldprintMonographVolumeAction, actionSource, false));
        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintMonographVolumeToOldPrintMonographUnitAction, actionSource, false));
        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintMonographUnitToOldPrintMonographVolumeAction, actionSource, false));
        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintMusicsheetToNdkMusicsheetAction, actionSource, false));
        changeSttModelsMenu.addItem(Actions.asMenuItem(changeOldprintSupplementToNdkSupplementAction, actionSource, false));
        changeNdkModels.setSubmenu(changeSttModelsMenu);
        return changeNdkModels;
    }

    private MenuItem initChangePageMenu(ActionSource actionSource) {
        MenuItem changePageModels = new MenuItem(i18n.ChangePageModels());
        Menu changePageModelsMenu = Actions.createMenu();
        changePageModelsMenu.addItem(Actions.asMenuItem(changePageToNdkPageAction, actionSource, false));
        changePageModelsMenu.addItem(Actions.asMenuItem(changeNdkPageToPageAction, actionSource, false));
        changePageModelsMenu.addItem(Actions.asMenuItem(changeSttPageToNdkPageAction, actionSource, false));
        changePageModelsMenu.addItem(Actions.asMenuItem(changeNdkPageToSttPageAction, actionSource, false));
        changePageModelsMenu.addItem(Actions.asMenuItem(changePageToSttPageAction, actionSource, false));
        changePageModelsMenu.addItem(Actions.asMenuItem(changeSttPageToPageAction, actionSource, false));
        changePageModels.setSubmenu(changePageModelsMenu);
        return changePageModels;
    }

    private MenuItem initChangeNdkModelsMenu(ActionSource actionSource) {
        MenuItem changeNdkModels = new MenuItem(i18n.ChangeNdkModels());
        Menu changeNdkModelsMenu = Actions.createMenu();
        //changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkMonographVolumeToClippingsVolumeAction, actionSource, false));
        //changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkMonographTitleToClippingsTitleAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkMonographTitleToNdkMonographVolumeAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkMonographUnitToNdkMonographVolumeAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkMonographVolumeToNdkMonographTitleAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkMonographVolumeToNdkMonographUnitAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkCartographicToOldprintCartographicAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkChapterToSttChapterAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkMonographSupplementToOldprintMonographSupplementAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkMonographVolumeToOldprintMonographVolumeAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkMusicsheetToOldprintMusicsheetAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkPictureToOldprintGraphicAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkPeriodicalToNdkEPeriodicalAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkPeriodicalVolumeToNdkEPeriodicalVolumeAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkPeriodicalIssueToNdkEPeriodicalIssueAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkPeriodicalSupplementToNdkEPeriodicalSupplementAction, actionSource, false));
        changeNdkModelsMenu.addItem(Actions.asMenuItem(changeNdkArticleToNdkEArticleAction, actionSource, false));
        changeNdkModels.setSubmenu(changeNdkModelsMenu);
        return changeNdkModels;
    }

    private MenuItem initChangeNdkEModelsMenu(ActionSource actionSource) {
        MenuItem changeNdkEModels = new MenuItem(i18n.ChangeNdkEModels());
        Menu changeNdkEModelsMenu = Actions.createMenu();
        changeNdkEModelsMenu.addItem(Actions.asMenuItem(changeNdkEPeriodicalToNdkPeriodicalAction, actionSource, false));
        changeNdkEModelsMenu.addItem(Actions.asMenuItem(changeNdkEPeriodicalVolumeToNdkPeriodicalVolumeAction, actionSource, false));
        changeNdkEModelsMenu.addItem(Actions.asMenuItem(changeNdkEPeriodicalIssueToNdkPeriodicalIssueAction, actionSource, false));
        changeNdkEModelsMenu.addItem(Actions.asMenuItem(changeNdkEPeriodicalSupplementToNdkPeriodicalSupplementAction, actionSource, false));
        changeNdkEModelsMenu.addItem(Actions.asMenuItem(changeNdkEArticleToNdkArticleAction, actionSource, false));
        changeNdkEModelsMenu.addItem(Actions.asMenuItem(changeNdkEArticleToBdmArticleAction, actionSource, false));
        changeNdkEModels.setSubmenu(changeNdkEModelsMenu);
        return changeNdkEModels;
    }

    private MenuItem initChangeBdmModelsMenu(ActionSource actionSource) {
        MenuItem changeBdmModels = new MenuItem(i18n.ChangeBdmModels());
        Menu changeBdmModelsMenu = Actions.createMenu();
        changeBdmModelsMenu.addItem(Actions.asMenuItem(changeBdmArticleToNdkEArticleAction, actionSource, false));
        changeBdmModels.setSubmenu(changeBdmModelsMenu);
        return changeBdmModels;
    }

    private MenuItem initChangeK4ModelsMenu(ActionSource actionSource) {
        MenuItem changeK4Models = new MenuItem(i18n.ChangeK4Models());
        Menu changeK4ModelsMenu = Actions.createMenu();
        changeK4ModelsMenu.addItem(Actions.asMenuItem(changeK4PeriodicalToNdkPeriodicalAction, actionSource, false));
        changeK4ModelsMenu.addItem(Actions.asMenuItem(changeK4PeriodicalVolumeToNdkPeriodicalVolumeAction, actionSource, false));
        changeK4ModelsMenu.addItem(Actions.asMenuItem(changeK4PeriodicalIssueToNdkPeriodicalIssueAction, actionSource, false));
        changeK4ModelsMenu.addItem(Actions.asMenuItem(changeK4MonographToNdkMonographVolumeAction, actionSource, false));
        changeK4ModelsMenu.addItem(Actions.asMenuItem(changeK4MonographUnitToNdkMonographVolumeAction, actionSource, false));
        changeK4ModelsMenu.addItem(Actions.asMenuItem(changeK4MonographUnitToNdkMonographUnitAction, actionSource, false));
        changeK4Models.setSubmenu(changeK4ModelsMenu);
        return changeK4Models;
    }

    private void initContextMenu(Menu menu, ActionSource actionSource) {
        menu.addItem(Actions.asMenuItem(modsEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(noteEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(parentEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(mediaEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(ocrEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(childrenEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(atmEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(technicalMetadataAction, actionSource, false));
        menu.addItem(new MenuItemSeparator());
        menu.addItem(Actions.asMenuItem(foxmlAction, actionSource, true));
        menu.addItem(new MenuItemSeparator());
        menu.addItem(Actions.asMenuItem(archiveExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(archiveOldPrintExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(krameriusExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(ndkExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(ndkOldPrintExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(cejshExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(crossrefExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(desaExportAction, actionSource, true));
        menu.addItem(Actions.asMenuItem(desaDownloadAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(fullDataStreamExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(rawDataStreamExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(ndkUserDataStreamExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(kwisExportAction, actionSource, false));
        menu.addItem(new MenuItemSeparator());
        menu.addItem(Actions.asMenuItem(deleteAction, actionSource, true));
        menu.addItem(Actions.asMenuItem(registerUrnNbnAction, actionSource, true));
        //menu.addItem(Actions.asMenuItem(copyObjectAction, actionSource, true));
        menu.addItem(Actions.asMenuItem(expandTreeAction, actionSource, true));
    }

    private final class RefreshableView implements Refreshable {

        private final Refreshable delegate;

        RefreshableView(Refreshable delegate) {
            this.delegate = delegate;
        }

        @Override
        public void refresh() {
            fetchModels(true);
            delegate.refresh();
        }
    }

    public enum MenuType {
        TREE_VIEW, FOUND_VIEW
    }
}


