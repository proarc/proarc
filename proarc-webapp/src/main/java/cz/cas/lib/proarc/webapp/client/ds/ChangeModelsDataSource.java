/*
 * Copyright (C) 2019 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.client.ds;

import com.smartgwt.client.data.fields.DataSourceTextField;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;

/**
 *
 * @author Lukas Sykora
 */
public class ChangeModelsDataSource extends ProarcDataSource {

    public static final String FIELD_PID = DigitalObjectResourceApi.DIGITALOBJECT_PID;
    public static final String FIELD_MODEL = DigitalObjectResourceApi.DIGITALOBJECT_MODEL;

    private ChangeModelsDataSource(String type) {
        setDataURL(type);
        DataSourceTextField pid = new DataSourceTextField(FIELD_PID);
        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        setFields(pid, model);
        setOperationBindings(RestConfig.createAddOperation());
    }

    public static ChangeModelsDataSource changePageToNdkPage() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_PAGE_TO_NDK_PAGE);
    }

    public static ChangeModelsDataSource changeSttPageToNdkPage() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_STT_PAGE_TO_NDK_PAGE);
    }

    public static ChangeModelsDataSource changeNdkPageToSttPage() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_PAGE_TO_STT_PAGE);
    }

    public static ChangeModelsDataSource changeNdkPageToPage() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_PAGE_TO_PAGE);
    }

    public static ChangeModelsDataSource changePageToSttPage() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_PAGE_TO_STT_PAGE);
    }

    public static ChangeModelsDataSource changeSttPageToPage() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_STT_PAGE_TO_PAGE);
    }

    public static ChangeModelsDataSource changeClippingsVolumeToNdkMonographVolume() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_CLIPPINGS_VOLUME_TO_NDK_MONOGRAPH_VOLUME);
    }

    public static ChangeModelsDataSource changeNdkMonographVolumeToClippingsVolume() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_MONOGRAPH_VOLUME_TO_CLIPPINGS_VOLUME);
    }

    public static ChangeModelsDataSource changeClippingsTitleToNdkMonographTitle() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_CLIPPINGS_TITLE_TO_NDK_MONOGRAPH_TITLE);
    }

    public static ChangeModelsDataSource changeNdkMonographTitleToClippingsTitle() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_MONOGRAPH_TITLE_TO_CLIPPINGS_TITLE);
    }

    public static ChangeModelsDataSource changeNdkMonographTitleToNdkMonographVolume() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_MONOGRAPH_TITLE_TO_NDK_MONOGRAPH_VOLUME);
    }

    public static ChangeModelsDataSource changeNdkMonographUnitToNdkMonographVolume() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_VOLUME);
    }

    public static ChangeModelsDataSource changeNdkMonographVolumeToNdkMonographTitle() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_MONOGRAPH_VOLUME_TO_NDK_MONOGRAPH_TITLE);
    }

    public static ChangeModelsDataSource changeNdkMonographVolumeToNdkMonographUnit() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_MONOGRAPH_VOLUME_TO_NDK_MONOGRAPH_UNIT);
    }

    public static ChangeModelsDataSource changeK4PeriodicalToNdkPeriodical() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_K4_PERIODICAL_TO_NDK_PERIODICAL);
    }

    public static ChangeModelsDataSource changeK4PeriodicalVolumeToNdkPeriodicalVolume() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_K4_PERIODICAL_VOLUME_TO_NDK_PERIODICAL_VOLUME);
    }

    public static ChangeModelsDataSource changeK4PeriodicalIssueToNdkPeriodicalIssue() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_K4_PERIODICAL_ISSUE_TO_NDK_PERIODICAL_ISSUE);
    }

    public static ChangeModelsDataSource changeK4MonographToNdkMonographVolume() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_K4_MONOGRAPH_TO_NDK_MONOGRAPH_VOLUME);
    }

    public static ChangeModelsDataSource changeK4MonographUnitToNdkMonographVolume() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_VOLUME);
    }

    public static ChangeModelsDataSource changeK4MonographUnitToNdkMonographUnit() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_UNIT);
    }

    public static ChangeModelsDataSource changeNdkChapterToOldPrintChapter() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_NDK_CHAPTER_TO_STT_CHAPTER);
    }

    public static ChangeModelsDataSource changeOldPrintChapterToNdkChapter() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_STT_CHAPTER_TO_NDK_CHAPTER);
    }

    public static ChangeModelsDataSource changeNdkPictureToOldPrintGraphic() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_NDK_PICTURE_TO_STT_GRAPHIC);
    }

    public static ChangeModelsDataSource changeOldPrintGraphicToNdkPicture() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_STT_GRAPHIC_TO_NDK_PICTURE);
    }

    public static ChangeModelsDataSource changeNdkCartographicToOldPrintCartographic() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_NDK_CARTOGRAPHIC_TO_STT_CARTOGRAPHIC);
    }

    public static ChangeModelsDataSource changeOldPrintCartographicToNdkCartographic() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_STT_CARTOGRAPHIC_TO_NDK_CARTOGRAPHIC);
    }

    public static ChangeModelsDataSource changeNdkMonographVolumeToOldPrintMonographVolume() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_NDK_MONOGRAPH_TO_STT_MONOGRAPH);
    }

    public static ChangeModelsDataSource changeOldPrintMonographVolumeToNdkMonographVolume() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_STT_MONOGRAPH_TO_NDK_MONOGRAPH);
    }

    public static ChangeModelsDataSource changeOldPrintMonographVolumeToOldPrintMonographUnit() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_STT_MONOGRAPHVOLUME_TO_STT_MONOGRAPHUNIT);
    }

    public static ChangeModelsDataSource changeOldPrintMonographUnitToOldPrintMonographVolume() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_STT_MONOGRAPHUNIT_TO_STT_MONOGRAPHVOLUME);
    }

    public static ChangeModelsDataSource changeNdkMusicSheetToOldPrintMusicSheet() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_NDK_MUSICSHEET_TO_STT_MUSICSHEET);
    }

    public static ChangeModelsDataSource changeOldPrintMusicSheetToNdkMusicSheet() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_STT_MUSICSHEET_TO_NDK_MUSICSHEET);
    }

    public static ChangeModelsDataSource changeNdkMonographSupplementToOldPrintMonographSupplement() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_NDK_SUPPLEMENT_TO_STT_SUPPLEMENT);
    }

    public static ChangeModelsDataSource changeOldPrintMonographSupplementToNdkMonographSupplement() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_STT_SUPPLEMENT_TO_NDK_SUPPLEMENT);
    }

    public static ChangeModelsDataSource changeOldPrintGraphicToToOldPrintMonographVolume() {
        return new ChangeModelsDataSource(RestConfig.URl_CHANGE_STT_GRAPHIC_TO_STT_MONOGRAPH_VOLUME);
    }

    public static ChangeModelsDataSource changeOldPrintMonographVolumeToOldPrintMusicSheet() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_STT_MONOGRAPH_TO_STT_MUSICSHEET);
    }

    public static ChangeModelsDataSource changeOldPrintMonographVolumeToOldPrintGraphic() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_STT_MONOGRAPH_TO_STT_GRAPHIC);
    }

    public static ChangeModelsDataSource changeNdkPeriodicalToNdkEPeriodical() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_PERIODICAL_TO_NDK_EPERIODICAL);
    }
    public static ChangeModelsDataSource changeNdkPeriodicalVolumeToNdkEPeriodicalVolume() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_PERIODICAL_VOLUME_TO_NDK_EPERIODICAL_VOLUME);
    }

    public static ChangeModelsDataSource changeNdkPeriodicalIssueToNdkEPeriodicalIssue() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_PERIODICAL_ISSUE_TO_NDK_EPERIODICAL_ISSUE);
    }

    public static ChangeModelsDataSource changeNdkPeriodicalSupplementToNdkEPeriodicalSupplement() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_PERIODICAL_SUPPLEMENT_TO_NDK_EPERIODICAL_SUPPLEMENT);
    }

    public static ChangeModelsDataSource changeNdkArticleToNdkEArticle() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_ARTICLE_TO_NDK_EARTICLE);
    }

    public static ChangeModelsDataSource changeBdmArticleToNdkEArticle() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_BDM_ARTICLE_TO_NDK_EARTICLE);
    }

    public static ChangeModelsDataSource changeNdkEPeriodicalToNdkPeriodical() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_EPERIODICAL_TO_NDK_PERIODICAL);
    }
    public static ChangeModelsDataSource changeNdkEPeriodicalVolumeToNdkPeriodicalVolume() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_EPERIODICAL_VOLUME_TO_NDK_PERIODICAL_VOLUME);
    }

    public static ChangeModelsDataSource changeNdkEPeriodicalIssueToNdkPeriodicalIssue() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_EPERIODICAL_ISSUE_TO_NDK_PERIODICAL_ISSUE);
    }

    public static ChangeModelsDataSource changeNdkEPeriodicalSupplementToNdkPeriodicalSupplement() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_EPERIODICAL_SUPPLEMENT_TO_NDK_PERIODICAL_SUPPLEMENT);
    }

    public static ChangeModelsDataSource changeNdkEArticleToNdkArticle() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_EARTICLE_TO_NDK_ARTICLE);
    }

    public static ChangeModelsDataSource changeNdkEArticleToBdmArticle() {
        return new ChangeModelsDataSource(RestConfig.URL_CHANGE_NDK_EARTICLE_TO_BDM_ARTICLE);
    }
}
