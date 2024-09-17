/*
 * Copyright (C) 2022 Lukas Sykora
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
package cz.cas.lib.proarc.common.object.ndk;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.storage.DigitalObjectValidationException;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.RelatedItemDefinition;
import java.time.LocalDate;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import org.apache.commons.configuration.Configuration;

/**
 * Checks Mods rules.
 *
 * @author Lukas Sykora
 */
public class ModsRules {

    private String modelId;
    private ModsDefinition mods;
    private DigitalObjectValidationException exception;
    private NdkMapper.Context context;
    private AppConfiguration config;
    private String parentModel;

    private static final String PROP_MODS_PHYSICAL_LOCATION_SIGLA = "metadata.mods.location.physicalLocation.sigla";
    private List<String> acceptableSiglaId;

    public static final String ERR_NDK_SUPPLEMENT_GENRE_TYPE ="Err_Ndk_Supplement_Genre_Type";
    public static final String ERR_NDK_ORIGININFO_DATEISSSUED ="Err_Ndk_OriginInfo_DateIssued";
    public static final String ERR_NDK_PHYSICALLOCATION_MULTIPLE ="Err_Ndk_PhysicalLocation_Multiple";
    public static final String ERR_NDK_PHYSICALLOCATION_SIGLA ="Err_Ndk_PhysicalLocation_Sigla";
    public static final String ERR_NDK_RELATEDITEM_PHYSICALLOCATION_SIGLA ="Err_Ndk_RelatedItem_PhysicalLocation_Sigla";

    private ModsRules() {}

    public ModsRules(String modelId, ModsDefinition mods, DigitalObjectValidationException ex, NdkMapper.Context context, AppConfiguration appConfiguration) {
        this.modelId = modelId;
        this.mods = mods;
        this.exception = ex;
        this.context = context;
        this.config = appConfiguration;
        this.parentModel = null;
    }

    public ModsRules(String modelId, ModsDefinition mods, DigitalObjectValidationException ex, String parentModel, AppConfiguration appConfiguration) {
        this.modelId = modelId;
        this.mods = mods;
        this.exception = ex;
        this.context = null;
        this.config = appConfiguration;
        this.parentModel = parentModel;
    }

    public void check() throws DigitalObjectValidationException{
        if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            checkGenreType(mods);
        }
        checkDateIssued(mods, modelId);
        checkPhysicalLocation(mods.getLocation());
        checkRelatedItemPhysicalLocation(mods.getRelatedItem());

        if (!exception.getValidations().isEmpty()){
            throw exception;
        }
    }

    public void checkExtended() throws DigitalObjectValidationException{
        if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            checkGenreType(mods);
        }
        checkDateIssued(mods, modelId);
        checkPhysicalLocation(mods.getLocation());
        checkPhysicalLocationCount(mods.getLocation());
        checkRelatedItemPhysicalLocation(mods.getRelatedItem());

        if (!exception.getValidations().isEmpty()){
            throw exception;
        }
    }

    public void checkPhysicalLocation(List<LocationDefinition> locations) {
        checkPhysicalLocation(locations, ERR_NDK_PHYSICALLOCATION_SIGLA);
    }

    public void checkPhysicalLocationCount(List<LocationDefinition> locations) {
        if (locations.size() > 1) {
            if (!NdkPlugin.MODEL_MONOGRAPHTITLE.equals(modelId)) {
                exception.addValidation("MODS rules", ERR_NDK_PHYSICALLOCATION_MULTIPLE, true);
            }
        }
    }

    public void checkRelatedItemPhysicalLocation(List<RelatedItemDefinition> relatedItems) {
        for (RelatedItemDefinition relatedItem : relatedItems) {
            checkPhysicalLocation(relatedItem.getLocation(), ERR_NDK_RELATEDITEM_PHYSICALLOCATION_SIGLA);
        }
    }

    public void checkPhysicalLocation (List<LocationDefinition> locations, String bundleKey) {
        for (LocationDefinition location : locations) {
            for (PhysicalLocationDefinition physicalLocation : location.getPhysicalLocation()) {
                if ("siglaADR".equals(physicalLocation.getAuthority())) {
                    List<String> accepted = config.getModsOptions().getAcceptableSiglaId();
                    if (!accepted.contains(physicalLocation.getValue())) {
                        exception.addValidation("MODS rules", bundleKey, false, physicalLocation.getValue());
                    }
                }
            }
        }
    }

    private void checkGenreType(ModsDefinition mods) {
        String expectedType = getExpectedType();
        if (mods != null) {
            for (GenreDefinition genre : mods.getGenre()) {
                if (expectedType == null) {
                    continue; // nenalezen expected type
                } else if (!expectedType.equals(genre.getType())) {
                    exception.addValidation("MODS rules", ERR_NDK_SUPPLEMENT_GENRE_TYPE, false, expectedType, genre.getType());
                }
            }
        }
    }

    public void checkDateIssued(ModsDefinition mods, String modelId) {
        if (NdkPlugin.MODEL_PERIODICALISSUE.equals(modelId) || NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            if (mods != null) {
                for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
                    for (DateDefinition date : originInfo.getDateIssued()) {
                        checkDateFormat(date.getValue());
                    }
                }
            }
        }
    }

    private void checkDateFormat(String value) {
        if (value != null && !value.isEmpty()) {
            if (!DatumValidator.isValid(value)) {
                exception.addValidation("MODS rules", ERR_NDK_ORIGININFO_DATEISSSUED, false, value);
            }
        }
    }

    private String getExpectedType() {
        if ((parentModel == null || parentModel.isEmpty()) && context != null) {
            parentModel = context.getParentModel();
        }
        if (parentModel == null) {
            return null;
        } else if (NdkPlugin.MODEL_PERIODICALISSUE.equals(parentModel)) {
            return "issue_supplement";
        } else if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(parentModel)) {
            return "volume_supplement";
        }
        return null;
    }

    public static ModsRules getOptions(Configuration config) {
        ModsRules options = new ModsRules();

        String[] modsRules = config.getStringArray(PROP_MODS_PHYSICAL_LOCATION_SIGLA);
        options.setAcceptableSiglaId(Arrays.asList(modsRules));
        return options;
    }

    public List<String> getAcceptableSiglaId() {
        return acceptableSiglaId;
    }

    public void setAcceptableSiglaId(List<String> acceptableSiglaId) {
        this.acceptableSiglaId = acceptableSiglaId;
    }

    public static class DatumValidator {
        private static String[] regexPatterns = {
                "^\\d{2}\\.\\d{2}\\.\\d{4}$",                           // DD.MM.RRRR
                "^\\d{2}\\.\\d{4}$",                                    // MM.RRRR
                "^\\d{4}$",                                             // RRRR
                "^\\d{2}\\.\\-\\d{2}\\.\\d{4}$",                        // MM.-MM.RRRR
                "^\\d{2}\\.\\d{4}-\\d{2}\\.\\d{4}$",                    // MM.RRRR-MM.RRRR
                "^\\d{2}\\.\\d{2}\\.\\-\\d{2}\\.\\d{2}\\.\\d{4}$",      // DD.MM.-DD.MM.RRRR
                "^\\d{2}\\.\\d{2}\\.\\d{4}-\\d{2}\\.\\d{2}\\.\\d{4}$",  // DD.MM.RRRR-DD.MM.RRRR
                "^\\d{2}\\.\\-\\d{2}\\.\\d{2}\\.\\d{4}$"                // DD.-DD.MM.RRRR
        };

        private static boolean isBefore(String first, String last) {
            if (isValidFullDate(first) && isValidFullDate(last)) {
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
                LocalDate firstDate = LocalDate.parse(first, formatter);
                LocalDate lastDate = LocalDate.parse(last, formatter);
                return firstDate.isBefore(lastDate);
            } else if (isValidMonthYear(first) && isValidMonthYear(last)) {
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MM.yyyy");
                YearMonth firstMonth = YearMonth.parse(first, formatter);
                YearMonth lastMonth = YearMonth.parse(last, formatter);
                return firstMonth.isBefore(lastMonth);
            }
            return false;
        }

        // Kontrola formátu DD.MM.RRRR
        public static boolean isValidFullDate(String datum) {
            try {
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
                LocalDate.parse(datum, formatter); // Zde kontrolujeme, zda datum existuje (tj. např. 31.02. selže)
                return true;
            } catch (DateTimeParseException e) {
                return false;
            }
        }

        // Kontrola formátu MM.RRRR
        public static boolean isValidMonthYear(String datum) {
            try {
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MM.yyyy");
                YearMonth.parse(datum, formatter);
                return true;
            } catch (DateTimeParseException e) {
                return false;
            }
        }

        // Kontrola formátu RRRR (jen rok)
        public static boolean isValidYear(String datum) {
            try {
                int year = Integer.parseInt(datum);
                return year >= 0;  // Základní kontrola roku
            } catch (NumberFormatException e) {
                return false;
            }
        }

        // Kontrola formátu MM.-MM.RRRR
        public static boolean isValidMultiMonthYear(String datum) {
            String[] parts = datum.split("\\.-");
            if (parts.length == 2) {
                String value1 = parts[0] + "." + parts[1].split("\\.")[1];
                return isValidMonthYear(value1) && isValidMonthYear(parts[1]) && isBefore(value1, parts[1]);
            }
            return false;
        }

        // Kontrola formátu MM.RRRR-MM.RRRR
        public static boolean isValidMonthYearRange(String datum) {
            String[] parts = datum.split("-");
            if (parts.length == 2) {
                return isValidMonthYear(parts[0]) && isValidMonthYear(parts[1]) && isBefore(parts[0], parts[1]);
            }
            return false;
        }

        // Kontrola formátu DD.MM.RRRR-DD.MM.RRRR
        public static boolean isValidFullDateRange(String datum) {
            String[] parts = datum.split("-");
            if (parts.length == 2) {
                return isValidFullDate(parts[0]) && isValidFullDate(parts[1]) && isBefore(parts[0], parts[1]);
            }
            return false;
        }

        // Kontrola formátu DD.MM.-DD.MM.RRRR
        public static boolean isValidMultiDayRange(String datum) {
            String[] parts = datum.split("\\.-");
            if (parts.length == 2) {
                String value1 = parts[0] + "." + parts[1].split("\\.")[2];
                return isValidFullDate(value1) && isValidFullDate(parts[1]) && isBefore(value1, parts[1]);
            }
            return false;
        }

        // Kontrola formátu DD.-DD.MM.RRRR
        public static boolean isValidMultiDaySameMonth(String datum) {
            String[] parts = datum.split("\\.-");
            if (parts.length == 2) {
                String value1 = parts[0] + "." + parts[1].split("\\.")[1] + "." + parts[1].split("\\.")[2];
                return isValidFullDate(value1) && isValidFullDate(parts[1]) && isBefore(value1, parts[1]);
            }
            return false;
        }

        private static boolean isValidFormat(String datum) {
            // Procházíme seznam regulárních výrazů a kontrolujeme shodu
            for (String pattern : regexPatterns) {
                if (Pattern.matches(pattern, datum)) {
                    return true;
                }
            }
            return false;  // Pokud datum neodpovídá žádnému formátu
        }

        public static boolean isValidValue(String datum) {
            return isValidFullDate(datum) ||
                    isValidMonthYear(datum) ||
                    isValidYear(datum) ||
                    isValidMultiMonthYear(datum) ||
                    isValidMonthYearRange(datum) ||
                    isValidMultiDayRange(datum) ||
                    isValidFullDateRange(datum) ||
                    isValidMultiDaySameMonth(datum);
        }

        public static boolean isValid(String datum) {
            return isValidFormat(datum) && isValidValue(datum);
        }
    }
}
