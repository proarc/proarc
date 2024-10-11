/*
 * Copyright (C) 2024 Lukas Sykora
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

package cz.cas.lib.proarc.common.catalog.updateCatalog;

import cz.cas.lib.proarc.common.catalog.BibliographicCatalog;
import cz.cas.lib.proarc.common.catalog.MetadataItem;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.config.Catalogs;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsCollectionDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.UrlDefinition;
import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.TransformerException;
import org.apache.commons.io.FileUtils;
import org.codehaus.jettison.json.JSONException;

import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_CATALOG_DIRECTORY;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_CATALOG_URL_LINK;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_FIELD001_BASE_DEFAULT;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_FIELD001_BASE_LENGHT;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_FIELD001_SYSNO_LENGHT;


/**
 * Class that updates Aleph Catalogs record using xml file
 *
 * @author Lukas Sykora
 */
public class AlephXmlUpdateCatalog extends UpdateCatalog {

    private static final Logger LOG = Logger.getLogger(AlephXmlUpdateCatalog.class.getName());
    public static final String ID = "ALEPH_XML";

    public AlephXmlUpdateCatalog(AppConfiguration appConfiguration, AkubraConfiguration akubraConfiguration) {
        super(appConfiguration, akubraConfiguration);
    }

    @Override
    protected boolean allowUpdateRecord(CatalogConfiguration catalog) {
        boolean ok = true;
        if (catalog.getCatalogDirectory() == null || catalog.getCatalogDirectory().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_CATALOG_DIRECTORY));
            ok = false;
        }
        if (catalog.getCatalogUrlLink() == null || catalog.getCatalogUrlLink().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_CATALOG_URL_LINK));
            ok = false;
        }
        if (catalog.getField001BaseLenght() == null || catalog.getField001BaseLenght() < 0) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_FIELD001_BASE_LENGHT));
            ok = false;
        }
        if (catalog.getField001BaseDefault() != null && catalog.getField001BaseDefault().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_FIELD001_BASE_DEFAULT));
            ok = false;
        }
        if (catalog.getField001SysnoLenght() == null) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_FIELD001_SYSNO_LENGHT));
            ok = false;
        }
        return ok;
    }

    @Override
    public boolean process(CatalogConfiguration catalogConfiguration, String field001, String pid) throws DigitalObjectException, JSONException, IOException {
        if (allowUpdateRecord(catalogConfiguration)) {
            try {
                Integer expectedLength = getExpectedLength(catalogConfiguration);
                String base = null;
                String sysno = null;
                boolean containsDigitalizationInfo = false;
                if (expectedLength == null) {
                    sysno = field001;
                } else if (expectedLength == field001.length() || expectedLength == field001.length() + catalogConfiguration.getField001BaseDefault().length()) {
                    base = getBase(field001, catalogConfiguration);
                    sysno = getSysno(field001, catalogConfiguration);
                } else {
                    LOG.log(Level.SEVERE, "Špatná délka SYSNa. Očekávaná délka je " + expectedLength + " ale délka pole 001 (" + field001 + ") je " + field001.length());
                    throw new IOException("Špatná délka SYSNa. Očekávaná délka je " + expectedLength + " ale délka pole 001 (" + field001 + ") je " + field001.length());
                }
                if (catalogConfiguration.checkValidSysnoBeforeUpdate()) {
                    try {
                        containsDigitalizationInfo = checkValidSysnoInCatalog(catalogConfiguration, sysno, pid);
                    } catch (UpdateCatalogException ex) {
                        throw new IOException(ex.getMessage(), ex);
                    } catch (Exception ex) {
                        throw new IOException("Nepodařilo se zvalidovat sysno proti katalogu.", ex);
                    }
                }
                if (containsDigitalizationInfo) {
                    throw new IOException("Záznam v katalogu (" + catalogConfiguration.getId() + ", sysno: " + sysno + ") již obsahuje info o digitalizaci.");
                }
                return updateRecord(base, sysno, catalogConfiguration.getCatalogUrlLink(), pid, catalogConfiguration.getCatalogDirectory());
            } catch (StringIndexOutOfBoundsException ex) {
                LOG.log(Level.SEVERE, ex.getMessage(), ex);
                throw new IOException("Wrong value in proarc.cfg for base or sysno lenght.", ex);
            }
        } else {
            LOG.severe("Catalog with id " + catalogConfiguration.getId() + " does not support Record modification");
            throw new IOException("Catalog with id " + catalogConfiguration.getId() + " does not support Record modification");
        }
    }

    private boolean checkValidSysnoInCatalog(CatalogConfiguration catalogConfiguration, String sysno, String pid) throws IOException, TransformerException, UpdateCatalogException {
        BibliographicCatalog catalog = Catalogs.getCatalog(catalogConfiguration, null);
        List<MetadataItem> data = catalog.find(catalogConfiguration.getId(), catalogConfiguration.getDefaultSearchField(), sysno, new Locale("cs"));
        if (data == null || data.isEmpty()) {
            throw new UpdateCatalogException("Nenalezena žádná data v katalogu " + catalogConfiguration.getId() + " pro identifikator " + sysno + ".");
        }
        MetadataItem item = data.get(0);
        return checkDigitalizationInfo(item.getMods(), catalogConfiguration, pid);
    }

    private boolean checkDigitalizationInfo(String modsAsString, CatalogConfiguration catalogConfiguration, String pid) {
        ModsCollectionDefinition modsCollection = ModsUtils.unmarshal(modsAsString, ModsCollectionDefinition.class);
        ModsDefinition mods = null;
        if (modsCollection == null || modsCollection.getMods().isEmpty()) {
            mods = ModsUtils.unmarshal(modsAsString, ModsDefinition.class);
        } else {
            mods = modsCollection.getMods().get(0);
        }

        if (mods.getLocation() == null || mods.getLocation().isEmpty()) {
            return false;
        }
        String expectedLinkValue = createCatalogLink(catalogConfiguration.getCatalogUrlLink(), pid);
        for (LocationDefinition location : mods.getLocation()) {
            for (UrlDefinition url : location.getUrl()) {
                if (url.getValue() != null && (url.getValue().equalsIgnoreCase(expectedLinkValue) || url.getValue().equalsIgnoreCase(expectedLinkValue.replaceAll("/view/", "/uuid/")))) {
                    return true;
                }
            }
        }
        return false;
    }

    private Integer getExpectedLength(CatalogConfiguration catalogConfiguration) {
        int lenght = 0;
        Integer configLength = catalogConfiguration.getField001BaseLenght();
        if (configLength != null && configLength < 1) {
            lenght = lenght + 0;
        } else {
            lenght += configLength;
        }
        configLength = catalogConfiguration.getField001SysnoLenght();
        if (catalogConfiguration.getField001SysnoLenght() < 0) {
            return null;
        }
        if (configLength != null && configLength < 1) {
            lenght = lenght + 0;
        } else {
            lenght += configLength;
        }
        return lenght;
    }

    private String getSysno(String field001, CatalogConfiguration catalogConfiguration) {
        Integer baseLenght = catalogConfiguration.getField001BaseLenght();
        Integer sysnoLenght = catalogConfiguration.getField001SysnoLenght();
        if (baseLenght == null || baseLenght < 1) {
            if (sysnoLenght == null) {
                return field001;
            } else {
                return field001.substring(0, sysnoLenght);
            }
        } else {
            if (sysnoLenght == null) {
                return field001;
            }
            if (sysnoLenght == field001.length()) {
                return field001;
            }
            return field001.substring(baseLenght, baseLenght + sysnoLenght);
        }
    }

    private String getBase(String field001, CatalogConfiguration catalogConfiguration) {
        Integer lenght = catalogConfiguration.getField001BaseLenght();
        if (lenght == null || lenght < 1) {
            return null;
        } else {
            if (field001.length() == catalogConfiguration.getField001SysnoLenght()) {
                return catalogConfiguration.getField001BaseDefault();
            } else {
                return field001.substring(0, lenght).toLowerCase();
            }
        }
    }

    protected void validateValues(String base, String sysno, String catalogLink, String pid, String catalogPathValue) throws IOException {
        if (base != null && base.isEmpty()) {
            throw new IOException("Base is empty");
        }
        if (sysno == null || sysno.isEmpty()) {
            throw new IOException("Base is null or empty");
        }
        if (catalogLink == null || catalogLink.isEmpty()) {
            throw new IOException("Catalog Link is null or empty");
        }
        if (pid == null || pid.isEmpty()) {
            throw new IOException("UUID is null or empty");
        }
        if (catalogPathValue == null || catalogPathValue.isEmpty()) {
            throw new IOException("Catalog path is null or empty");
        } else {
            File catelogPath = new File(catalogPathValue);
            if (!catelogPath.exists() || !catelogPath.isDirectory() || !catelogPath.canWrite()) {
                throw new IOException("Catalog path is unreachable or cannot be written to.");
            }
        }
    }

    private boolean updateRecord(String base, String sysno, String catalogLink, String pid, String catalogPath) throws IOException {
        try {
            validateValues(base, sysno, catalogLink, pid, catalogPath);
        } catch (IOException e) {
            LOG.log(Level.SEVERE, e.getMessage(), e);
            throw e;
        }

        catalogLink = createCatalogLink(catalogLink, pid);

        File alephDirectory = new File(catalogPath);
        File csvFile = alephDirectory.toPath().resolve(pid.substring(5) + ".csv").toFile();

        try {
            String preparedLink = prepareString(base, sysno, catalogLink);
            FileUtils.writeStringToFile(
                    csvFile,
                    preparedLink,
                    Charset.defaultCharset());

            csvFile.setReadable(true, false);
            csvFile.setWritable(true, false);
        } catch (IOException e) {
            LOG.log(Level.SEVERE, e.getMessage(), e);
            throw e;
        }

        return true;
    }

    private String createCatalogLink(String catalogLink, String pid) {
        if (catalogLink.endsWith("/")) {
            catalogLink = catalogLink + pid;
        } else {
            catalogLink = catalogLink + "/" + pid;
        }
        return catalogLink;
    }

    private String prepareString(String base, String sysno, String catalogLink) {
        StringBuilder builder = new StringBuilder();
        if (base != null && !base.isEmpty()) {
            builder.append(base).append(" @ ");
        }
        if (sysno != null && !sysno.isEmpty()) {
            builder.append(sysno).append(" @ ");
        }
        if (catalogLink != null && !catalogLink.isEmpty()) {
            builder.append(catalogLink);
        }
        return builder.toString();
    }
}
