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

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.io.FileUtils;
import org.codehaus.jettison.json.JSONException;

import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_CATALOG_DIRECTORY;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_CATALOG_URL_LINK;
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
        }if (catalog.getField001SysnoLenght() == null || catalog.getField001SysnoLenght() < 0) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_FIELD001_SYSNO_LENGHT));
            ok = false;
        }
        return ok;
    }

    @Override
    public boolean process(CatalogConfiguration catalogConfiguration, String field001, String pid) throws DigitalObjectException, JSONException, IOException {
        if (allowUpdateRecord(catalogConfiguration)) {
            try {
                String base = getBase(field001, catalogConfiguration);
                String sysno = getSysno(field001, catalogConfiguration);
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

    private String getSysno(String field001, CatalogConfiguration catalogConfiguration) {
            Integer baseLenght = catalogConfiguration.getField001BaseLenght();
            Integer sysnoLenght = catalogConfiguration.getField001SysnoLenght();
            if (baseLenght == null || baseLenght < 1) {
                return field001.substring(0, sysnoLenght);
            } else {
                return field001.substring(baseLenght, baseLenght + sysnoLenght);
            }
    }

    private String getBase(String field001, CatalogConfiguration catalogConfiguration) {
        Integer lenght = catalogConfiguration.getField001BaseLenght();
        if (lenght == null || lenght < 1) {
            return "";
        } else {
            return field001.substring(0, lenght).toLowerCase();
        }
    }

    protected void validateValues(String base, String sysno, String catalogLink, String pid, String catalogPathValue) throws IOException {
        if (base == null || base.isEmpty()) {
            throw new IOException("Base is null or empty");
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

        if (catalogLink.endsWith("/")) {
            catalogLink = catalogLink + pid;
        } else {
            catalogLink = catalogLink + "/" + pid;
        }

        File alephDirectory = new File(catalogPath);
        File csvFile = alephDirectory.toPath().resolve(pid.substring(5) + ".csv").toFile();

        try {
            FileUtils.writeStringToFile(
                    csvFile,
                    base + " @ " + sysno + " @ " + catalogLink + pid,
                    Charset.defaultCharset());

            csvFile.setReadable(true, false);
            csvFile.setWritable(true, false);
        } catch (IOException e) {
            LOG.log(Level.SEVERE, e.getMessage(), e);
            throw e;
        }

        return true;
    }
}
