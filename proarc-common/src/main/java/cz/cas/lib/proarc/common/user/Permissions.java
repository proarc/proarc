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
package cz.cas.lib.proarc.common.user;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Jan Pokorsky
 */
public final class Permissions {

    public static final Permission ADMIN = new Permission("proarc.permission.admin", "Správa systému");
    public static final Permission IMPORT_FOLDER = new Permission("proarc.permission.import.loadFolder", "Načíst adresář");
    public static final Permission FEDORA_BATCH_INGEST = new Permission("proarc.permission.import.fedoraIngest", "Fedora import");
    public static final Permission IMPORT_UPDATE_MODS = new Permission("proarc.permission.import.update.mods", "Editace MODS");
    public static final Permission EDIT_UPDATE_MODS = new Permission("proarc.permission.editor.update.mods", "Editace MODS");
    public static final Permission EDIT_UPDATE_DC = new Permission("proarc.permission.editor.update.dc", "Editace Dublin Core");
    public static final Permission EDIT_CREATE_OBJECT = new Permission("proarc.permission.editor.create.object", "Vytvořit objekt");
    public static final Permission USERS_CREATE = new Permission("proarc.permission.users.create", "Vytvořit uživatele");
    public static final Permission USERS_READ = new Permission("proarc.permission.users.read", "Zobrazit uživatele");
    public static final Permission USER_LOGIN = new Permission("proarc.permission.user.login", "Přihlášení uživatele");
    
    private final HashSet<Permission> all = new HashSet<Permission>();

    public Permissions() {
        all.add(ADMIN);
        // XXX add others
    }

    public Set<Permission> getAll() {
        return Collections.unmodifiableSet(all);
    }

}
