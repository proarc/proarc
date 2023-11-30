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
package cz.cas.lib.proarc.common.fedora;

import cz.cas.lib.proarc.common.process.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.process.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixType;
import cz.cas.lib.proarc.mix.MixUtils;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import java.io.File;
import javax.xml.transform.Source;

/**
 * Edits technical metadata in MIX format.
 *
 * @author Jan Pokorsky
 */
public class MixEditor {

    public static final String NDK_ARCHIVAL_ID = "NDK_ARCHIVAL_MIX";
    public static final String RAW_ID = "RAW_MIX";
    private static final String MIX_FORMAT_URI = MixUtils.NS;

    private final XmlStreamEditor editor;
    private final FedoraObject object;
    private final DatastreamProfile profileTemplate;

    public static DatastreamProfile rawProfile() {
        return FoxmlUtils.managedProfile(RAW_ID, MIX_FORMAT_URI, "Technical metadata for RAW stream.");
    }

    public static DatastreamProfile ndkArchivalProfile() {
        return FoxmlUtils.managedProfile(NDK_ARCHIVAL_ID, MIX_FORMAT_URI, "Technical metadata for NDK_ARCHIVAL stream.");
    }

    /**
     * Gets editor to manage NDK_ARCHIVAL datastream metadata.
     * MIX does not contain info about the scanner.
     */
    public static MixEditor ndkArchival(FedoraObject object) {
        return new MixEditor(object, ndkArchivalProfile());
    }

    /**
     * Gets editor to manage RAW datastream metadata. MIX does not contain info
     * about the scanner.
     */
    public static MixEditor raw(FedoraObject object) {
        return new MixEditor(object, rawProfile());
    }

    public MixEditor(FedoraObject object, DatastreamProfile profile) {
        this.editor = object.getEditor(profile);
        this.object = object;
        this.profileTemplate = profile;
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    /**
     * Gets persisted MIX.
     * @return MIX or {@code null}
     * @throws DigitalObjectException failure
     */
    public MixType read() throws DigitalObjectException {
        Source src = editor.read();
        MixType result = null;
        if (src != null) {
            result = MixUtils.unmarshal(src, MixType.class);
        }
        return result;
    }

    /**
     * Gets persisted MIX as {@link Mix} class.
     * @return MIX or {@code null}
     * @throws DigitalObjectException failure
     */
    public Mix readMix() throws DigitalObjectException {
        Source src = editor.read();
        Mix result = null;
        if (src != null) {
            result = MixUtils.unmarshal(src, Mix.class);
        }
        return result;
    }

    public void write(MixType mix, long timestamp, String msg) throws DigitalObjectException {
        EditorResult result = editor.createResult();
        MixUtils.marshal(result, mix, true);
        editor.write(result, timestamp, msg);
    }

    /**
     * Generates and writes MIX for the passed content.
     * 
     * @param content file containing e.g. an image
     * @param jhoveCtx jHove context
     * @param timestamp timestamp
     * @param msg log message
     * @throws DigitalObjectException failure
     */
    public void write(File content, JhoveContext jhoveCtx, long timestamp, String msg) throws DigitalObjectException {
        try {
            Mix mix = JhoveUtility.getMix(content, jhoveCtx, null, null, null).getMix();
            if (mix == null) {
                throw new DigitalObjectException(
                    object.getPid(), null, profileTemplate.getDsID(), "jHove cannot generate MIX for " + content.toString(), null);
            }
            write(mix, timestamp, msg);
        } catch (DigitalObjectException ex) {
            throw ex;
        } catch (Exception ex) {
            throw new DigitalObjectException(
                    object.getPid(), null, profileTemplate.getDsID(), null, ex);
        }
    }

    public String readAsString() throws DigitalObjectException {
        Mix mix = readMix();
        if (mix != null) {
            return MixUtils.toXml(mix, true);
        }
        return null;
    }

//    public void generate(String dsId, JhoveContext jhoveCtx) {
//
//    }

}
