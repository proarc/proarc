package cz.cas.lib.proarc.common.process.imports.audio;

/*
 * Copyright (C) 2017 Lukas Sykora
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
public enum AudioMimeType {

    WAVE("audio/x-wav", "wave", true, false, false),
    FLAC("audio/flac", "flac", true, true, false),
    MP3("audio/mpeg3", "mp3", true, false, false),
    OGG("audio/ogg", "ogg", true, false, false);

    private String mime;
    private boolean supportedbyJava;
    private boolean supportedbyJAI;
    private boolean multipageFormat;
    private String defaultFileExtension;

    AudioMimeType(String mime, String defaultExtension, boolean javasupport, boolean jaiSupport, boolean multipageformat) {
        this.mime = mime;
        this.supportedbyJava = javasupport;
        this.supportedbyJAI = jaiSupport;
        this.multipageFormat = multipageformat;
        this.defaultFileExtension = defaultExtension;
    }

    public String getMimeType() {
        return this.mime;
    }

    public boolean javaNativeSupport() {
        return this.supportedbyJava;
    }

    public boolean isMultipageFormat() {
        return this.multipageFormat;
    }

    public boolean isSupportedbyJAI() {
        return this.supportedbyJAI;
    }

    public void setSupportedbyJAI(boolean supportedbyJAI) {
        this.supportedbyJAI = supportedbyJAI;
    }

    public String getDefaultFileExtension() {
        return this.defaultFileExtension;
    }

    public static AudioMimeType loadFromMimeType(String mime) {
        AudioMimeType[] values = values();
        AudioMimeType[] arr = values;
        int len = values.length;

        for(int i = 0; i < len; ++i) {
            AudioMimeType iType = arr[i];
            if (iType.getMimeType().equals(mime)) {
                return iType;
            }
        }
        return null;
    }
}
