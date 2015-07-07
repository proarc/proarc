/*
 * Copyright (C) 2015 Jan Pokorsky
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

import java.util.ArrayList;
import java.util.List;

/**
 * Notifies about validation errors.
 *
 * @author Jan Pokorsky
 */
public class DigitalObjectValidationException extends DigitalObjectException {

    private final List<ValidationResult> validations = new ArrayList<ValidationResult>();

    /**
     * Creates a new instance of <code>DigitalObjectValidationException</code>
     * without detail message.
     */
    public DigitalObjectValidationException(String pid, Integer batchId, String dsId, String message, Throwable cause) {
        super(pid, batchId, dsId, message, cause);
    }

    public DigitalObjectValidationException addValidation(String name, String bundleKey, Object... values) {
        ValidationResult vr = new ValidationResult();
        vr.setName(name);
        vr.setBundleKey(bundleKey);
        vr.setValues(values);
        validations.add(vr);
        return this;
    }

    public List<ValidationResult> getValidations() {
        return validations;
    }

    public static class ValidationResult {
        private String name;
        private String bundleKey;
        private Object[] values;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        /**
         * The key for an i18n message or the message itself.
         */
        public String getBundleKey() {
            return bundleKey;
        }

        /**
         * The key for an i18n message or the message itself.
         */
        public void setBundleKey(String bundleKey) {
            this.bundleKey = bundleKey;
        }

        /**
         * Values that can be used as arguments for a message in {@link java.text.MessageFormat}.
         */
        public Object[] getValues() {
            return values;
        }

        /**
         * Values that can be used as arguments for a message in {@link java.text.MessageFormat}.
         */
        public void setValues(Object[] values) {
            this.values = values;
        }
    }
}
