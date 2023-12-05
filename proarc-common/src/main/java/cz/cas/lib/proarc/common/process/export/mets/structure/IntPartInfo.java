/*
 * Copyright (C) 2013 Robert Simonovsky
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

package cz.cas.lib.proarc.common.process.export.mets.structure;

/**
 *
 * Internal part properties class
 *
 * @author Robert Simonovsky
 *
 */
public class IntPartInfo {
    private String type;
    private String altoPID;
    private String begin;
    private String order;

    /**
     * Getter for Order attribute
     *
     * @return
     */
    public String getOrder() {
        return order;
    }

    /**
     * Setter for Order attribute
     *
     * @param order
     */
    public void setOrder(String order) {
        this.order = order;
    }

    /**
     *
     * Constructor
     *
     * @param type
     * @param altoPID
     * @param begin
     * @param order
     */
    public IntPartInfo(String type, String altoPID, String begin, String order) {
        super();
        this.type = type;
        this.altoPID = altoPID;
        this.begin = begin;
        this.order = order;
    }

    /**
     * Getter for Type attribute
     *
     * @return
     */
    public String getType() {
        return type;
    }

    /**
     * Setter for Type attribute
     *
     * @param type
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Getter for AltoPID attribute
     *
     * @return
     */
    public String getAltoPID() {
        return altoPID;
    }

    /**
     * Setter for AltoPID attribute
     *
     * @param altoPID
     */
    public void setAltoPID(String altoPID) {
        this.altoPID = altoPID;
    }

    /**
     * Getter for Begin attribute
     *
     * @return
     */
    public String getBegin() {
        return begin;
    }

    /**
     * Setter for Begin attribute
     *
     * @param begin
     */
    public void setBegin(String begin) {
        this.begin = begin;
    }
}