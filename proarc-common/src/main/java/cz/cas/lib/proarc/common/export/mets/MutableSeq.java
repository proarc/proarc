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

package cz.cas.lib.proarc.common.export.mets;

import org.apache.log4j.Logger;

/**
 * 
 * Mutable sequence class
 * 
 * @author Robert Simonovsky
 * 
 */
public class MutableSeq {
    @SuppressWarnings("unused")
    private static Logger logger = Logger.getLogger(MutableSeq.class);
    long number = 0;

    /**
     * 
     * Returns a sequence number + parameter
     * 
     * @param number
     * @return
     */
    public long add(long number) {
	this.number = this.number + number;
	return this.number;
    }

    /**
     * 
     * Sets the sequence value
     * 
     * @param number
     */
    public void set(long number) {
	this.number = number;
    }

    /**
     * 
     * Returns a sequence value
     * 
     * @return
     */
    public long get() {
	return this.number;
    }
}
