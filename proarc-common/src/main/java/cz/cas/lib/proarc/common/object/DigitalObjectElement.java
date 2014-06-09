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
package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Encapsulates index description with data handler.
 *
 * @author Jan Pokorsky
 */
public class DigitalObjectElement {

    public static final DigitalObjectElement NULL = new DigitalObjectElement();
    private static final Logger LOG = Logger.getLogger(DigitalObjectElement.class.getName());
    private SearchView.Item item;
    private DigitalObjectHandler handler;

    private  DigitalObjectElement() {
    }

    public DigitalObjectElement(Item item, DigitalObjectHandler handler) {
        if (item == null) {
            throw new NullPointerException();
        }
        if (handler == null) {
            throw new NullPointerException();
        }
        this.item = item;
        this.handler = handler;
    }

    public String getPid() {
        return item.getPid();
    }

    public String getModelId() {
        return item.getModel();
    }

    public Item getItem() {
        return item;
    }

    public DigitalObjectHandler getHandler() {
        return handler;
    }

    public void setHandler(DigitalObjectHandler handler) {
        this.handler = handler;
    }

    public <R,P> R accept(DigitalObjectVisitor<R,P> v, P p) throws VisitorException {
        if (LOG.isLoggable(Level.FINE)) {
            LOG.log(Level.FINE, toString());
        }
        return v.visit(this, p);
    }

    @Override
    public String toString() {
        String sitem = item == null ? null
                : String.format("%s, %s, %s", item.getModel(), item.getPid(), item.getLabel());
        return "DigitalObjectElement{" + "item=" + sitem + ", handler=" + handler + '}';
    }

    public String toLog() {
        return String.format("%s, %s, %s", getModelId(), getPid(), getItem().getLabel());
    }


    public static class Factory {

        public DigitalObjectElement create(Item item, DigitalObjectHandler handler) {
            return new DigitalObjectElement(item, handler);
        }
    }

}
