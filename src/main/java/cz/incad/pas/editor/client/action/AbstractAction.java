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
package cz.incad.pas.editor.client.action;

/**
 * A basic implementation of {@link Action} interface.
 *
 * @author Jan Pokorsky
 */
public abstract class AbstractAction implements Action {

    private String title;
    private String icon;
    private String tooltip;

    public AbstractAction(String title, String icon, String tooltip) {
        this.title = title;
        this.icon = icon;
        this.tooltip = tooltip;
    }

    @Override
    public boolean accept(ActionEvent event) {
        return true;
    }

    @Override
    public String getIcon() {
        return icon;
    }

    @Override
    public String getTitle() {
        return title;
    }

    @Override
    public String getTooltip() {
        return tooltip;
    }

    @Override
    public void setIcon(String icon) {
        this.icon = icon;
    }

    @Override
    public void setTitle(String title) {
        this.title = title;
    }

    @Override
    public void setTooltip(String tooltip) {
        this.tooltip = tooltip;
    }

}
