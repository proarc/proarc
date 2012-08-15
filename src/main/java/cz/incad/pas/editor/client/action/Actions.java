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

import com.smartgwt.client.widgets.IconButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.menu.IconMenuButton;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import java.util.logging.Logger;

/**
 * Helpers for {@link Action}'s instances.
 *
 * @author Jan Pokorsky
 */
public final class Actions {

    private static final Logger LOG = Logger.getLogger(Actions.class.getName());

    public static ToolStripButton asToolStripButton(final Action action, final Object source) {
        ToolStripButton tsb = new ToolStripButton();
        String title = action.getTitle();
        if (title != null) {
            tsb.setTitle(title);
        }
        String icon = action.getIcon();
        if (icon != null) {
            tsb.setIcon(icon);
        }
        String tooltip = action.getTooltip();
        if (tooltip != null) {
            tsb.setTooltip(tooltip);
        }
        tsb.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                ActionEvent aEvent = new ActionEvent(source);
                action.performAction(aEvent);
            }
        });
        return tsb;
    }

    public static IconButton asIconButton(final Action action, final Object source) {
        IconButton btn = new IconButton();
        String title = action.getTitle();
        if (title != null) {
            btn.setTitle(title);
        } else {
            btn.setTitle("");
        }
        String icon = action.getIcon();
        if (icon != null) {
            btn.setIcon(icon);
        }
        String tooltip = action.getTooltip();
        if (tooltip != null) {
            btn.setTooltip(tooltip);
        }
        btn.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                ActionEvent aEvent = new ActionEvent(source);
                action.performAction(aEvent);
            }
        });
        return btn;
    }

    public static IconMenuButton asIconMenuButton(final Action action, final Object source) {
        final IconMenuButton btn = new IconMenuButton();
        String title = action.getTitle();
        if (title != null) {
            btn.setTitle(title);
        }
        String icon = action.getIcon();
        if (icon != null) {
            btn.setIcon(icon);
        }
        String tooltip = action.getTooltip();
        if (tooltip != null) {
            btn.setTooltip(tooltip);
        }
        btn.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                Menu menu = btn.getMenu();
                boolean showMenu = menu != null && !(menu.isDrawn() && menu.isVisible());
                if (showMenu) {
                    // here could be default action
                    btn.showMenu();
                    return;
                }
                if (menu == null) {
                    ActionEvent aEvent = new ActionEvent(source);
                    action.performAction(aEvent);
                }
            }
        });
        return btn;
    }

    public static MenuItem asMenuItem(final Action action, final Object source) {
        MenuItem mi = new MenuItem();
        String title = action.getTitle();
        if (title != null) {
            mi.setTitle(title);
        }
        String icon = action.getIcon();
        if (icon != null) {
            mi.setIcon(icon);
        }
        mi.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

            @Override
            public void onClick(MenuItemClickEvent event) {
                ActionEvent aEvent = new ActionEvent(source);
                action.performAction(aEvent);
            }
        });
        return mi;
    }

    public static ToolStrip createToolStrip() {
        ToolStrip t = new ToolStrip();
        t.setMembersMargin(2);
        t.setLayoutMargin(2);
        t.setWidth100();
        return t;
    }

    /**
     * Gets selected objects from the action event.
     * @param <T> type of selected objects
     * @param event event holding {@link Selectable}
     * @return the selection or {@code null}
     */
    public static <T> T[] getSelection(ActionEvent event) {
        Object source = event.getSource();
        if (source instanceof Selectable) {
            Selectable<T> selectable = (Selectable<T>) source;
            return selectable.getSelection();
        }
        return null;
    }

}
