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
package cz.cas.lib.proarc.webapp.client.action;

import com.google.gwt.event.logical.shared.HasValueChangeHandlers;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.widgets.IconButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.ShowContextMenuEvent;
import com.smartgwt.client.widgets.events.ShowContextMenuHandler;
import com.smartgwt.client.widgets.events.VisibilityChangedEvent;
import com.smartgwt.client.widgets.events.VisibilityChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.IconMenuButton;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuIconClickEvent;
import com.smartgwt.client.widgets.menu.events.MenuIconClickHandler;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.logging.Logger;

/**
 * Helpers for {@link Action}'s instances.
 *
 * @author Jan Pokorsky
 */
public final class Actions {

    private static final Logger LOG = Logger.getLogger(Actions.class.getName());

    /**
     * No-op action useful for menu buttons.
     */
    public static Action emptyAction(String title, String icon, String tooltip) {
        return new AbstractAction(title, icon, tooltip) {

            @Override
            public void performAction(ActionEvent event) {
                throw new UnsupportedOperationException("Not supported");
            }
        };
    }

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

    /** Gets action view listening to action source changes. */
    public static IconButton asIconButton(Action action, ActionSource source) {
        return asIconButton(action, source.getSource(), source);
    }
    
    /** Gets action view. */
    public static IconButton asIconButton(Action action, Object source) {
        return asIconButton(action, source, null);
    }

    private static <T> IconButton asIconButton(
            final Action action, final Object source, ActionSource actionSource) {

        final IconButton btn = new IconButton();
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
        if (actionSource != null) {
            actionSource.addValueChangeHandler(new ValueChangeHandler<ActionEvent>() {

                @Override
                public void onValueChange(ValueChangeEvent<ActionEvent> event) {
                    boolean accept = action.accept(event.getValue());
                    btn.setVisible(accept);
                }
            });
        }
        return btn;
    }

    /** Gets action view. */
    public static IconMenuButton asIconMenuButton(final Action action, final Object source) {
        return asIconMenuButton(action, source, null);
    }

    /** Gets action view listening to action source changes. */
    public static IconMenuButton asIconMenuButton(Action action, ActionSource source) {
        return asIconMenuButton(action, source.getSource(), source);
    }
    
    private static IconMenuButton asIconMenuButton(final Action action,
            final Object source, ActionSource actionSource) {

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

        IconMenuButtonHandler handler = new IconMenuButtonHandler(btn, source, action);
        btn.addClickHandler(handler);
        btn.addMenuIconClickHandler(handler);

        if (actionSource != null) {
            actionSource.addValueChangeHandler(new ValueChangeHandler<ActionEvent>() {

                @Override
                public void onValueChange(ValueChangeEvent<ActionEvent> event) {
                    boolean accept = action.accept(event.getValue());
                    btn.setVisible(accept);
                }
            });
        }
        return btn;
    }

    /** Gets action view. */
    public static MenuItem asMenuItem(final Action action, final Object source) {
        return asMenuItem(action, source, false);
    }

    /** Gets action view. */
    public static MenuItem asMenuItem(final Action action, final Object source, boolean willAsk) {
        return asMenuItem(action, source, null, willAsk);
    }

    /** Gets action view listening to action source changes. */
    public static MenuItem asMenuItem(final Action action, final ActionSource source, boolean willAsk) {
        return asMenuItem(action, source.getSource(), source, willAsk);
    }

    private static MenuItem asMenuItem(final Action action, final Object source,
            ActionSource actionSource, boolean willAsk) {

        final MenuItem mi = new MenuItem();
        mi.setEnabled(Boolean.TRUE);
        String title = action.getTitle();
        if (title != null) {
            title = willAsk ? title + "..." : title;
            mi.setTitle(title);
        }
        String icon = action.getIcon();
        if (icon != null) {
            // workaround: Menu uses own skin img dir default
            icon = icon.replace("[SKIN]", Page.getSkinImgDir());
            mi.setIcon(icon);
        }
        mi.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

            @Override
            public void onClick(MenuItemClickEvent event) {
                ActionEvent aEvent = new ActionEvent(source);
                action.performAction(aEvent);
            }
        });

        if (actionSource != null) {
            actionSource.addValueChangeHandler(new ValueChangeHandler<ActionEvent>() {

                @Override
                public void onValueChange(ValueChangeEvent<ActionEvent> event) {
                    boolean accept = action.accept(event.getValue());
                    mi.setEnabled(accept);
                }
            });
        }
        return mi;
    }

    /**
     * Creates default menu.
     */
    public static Menu createMenu() {
        final LiveMenu menu = new LiveMenu();
        menu.setShowShadow(Boolean.TRUE);
        menu.addVisibilityChangedHandler(new VisibilityChangedHandler() {

            @Override
            public void onVisibilityChanged(VisibilityChangedEvent event) {
                if (event.getIsVisible()) {
                    menu.includeEnabled();
                }
            }
        });
        return menu;
    }

    /**
     * Creates default toolbar.
     */
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

    /**
     * Fixes {@link ListGrid} context menu
     * to update grid selection on right click properly.
     * <p>Bug: right click selects row without firing selection event.
     */
    public static void fixListGridContextMenu(final ListGrid grid) {
        grid.addShowContextMenuHandler(new ShowContextMenuHandler() {

            @Override
            public void onShowContextMenu(ShowContextMenuEvent event) {
                int eventRow = grid.getEventRow();
                if (eventRow < 0) {
                    return ;
                }
                ListGridRecord[] selectedRecords = grid.getSelectedRecords();

                if (selectedRecords.length <= 1) {
                    // ListGrid does not fire selection updated event if right click
                    // no select if multi-selection exists
                    grid.selectSingleRecord(eventRow);
                }
                Menu contextMenu = grid.getContextMenu();
                contextMenu.showContextMenu();
                event.cancel();
            }
        });

    }

    /**
     * Helper class to fire action source changes that can result to show/hide
     * action view that listen to this instance.
     */
    public static final class ActionSource implements HasValueChangeHandlers<ActionEvent> {

        private HandlerManager handlerManager = new HandlerManager(this);
        private final Object source;

        public ActionSource(Object source) {
            this.source = source;
        }

        @Override
        public HandlerRegistration addValueChangeHandler(ValueChangeHandler<ActionEvent> handler) {
            return handlerManager.addHandler(ValueChangeEvent.getType(), handler);
        }

        public Object getSource() {
            return source;
        }

        @Override
        public void fireEvent(GwtEvent<?> event) {
            if (event == null) {
                fireEvent();
                return ;
            }
            handlerManager.fireEvent(event);
        }

        public void fireEvent() {
            ValueChangeEvent.fire(this, new ActionEvent(source));
        }

    }

    /**
     * Hides disabled items and superfluous separators.
     */
    private static final class LiveMenu extends Menu {

        private final ArrayList<MenuItem> menuItems = new ArrayList<MenuItem>();

        /**
         * Recomputes menu.
         */
        public void includeEnabled() {
            ArrayList<MenuItem> enabled = new ArrayList<MenuItem>(menuItems.size());
            // skip separators of hidden actions
            boolean lastWasSeparator = true;
            for (MenuItem item : menuItems) {
                if (item.getEnabled() || (item.getIsSeparator() && !lastWasSeparator)) {
                    enabled.add(item);
                    lastWasSeparator = item.getIsSeparator();
                }
            }
            MenuItem[] toArray = enabled.toArray(new MenuItem[enabled.size()]);
            super.setItems(toArray);
        }

        public void includeAll() {
            if (menuItems.size() != getItems().length) {
                MenuItem[] toArray = menuItems.toArray(new MenuItem[menuItems.size()]);
                super.setItems(toArray);
            }
        }

        @Override
        public void addItem(MenuItem item) {
            menuItems.add(item);
            super.addItem(item);
        }

        @Override
        public void addItem(MenuItem item, int index) {
            includeAll();
            menuItems.add(index, item);
            super.addItem(item, index);
        }

        @Override
        public void setItems(MenuItem... items) {
            menuItems.clear();
            menuItems.addAll(Arrays.asList(items));
            super.setItems(items);
        }

        @Override
        public void removeItem(MenuItem item) {
            menuItems.remove(item);
            super.removeItem(item);
        }
    }

    private static class IconMenuButtonHandler implements ClickHandler, MenuIconClickHandler {

        private final IconMenuButton btn;
        private final Object source;
        private final Action action;

        public IconMenuButtonHandler(IconMenuButton btn, Object source, Action action) {
            this.btn = btn;
            this.source = source;
            this.action = action;
        }

        @Override
        public void onClick(ClickEvent event) {
            onClick();
        }

        @Override
        public void onMenuIconClick(MenuIconClickEvent event) {
            onClick();
        }

        private void onClick() {
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

    }

}
