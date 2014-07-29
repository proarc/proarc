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
package cz.cas.lib.proarc.webapp.client.widget;

import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.events.DataArrivedEvent;
import com.smartgwt.client.data.events.DataArrivedHandler;
import com.smartgwt.client.data.events.ErrorEvent;
import com.smartgwt.client.data.events.HandleErrorHandler;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Progressbar;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Shows progress in canvas or window.
 *
 * <p/>Use {@link #setInit() setInit}, {@link #setProgress setProgress}, {@link #setDone setDone}
 * to manually control progress.
 *
 * <p>Or use {@link #setInit() setInit}, {@link #setDataSource setDataSource} to bind data source that
 * supports paging.
 *
 * @author Jan Pokorsky
 */
public final class ProgressTracker {

    private static final Logger LOG = Logger.getLogger(ProgressTracker.class.getName());

    private final VLayout widget;
    private final Progressbar progressbar;
    private final Label label;
    private int lastDone;
    private int lastTotal;
    private DataSource datasource;
    private Criteria criteria;
    private ProgressHandler progressHandler;
    private Runnable exitCallback;
    private Window window;
    private final ClientMessages i18n;
    private String progressPrefix;
    private IButton closeBtn;

    public ProgressTracker(ClientMessages i18n) {
        this.i18n = i18n;
        widget = new VLayout(4);
        label = new Label();
        label.setWidth100();
        label.setAutoHeight();
        progressbar = new Progressbar();
        progressbar.setVertical(false);
        progressbar.setWidth100();
        progressbar.setHeight(24);
        progressbar.setBreadth(1);

        widget.setMembers(label, progressbar);
        widget.setWidth100();
        widget.setAutoHeight();
        progressPrefix = i18n.ProgressTracker_Progress_0();

        closeBtn = new IButton(i18n.ProgressTracker_CloseBtn_Title(), new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                stop();
            }
        });
        closeBtn.setTooltip(i18n.ProgressTracker_CloseBtn_Hint());
        closeBtn.setAutoFit(true);

    }

    public VLayout asPanel() {
        return widget;
    }

    public void showInWindow(Runnable exitCallback) {
        showInWindow(exitCallback, i18n.ProgressTracker_Window_Title());
    }

    public void showInWindow(Runnable exitCallback, String title) {
        this.exitCallback = exitCallback;
        if (window == null) {
            window = new Window();
            window.setWidth(400);
            window.setAutoSize(true);
            window.setAutoCenter(true);
            window.setIsModal(true);
            widget.setMargin(10);
            window.setTitle(title);
            window.setShowMinimizeButton(false);
            window.setShowModalMask(true);
            window.addCloseClickHandler(new CloseClickHandler() {

                @Override
                public void onCloseClick(CloseClickEvent event) {
                    stop();
                }
            });
            window.addItem(widget);
            window.addItem(createControls());
        }
        window.show();
    }

    private Canvas createControls() {
        HStack btnLayout = new HStack(5);
        btnLayout.setAutoHeight();
        btnLayout.setMargin(10);
        btnLayout.setLayoutAlign(Alignment.RIGHT);
        btnLayout.setMembers(closeBtn);
        return btnLayout;
    }

    public void setInit() {
        lastDone = lastTotal = 0;
        label.setIcon(null);
        label.setContents(i18n.ProgressTracker_Initializing_Msg());
        progressbar.setPercentDone(0);
        if (datasource != null) {
            progressHandler = new ProgressHandler(datasource, criteria);
            ResultSet resultSet = progressHandler.getResultSet();
//            resultSet.getRange(0, 10);
            ClientUtils.getRangeWorkAround(resultSet, 0, 10);
        }
    }

    public void setProgress(int done, int total) {
        done = Math.max(0, done);
        total = Math.max(0, total);
        done = Math.min(done, total);
        lastDone = done;
        lastTotal = total;
        int progress = total == 0 ? 0 : done * 100 / total;
        String msg = i18n.ProgressTracker_Progress_Msg(
                progressPrefix, String.valueOf(done), String.valueOf(total));
        label.setContents(msg);
        progressbar.setPercentDone(progress);
    }

    public void setProgressPrefix(String prefix) {
        this.progressPrefix = prefix;
    }

    public void setDone(String msg) {
        setDone(msg, false);
    }

    public void setDone(String msg, boolean failure) {
        setProgress(lastTotal, lastTotal);
        if (msg != null) {
            label.setContents(msg);
        }
        if (failure) {
            label.setIcon("[SKIN]/Dialog/error.png");
        }
    }

    /**
     * Overrides default description of the close button.
     *
     * @see ClientMessages#ProgressTracker_CloseBtn_Title
     * @see ClientMessages#ProgressTracker_CloseBtn_Hint
     */
    public void setCloseButton(String title, String hint) {
        closeBtn.setTitle(title == null ? i18n.ProgressTracker_CloseBtn_Title(): title);
        closeBtn.setTooltip(hint == null ? i18n.ProgressTracker_CloseBtn_Hint() : hint);
    }

    public void stop() {
        if (progressHandler != null) {
            progressHandler.done();
            progressHandler = null;
        }
        if (window != null) {
            window.hide();
        }
        if (exitCallback != null) {
            exitCallback.run();
        }
    }

    public int getLastDone() {
        return lastDone;
    }

    public int getLastTotal() {
        return lastTotal;
    }

    public void setDataSource(DataSource ds, Criteria criteria) {
        this.datasource = ds;
        this.criteria = criteria;
    }

    private final class ProgressHandler implements Runnable, DataArrivedHandler, HandleErrorHandler {

        private final ArrayList<HandlerRegistration> registrations = new ArrayList<HandlerRegistration>();
        private final ResultSet resultSet;
        private ProgressTracker tracker = ProgressTracker.this;
        private boolean done;

        public ProgressHandler(DataSource ds, Criteria criteria) {
            resultSet = new ResultSet(ds);
            resultSet.setCriteria(criteria);
            resultSet.setFetchDelay(2000);
            registrations.add(ds.addHandleErrorHandler(this));
            registrations.add(resultSet.addDataArrivedHandler(this));
        }

        public ResultSet getResultSet() {
            return resultSet;
        }

        @Override
        public void run() {
            done();
        }

        public void done() {
            done = true;
            for (HandlerRegistration r : registrations) {
                r.removeHandler();
            }
            registrations.clear();
        }

        @Override
        public void onDataArrived(DataArrivedEvent event) {
            if (done) {
                return ;
            }
            int startRow = event.getStartRow();
            final int endRow = event.getEndRow();
            final int length = resultSet.getLength();
            Boolean lengthIsKnown = resultSet.lengthIsKnown();
            ClientUtils.log(LOG, Level.FINE, "onDataArrived: [%s,%s,%s], lengthIsKnown: %s", startRow, endRow, length, lengthIsKnown);
            tracker.setProgress(endRow, length);
            if (lengthIsKnown && endRow == resultSet.getLength()) {
                // done
                ProgressTracker.this.stop();
                ClientUtils.log(LOG, Level.FINE, "onDataArrived.done");
                done();
            } else {
                ClientUtils.log(LOG, Level.FINE, "onDataArrived.next: [%s,%s]", endRow, length);
//                resultSet.getRange(endRow, length);
                ClientUtils.getRangeWorkAround(resultSet, endRow, length);
            }
        }

        @Override
        public void onHandleError(ErrorEvent event) {
            ClientUtils.log(LOG, Level.FINE, "onHandleError");
            if (done) {
                return ;
            }
            event.cancel();
            StringBuilder sb = new StringBuilder();
            DSResponse response = event.getResponse();
            Map<?, ?> errors = response.getErrors();
            if (errors != null) {
                for (Iterator<?> it = errors.values().iterator(); it.hasNext();) {
                    sb.append(it.hasNext());
                    sb.append("<p>\n");
                }
            }
            if (sb.length() == 0) {
                sb.append(response.getHttpResponseText());
            }
            if (sb.length() == 0) {
                sb.append(i18n.ProgressTracker_Failure_UnknownReason_Title());
            }
            String msg = i18n.ProgressTracker_Failure_Msg(sb.toString());
            tracker.setDone(msg, true);
            done();
        }

    }

}
