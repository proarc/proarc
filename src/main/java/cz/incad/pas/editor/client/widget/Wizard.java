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
package cz.incad.pas.editor.client.widget;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.Editor;

/**
 * The wizard allows to connect multiple layout widgets as steps
 * and define a workflow.
 *
 * @author Jan Pokorsky
 */
public class Wizard extends VLayout {
    
    private final String WIZARD_LABEL_PREFIX = "<b>%s</b> - %s";
    private final HLayout bottomLayout;

    public enum StepKind {BACK, FORWARD, CANCEL}
    private final ClientMessages i18n;
    private LazyWizardStep[] steps;
    private int stepIdx = -1;
    private Button btnBack;
    private Button btnForward;
    private IButton btnCancel;
    private Layout stepContainer;
    private Label lblHeader;
    private boolean canStepForward = true;
    private boolean canStepBack = true;
    
    public Wizard(ClientMessages i18n, WizardStep... steps) {
        assert steps != null && steps.length > 0 && steps[0] != null;
        this.i18n = i18n;
        this.steps = LazyWizardStep.asLazySteps(steps);
        setHeight100();
        setWidth100();
        
        lblHeader = new Label();
        lblHeader.setAutoHeight();
        lblHeader.setPadding(4);
        lblHeader.setStyleName(Editor.CSS_PANEL_DESCRIPTION_TITLE);
        addMember(lblHeader);
        
        stepContainer = new Layout();
        stepContainer.setHeight100();
        
        addMember(stepContainer);
        
        bottomLayout = new HLayout();
        bottomLayout.setAlign(Alignment.RIGHT);
        bottomLayout.setDefaultLayoutAlign(VerticalAlignment.CENTER);
        bottomLayout.setBorder("1px solid gray");
        bottomLayout.setLayoutTopMargin(4);
        bottomLayout.setPadding(2);
        bottomLayout.setMembersMargin(2);
        
        btnBack = new Button(i18n.Wizard_ButtonBack_Title());
        btnBack.addClickHandler(new ClickHandler() {
            
            @Override
            public void onClick(ClickEvent event) {
                stepBack();
            }
        });
        bottomLayout.addMember(btnBack);
        
        btnForward = new Button(i18n.Wizard_ButtonForward_Title());
        btnForward.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                stepForward();
            }
        });
        
        bottomLayout.addMember(btnForward);
        
        btnCancel = new IButton("Cancel");
//        bottomLayout.addMember(btnCancel);
        addMember(bottomLayout);
    }

    /**
     * Helper to use the wizard just as a container.
     * @param showSteps
     */
    public void setShowButtons(boolean showSteps) {
        bottomLayout.setVisible(showSteps);
    }

    public void setWizardLabel(String title, String msg) {
        lblHeader.setContents(ClientUtils.format(WIZARD_LABEL_PREFIX, title, msg));
    }
    
    public void stepInit() {
        stepIdx = 0;
        updateOnStepAction(stepIdx, stepIdx);
    }
    
    private void stepBack() {
        if (stepIdx > 0) {
            if (!steps[stepIdx].onStepAction(this, StepKind.BACK)) {
                // cancel step change
                return ;
            }
            updateOnStepAction(stepIdx, --stepIdx);
        }
    }
    
    private void stepForward() {
        if (stepIdx + 1 < steps.length) {
            if (!steps[stepIdx].onStepAction(this, StepKind.FORWARD)) {
                // cancel step change
                return ;
            }
            updateOnStepAction(stepIdx, ++stepIdx);
        }
    }

    public void moveAt(WizardStep step) {
        int oldStepIdx = stepIdx;
        stepIdx = getStepIndex(step);
        updateOnStepAction(oldStepIdx, stepIdx);
    }

    private int getStepIndex(WizardStep step) {
        for (int i = 0; i < steps.length; i++) {
            if (steps[i].getDelegate() == step) {
                return i;
            }
        }
        throw new IllegalStateException("Unknown step: " + step);
    }
    
    private void updateOnStepAction(int oldStep, int newStep) {
        if (oldStep >= 0 && oldStep != newStep) {
            steps[oldStep].onHide(this);
        }
        stepContainer.setMembers(steps[newStep].asWidget());
        
        // reset buttons
        setBackButton(true, null);
        setForwardButton(true, null);
        this.canStepBack = true;
        this.canStepForward = true;

        steps[newStep].onShow(this);
        updateButtonStates();
    }

    private void updateButtonStates() {
        boolean btnBackDisabled = !canStepBack || stepIdx < 1;
        boolean btnForwardDisabled = (!canStepForward) || (stepIdx + 1 >= steps.length);
        boolean btnCancelDisabled = stepIdx % 2 > 0;

        btnBack.setDisabled(btnBackDisabled);
        btnForward.setDisabled(btnForwardDisabled);
        btnCancel.setDisabled(btnCancelDisabled);
    }

    public WizardStep getActiveStep() {
        return steps[stepIdx];
    }

    public void setCanStepForward(boolean allowed) {
        this.canStepForward = allowed;
        updateButtonStates();
    }

    public void setCanStepBack(boolean allowed) {
        this.canStepBack = allowed;
        updateButtonStates();
    }

    public void setBackButton(boolean show, String title) {
        setButton(btnBack, show, title == null ? i18n.Wizard_ButtonBack_Title() : title);
    }

    public void setForwardButton(boolean show, String title) {
        setButton(btnForward, show, title == null ? i18n.Wizard_ButtonForward_Title() : title);
    }

    private void setButton(Button btn, boolean show, String title) {
        if (show) {
            btn.show();
            btn.setTitle(title);
        } else {
            btn.hide();
        }
    }

    public static WizardStep emptyStep() {
        return new EmptyStep();
    }

    /**
     * One step of the wizard.
     */
    public interface WizardStep {

        public void onShow(Wizard wizard);
        public void onHide(Wizard wizard);

        /**
         * Notifies about click on a wizard button.
         *
         * @return {@code false} stands for cancel the action
         */
        public boolean onStepAction(Wizard wizard, StepKind step);

        public Canvas asWidget();

    }

    /**
     * Creates and holds widget on demand.
     */
    private static final class LazyWizardStep implements WizardStep {

        private final WizardStep delegate;
        private Canvas widget;

        public LazyWizardStep(WizardStep step) {
            this.delegate = step;
        }

        @Override
        public void onShow(Wizard wizard) {
            delegate.onShow(wizard);
        }

        @Override
        public void onHide(Wizard wizard) {
            delegate.onHide(wizard);
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            return delegate.onStepAction(wizard, step);
        }

        @Override
        public Canvas asWidget() {
            if (widget == null) {
                widget = delegate.asWidget();
            }
            return widget;
        }

        public WizardStep getDelegate() {
            return delegate;
        }

        public static LazyWizardStep[] asLazySteps(WizardStep[] steps) {
            if (steps == null) {
                return null;
            }
            LazyWizardStep[] handles = new LazyWizardStep[steps.length];
            for (int i = 0; i < steps.length; i++) {
                handles[i] = new LazyWizardStep(steps[i]);
            }
            return handles;
        }

    }

    private static final class EmptyStep implements WizardStep {

        @Override
        public void onShow(Wizard wizard) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public void onHide(Wizard wizard) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public Canvas asWidget() {
            return new Canvas();
        }

    }
    
}
