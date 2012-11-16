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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.client.action;

import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import cz.incad.pas.editor.client.ClientMessages;

/**
 * Helper class providing description for subclasses that saves some content.
 *
 * @author Jan Pokorsky
 */
public abstract class SaveAction extends AbstractAction {

    public SaveAction(ClientMessages i18n) {
        super(i18n.SaveAction_Title(),
                "[SKIN]/actions/save.png",
                i18n.SaveAction_Hint());
    }

    /**
     * Saves savable object.
     *
<pre><code>
ask strategy    states
T   ASK         validate, (IfValidAsk, IfYesSave | IfNoDone) | (IfInvalidAsk, IfReallyYesSave | IfNoDone)
T   IGNORE      ask, (IfYesSave | IfNoDone)
T   RUN         validate, (IfValidAsk, IfYesSave | IfNoDone) | IfInvalidDone
F   ASK         validate, IfValidSave | (IfInvalidAsk, IfYesSave | IfNoDone)
F   IGNORE      save
F   RUN         validate, IfValidSave | IfInvalidDone
</code></pre>
     *
     * @param savable object implementing save
     * @param saveCallback listener to get save result
     * @param ask ask user before the save
     * @param strategy validation strategy
     */
    public static void saveTask(Savable savable, BooleanCallback saveCallback,
            boolean ask, SaveValidation strategy, ClientMessages i18n) {

        BooleanCallback saveIfYes = new SaveIfYes(savable, saveCallback);
        BooleanCallback runIfValid = getRunIfValid(savable, saveCallback, ask,
                saveIfYes, strategy, i18n);

        if (strategy == SaveValidation.IGNORE) {
            if (ask) {
                askSave(saveIfYes, i18n);
            } else {
                savable.save(saveCallback);
            }
        } else {
            savable.validate(runIfValid);
        }

    }

    private static void askSave(final BooleanCallback result, ClientMessages i18n) {
        SC.ask(i18n.SaveAction_Ask_Msg(), new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                result.execute(value);
            }
        });

    }

    private static void askIgnoreValidation(final BooleanCallback result, ClientMessages i18n) {
        SC.ask(i18n.SaveAction_IgnoreInvalid_Msg(), new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                result.execute(value);
            }
        });

    }

    private static BooleanCallback getRunIfValid(
            final Savable savable, final BooleanCallback saveCallback,
            final boolean ask, final BooleanCallback saveIfYes,
            final SaveValidation strategy, final ClientMessages i18n) {

        final BooleanCallback runOnValid = new BooleanCallback() {

            @Override
            public void execute(Boolean valid) {
                if (valid != null && valid) {
                    if (ask) {
                        askSave(saveIfYes, i18n);
                    } else {
                        savable.save(saveCallback);
                    }
                } else if (strategy == SaveValidation.ASK) {
                    askIgnoreValidation(saveIfYes, i18n);
                } else {
                    saveCallback.execute(Boolean.FALSE);
                }
            }
        };
        return runOnValid;
    }

    private static class SaveIfYes implements BooleanCallback {

        private final Savable savable;
        private final BooleanCallback saveCallback;

        public SaveIfYes(Savable savable, BooleanCallback saveCallback) {
            this.savable = savable;
            this.saveCallback = saveCallback;
        }

        @Override
        public void execute(Boolean confirmed) {
            if (confirmed != null && confirmed) {
                savable.save(saveCallback);
            } else {
                saveCallback.execute(Boolean.FALSE);
            }
        }
    }

    /**
     * The interface should be implemented by objects able to validate
     * and save its contents.
     */
    public interface Savable {
        void save(BooleanCallback result);
        void validate(BooleanCallback result);
    }

    public enum SaveValidation {
        /** Ask user to ignore validation result. */
        ASK,
        /** Ignore validation result. */
        IGNORE,
        /** Run validation and save only on success. */
        RUN,
    }

}
