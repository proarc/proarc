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
package cz.cas.lib.proarc.common.workflow.profile;

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

/**
 * Compares {@link WorkflowItemView workflow items} according to their title.
 *
 * @author Jan Pokorsky
 */
public class WorkflowItemComparator implements Comparator<WorkflowItemView> {

    private final Collator collator;

    public WorkflowItemComparator(Locale locale) {
        this.collator = Collator.getInstance(locale);
    }

    @Override
    public int compare(WorkflowItemView o1, WorkflowItemView o2) {
        return collator.compare(o1.getTitle(), o2.getTitle());
    }

}
