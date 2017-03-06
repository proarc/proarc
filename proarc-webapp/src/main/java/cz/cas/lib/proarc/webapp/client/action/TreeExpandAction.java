package cz.cas.lib.proarc.webapp.client.action;

import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectTreeView;
import com.smartgwt.client.data.Record;

/**
 * Action for expanding tree in treeView
 *
 * @author Jakub Kremlacek
 */
public final class TreeExpandAction extends AbstractAction {

    private final DigitalObjectTreeView treeView;

    public TreeExpandAction(
            String title,
            String tooltip,
            DigitalObjectTreeView treeView) {
        this(title, tooltip, null, treeView);
    }

    public TreeExpandAction(
            String title,
            String tooltip,
            String icon,
            DigitalObjectTreeView treeView
    ) {
        super(title, icon == null ? "[SKIN]/actions/next.png" : icon, tooltip);

        this.treeView = treeView;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] records = Actions.getSelection(event);

        if (records.length == 0) return;

        String pid = records[0].getAttribute(RelationDataSource.FIELD_PID);

        treeView.expandNode(pid);
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);

        if (selection != null && selection.length == 1) {
            return true;
        }

        return false;
    }
}
