package cz.cas.lib.proarc.common.actions;

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;

public class UpdateObjects {

    private static final Logger LOG = Logger.getLogger(UpdateObjects.class.getName());

    private static AppConfiguration config;
    private static UserProfile user;
    private static Locale locale;

    public UpdateObjects(AppConfiguration appConfig, UserProfile user, Locale locale) {
        this.config = appConfig;
        this.user = user;
        this.locale = locale;
    }

    public List<SearchView.Item> findAllObjects() throws IOException, FedoraClientException {
        RemoteStorage remote = RemoteStorage.getInstance(config);
        SearchView search = remote.getSearch(locale);
        List<SearchView.Item> items = search.findAllObjects();
        return items;
    }

    public void setOrganization(List<SearchView.Item> items) throws DigitalObjectException {
        for (SearchView.Item item : items) {
            setOrganization(item.getPid());
        }
    }

    private void setOrganization(String pid) throws DigitalObjectException {
        try {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            FedoraObject fo = dom.find(pid, null);
            DigitalObjectHandler doh = dom.createHandler(fo);
            RelationEditor relations = doh.relations();
            relations.setOrganization(user.getOrganization());
            relations.write(relations.getLastModified(), "Add organization to foxml");
            doh.commit();
        } catch (DigitalObjectException ex)  {
            LOG.log(Level.WARNING, "Unable to update object " + pid);
        }
    }

    public Map<String, Integer> countObjects(List<SearchView.Item>  items) {
        Map<String, Integer> map = new HashMap<>();
        for (SearchView.Item item : items) {
            if (map.containsKey(item.getModel())) {
                Integer value = map.get(item.getModel());
                value++;
                map.put(item.getModel(), value);
            } else {
                map.put(item.getModel(), 1);
            }
        }
        return map;
    }
}
