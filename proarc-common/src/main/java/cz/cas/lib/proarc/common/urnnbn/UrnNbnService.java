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
package cz.cas.lib.proarc.common.urnnbn;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnStatusHandler.Status;
import cz.cas.lib.proarc.urnnbn.ResolverClient;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The service to register digital objects to the URN:NBN resolver.
 *
 * @author Jan Pokorsky
 */
public class UrnNbnService {

    private final DigitalObjectManager dom;
    private final SearchView search;
    private ResolverClient client;

    public UrnNbnService(AppConfiguration appConfig, UrnNbnConfiguration.ResolverConfiguration resolverConfig) {
        dom = DigitalObjectManager.getDefault();
        if (resolverConfig != null) {
            this.client = appConfig.getUrnNbnConfiguration().getClient(resolverConfig);
        }
        try {
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                search = RemoteStorage.getInstance().getSearch();
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
                search = AkubraStorage.getInstance(akubraConfiguration).getSearch();
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    public UrnNbnService(AppConfiguration appConfig) {
        this(appConfig, null);
    }

    public UrnNbnStatusHandler register(String pid, boolean hierarchy) {
        return register(Collections.singleton(pid), hierarchy);
    }

    public UrnNbnStatusHandler invalidateValue(String pid) {
        return invalidateValue(Collections.singleton(pid));
    }

    public UrnNbnStatusHandler register(Collection<String> pids, boolean hierarchy) {
        LinkedHashSet<String> queue = new LinkedHashSet<String>(pids);
        UrnNbnStatusHandler statusHandler = new UrnNbnStatusHandler();
        DigitalObjectCrawler crawler = new DigitalObjectCrawler(dom, search, null);
        UrnNbnVisitor reg = new UrnNbnVisitor(crawler);
        UrnNbnContext ctx = new UrnNbnContext();
        ctx.setStatus(statusHandler);
        ctx.setClient(client);
        ctx.setRegisterNewPid(true);

        if (initJhove(ctx) == null) {
            return statusHandler;
        }
        try {
            for (String pid : pids) {
                queue.remove(pid);
                try {
                    DigitalObjectElement elm = crawler.getEntry(pid);
                    elm.accept(reg, ctx);
                } catch (Exception ex) {
                    Logger.getLogger(UrnNbnService.class.getName()).log(Level.SEVERE, null, ex);
                    statusHandler.error(pid, ex);
                    break;
                }
            }
        } finally {
            ctx.getJhoveContext().destroy();
        }
        for (String pid : queue) {
            statusHandler.warning(pid, Status.NOT_PROCESSED, "Not processed! \n" + pid, null);
        }
        return statusHandler;
    }

    public UrnNbnStatusHandler invalidateValue(Collection<String> pids) {
        LinkedHashSet<String> queue = new LinkedHashSet<>(pids);
        UrnNbnStatusHandler statusHandler = new UrnNbnStatusHandler();
        DigitalObjectCrawler crawler = new DigitalObjectCrawler(dom, search, null);
        UrnNbnVisitor req = new UrnNbnVisitor(crawler);
        UrnNbnContext context = new UrnNbnContext();
        context.setStatus(statusHandler);
        context.setInvalidateUrnNbn(true);

        try {
            for (String pid : pids) {
                queue.remove(pid);
                try {
                    DigitalObjectElement elm = crawler.getEntry(pid);
                    elm.accept(req, context);
                } catch (Exception ex) {
                    Logger.getLogger(UrnNbnService.class.getName()).log(Level.SEVERE, null, ex);
                    statusHandler.error(pid, ex);
                    break;
                }
            }
        } finally {
            if (context.getJhoveContext() != null) {
                context.getJhoveContext().destroy();
            }
        }
        for (String pid : queue) {
            statusHandler.warning(pid, Status.NOT_PROCESSED, "Not processed! \n" + pid, null);
        }
        return statusHandler;
    }

    private JhoveContext initJhove(UrnNbnContext ctx) {
        try {
            ctx.setJhoveContext(JhoveUtility.createContext());
        } catch (MetsExportException ex) {
            ctx.getStatus().error((DigitalObjectElement) null, ex);
        }
        return ctx.getJhoveContext();
    }

}
