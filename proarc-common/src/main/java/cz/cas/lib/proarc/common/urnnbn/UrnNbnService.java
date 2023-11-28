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
import cz.cas.lib.proarc.urnnbn.model.response.ErrorType;
import cz.cas.lib.proarc.urnnbn.model.response.RegistrarScopeIdentifier;
import cz.cas.lib.proarc.urnnbn.model.response.UrnNbn;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The service to operate with digital objects in the URN:NBN resolver.
 *
 * @author Jan Pokorsky
 */
public class UrnNbnService {

    private static final Logger LOG = Logger.getLogger(UrnNbnService.class.getName());

    private final DigitalObjectManager dom;
    private final SearchView search;
    private ResolverClient client;

    private final static String URNNBN_PREFIX = "urn:nbn:cz:";

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

    public UrnNbnStatusHandler createSuccessor(Collection<String> pids, boolean hierarchy) {
        LinkedHashSet<String> queue = new LinkedHashSet<String>(pids);
        UrnNbnStatusHandler statusHandler = new UrnNbnStatusHandler();
        DigitalObjectCrawler crawler = new DigitalObjectCrawler(dom, search, null);
        UrnNbnVisitor reg = new UrnNbnVisitor(crawler);
        UrnNbnContext ctx = new UrnNbnContext();
        ctx.setStatus(statusHandler);
        ctx.setClient(client);
        ctx.setCreateSuccessor(true);
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
            if (ctx.getJhoveContext() != null) {
                ctx.getJhoveContext().destroy();
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

    public UrnNbnStatusHandler invalidateRemoteValue(String urnNbnValue, boolean hierarchy) {
        UrnNbnStatusHandler statusHandler = new UrnNbnStatusHandler();
        UrnNbnContext ctx = new UrnNbnContext();
        ctx.setStatus(statusHandler);
        ctx.setClient(client);

        if (!urnNbnValue.startsWith(URNNBN_PREFIX)) {
            urnNbnValue = URNNBN_PREFIX + urnNbnValue;
        }

        try {
            cz.cas.lib.proarc.urnnbn.model.response.Response response = ctx.getClient().deactivateUrnNbnValue(urnNbnValue);;
            ErrorType error = response.getError();
            if (error != null) {
                // remote deactivation failed
                LOG.log(Level.SEVERE, "{0}: {1}: {2}", new Object[]{urnNbnValue, error.getCode(), error.getMessage()});
                if ("INCORRECT_URN_NBN_STATE".equals(error.getCode().name())) {
                    if (error.getMessage().endsWith("FREE")) {
                        statusHandler.error(urnNbnValue, Status.EXCEPTION, urnNbnValue + " was never used");
                    } else {
                        statusHandler.error(urnNbnValue, Status.EXCEPTION, "Already deactivated " + urnNbnValue);
                    }
                } else {
                    statusHandler.error(urnNbnValue, Status.EXCEPTION, error.getCode() + ": " + error.getMessage());
                }
                return statusHandler;
            }
            UrnNbn urnNbn = response.getUrnNbn();
            if (urnNbn == null || urnNbn.getValue() == null) {
                statusHandler.error(urnNbnValue, Status.EXCEPTION,
                        "The resolver returns no URN:NBN value! Check the server configuration.");
            } else {
                if ("DEACTIVATED".equals(urnNbn.getStatus())) {
                    response = ctx.getClient().removeUuidCzidloRecord(urnNbnValue);
                    error = response.getError();
                    if (error != null) {
                        // remote registration failed
                        statusHandler.error(urnNbnValue, Status.EXCEPTION, error.getCode() + ": " + error.getMessage());
                        LOG.log(Level.SEVERE, "{0}: {1}: {2}",
                                new Object[]{urnNbnValue, error.getCode(), error.getMessage()});
                        return statusHandler;
                    }
                    RegistrarScopeIdentifier registrarScopeIdentifier = response.getId();
                    if (registrarScopeIdentifier == null || registrarScopeIdentifier.getValue() == null || registrarScopeIdentifier.getType() == null) {
                        statusHandler.warning(urnNbnValue, Status.EXCEPTION, "The resolver returns no UUID value! Check the server configuration.", urnNbnValue);
                    } else {
                        statusHandler.ok(registrarScopeIdentifier.getType() + ":" + registrarScopeIdentifier.getValue(), "Deactivated " + urnNbnValue);
                    }
                } else {
                    statusHandler.error(urnNbnValue, Status.EXCEPTION, urnNbn.getStatus() + ":" + urnNbn.getValue());
                }
            }
        } catch (Exception ex) {
            // unexpected remote registration failure
            statusHandler.error(urnNbnValue, ex);
            return null;
        }
        return statusHandler;
    }

    public UrnNbnStatusHandler registerAgain(Collection<String> pids, boolean hierarchy) {
        LinkedHashSet<String> queue = new LinkedHashSet<String>(pids);
        UrnNbnStatusHandler statusHandler = new UrnNbnStatusHandler();
        DigitalObjectCrawler crawler = new DigitalObjectCrawler(dom, search, null);
        UrnNbnVisitor reg = new UrnNbnVisitor(crawler);
        UrnNbnContext ctx = new UrnNbnContext();
        ctx.setStatus(statusHandler);
        ctx.setClient(client);
        ctx.setRegisterAgainPid(true);

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


    public UrnNbnStatusHandler updateCzidloRecord(Collection<String> pids, String identifier, String operation, boolean hierarchy) {
        LinkedHashSet<String> queue = new LinkedHashSet<String>(pids);
        UrnNbnStatusHandler statusHandler = new UrnNbnStatusHandler();
        DigitalObjectCrawler crawler = new DigitalObjectCrawler(dom, search, null);
        UrnNbnVisitor reg = new UrnNbnVisitor(crawler);
        UrnNbnContext ctx = new UrnNbnContext();
        ctx.setStatus(statusHandler);
        ctx.setClient(client);
        ctx.setUpdateCzidloRecord(true);
        ctx.setUpdateCzidloRecordOperation(operation);
        ctx.setUpdateCzidloRecordIdentifier(identifier);

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
}
