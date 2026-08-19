package cz.cas.lib.proarc.common.object.model;

import cz.cas.lib.proarc.common.object.DigitalObjectPlugin;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.ServiceLoader;
import java.util.Set;
import java.util.function.Predicate;

public final class MetaModelUtils {

    public static final String ALL_MODELS_CONST = "ALL";
    public static final String LEAF_MODELS_CONST = "LEAF";
    public static final String TOP_MODELS_CONST = "TOP";
    public static final String NON_TOP_MODELS_CONST = "NON_TOP";

    private static final List<MetaModel> MODELS = loadModels();

    public static final String[] ALL_MODELS = modelPids(model -> true);
    public static final String[] LEAF_MODELS = modelPids(model -> Boolean.TRUE.equals(model.isLeaf()));
    public static final String[] TOP_MODELS = modelPids(model -> Boolean.TRUE.equals(model.isRoot()));
    public static final String[] NON_TOP_MODELS = difference(ALL_MODELS, TOP_MODELS);

    private MetaModelUtils() {
    }

    private static List<MetaModel> loadModels() {
        List<MetaModel> models = new ArrayList<>();
        ServiceLoader.load(DigitalObjectPlugin.class)
                .forEach(plugin -> models.addAll(plugin.getModel()));
        return models;
    }

    private static String[] modelPids(Predicate<MetaModel> predicate) {
        return MODELS.stream()
                .filter(predicate)
                .map(MetaModel::getPid)
                .distinct()
                .toArray(String[]::new);
    }

    private static String[] difference(String[] models, String[] excludedModels) {
        Set<String> excluded = new HashSet<>(Arrays.asList(excludedModels));
        return Arrays.stream(models)
                .filter(model -> !excluded.contains(model))
                .toArray(String[]::new);
    }
}
