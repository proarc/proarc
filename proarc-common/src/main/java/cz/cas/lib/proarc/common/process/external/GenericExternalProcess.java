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
package cz.cas.lib.proarc.common.process.external;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.io.FilenameUtils;

/**
 * The experimental implementation of an external process that can be customized
 * in more details including input and output parameters.
 *
 * @author Jan Pokorsky
 */
public class GenericExternalProcess extends ExternalProcess {

    public static final String DST_PARENT = "output.folder";
    public static final String DST_PATH = "output.file";
    public static final String SRC_EXT = "input.file.ext";
    public static final String SRC_NAME = "input.file.name";
    public static final String SRC_NAME_EXT = "input.file.nameExt";
    public static final String SRC_PATH = "input.file";
    public static final String SRC_PARENT = "input.folder";

    private File inputFile;
    private File outputFile;
    private final ParamHandler parameters;
    private final Configuration conf;
    private static final Pattern REPLACE_PARAM_PATTERN = Pattern.compile("\\$\\{([^}]+)\\}");
    private boolean skippedProcess;
    private ProcessResult processResult;

    public GenericExternalProcess(Configuration conf) {
        super(conf);
        this.conf = conf;
        this.parameters = new ParamHandler();
    }

    public GenericExternalProcess addInputFile(File file) {
        this.inputFile = file;
        parameters.addInputFile(file);
        return this;
    }

    public File getInputFile() {
        return inputFile;
    }

    public GenericExternalProcess addOutputFile(File file) {
        this.outputFile = file;
        parameters.addOutput(file);
        return this;
    }

    public File getOutputFile() {
        return processResult == null ? outputFile : getResult().getOutputFile();
    }

    public ParamHandler getParameters() {
        return parameters;
    }

    public Map<String, String> getResultParameters() {
        return getResult().getParameters();
    }

    public ProcessResult getResult() {
        if (processResult == null) {
            throw new IllegalStateException("The process has not run yet!");
        }
        return processResult;
    }

    @Override
    public void run() {
        Configuration run = conf.subset("run");
        String[] ifNotExists = run.getStringArray("if.notExists");
        for (String ifNotExist : ifNotExists) {
            String path = interpolateParameters(ifNotExist, parameters.getMap());
            File file = new File(path);
            if (file.exists()) {
                skippedProcess = true;
                break;
            }
        }
        if (!skippedProcess) {
            super.run();
        }
        processResult = ProcessResult.getResultParameters(conf,
                parameters.getMap(), isSkippedProcess(), getExitCode(), getFullOutput());
    }

    public boolean isSkippedProcess() {
        return skippedProcess;
    }

    @Override
    protected List<String> buildCmdLine(Configuration conf) {
        List<String> cmdLine = super.buildCmdLine(conf);
        interpolateParameters(cmdLine);
        return cmdLine;
    }

    /**
     * Interpolate parameter values. The replace pattern is {@code ${name}}.
     * @param s a string to search for placeholders
     * @return the resolved string
     * @see #addParameter(java.lang.String, java.lang.String)
     */
    static String interpolateParameters(String s, Map<String, String> parameters) {
        if (s == null || s.length() < 4 || parameters.isEmpty()) { // minimal replaceable value ${x}
            return s;
        }
        // finds ${name} patterns
        Matcher m = REPLACE_PARAM_PATTERN.matcher(s);
        StringBuffer sb = null;
        while(m.find()) {
            if (m.groupCount() == 1) {
                String param = m.group(1);
                String replacement = parameters.get(param);
                if (replacement != null) {
                    sb = sb != null ? sb : new StringBuffer();
                    m.appendReplacement(sb, Matcher.quoteReplacement(replacement));
                }
            }
        }
        if (sb == null) {
            return s;
        }
        m.appendTail(sb);
        return sb.toString();
    }

    void interpolateParameters(List<String> cmdLine) {
        if (parameters.isEmpty()) {
            return ;
        }
        for (ListIterator<String> it = cmdLine.listIterator(); it.hasNext();) {
            it.set(interpolateParameters(it.next(), parameters.getMap()));
        }
    }

    public static class ParamHandler {

        private final Map<String, String> parameters = new HashMap<String, String>();

        public ParamHandler add(Map<String, String> parameters) {
            this.parameters.putAll(parameters);
            return this;
        }

        public ParamHandler add(String name, String value) {
            parameters.put(name, value);
            return this;
        }

        public ParamHandler addInputFile(File file) {
            String nameExt = file.getName();
            return add(SRC_PATH, file.getAbsolutePath())
                    .add(SRC_EXT, FilenameUtils.getExtension(nameExt))
                    .add(SRC_NAME, FilenameUtils.getBaseName(nameExt))
                    .add(SRC_NAME_EXT, nameExt)
                    .add(SRC_PARENT, file.getParentFile().getPath());
        }

        public ParamHandler addOutput(File file) {
            return add(DST_PATH, file.getAbsolutePath())
                    .add(DST_PARENT, file.getParentFile().getPath());
        }

        public Map<String, String> getMap() {
            return parameters;
        }

        public boolean isEmpty() {
            return parameters.isEmpty();
        }

    }
    
    public static class ProcessResult {

        private final Map<String, String> parameters;
        private final ExitStrategy exit;
        private final String processorId;

        public ProcessResult(String processorId, Map<String, String> params, ExitStrategy exit) {
            this.parameters = params;
            this.exit = exit;
            this.processorId = processorId;
        }

        public Map<String, String> getParameters() {
            return parameters;
        }

        public File getOutputFile() {
            String path = parameters.get(processorId + ".param." + DST_PATH);
            if (path == null) {
                path = parameters.get(DST_PATH);
            }
            return path == null || path.isEmpty() ? null : new File(path);
        }

        public ExitStrategy getExit() {
            return exit;
        }

        /**
         * Merges result parameters. {@code onExit} and {@code onSkip} are experimental features.
         *
         * @param conf
         * @param inputParameters
         * @param skippedProcess
         * @param exitCode
         * @param log
         * @return
         */
        public static ProcessResult getResultParameters(Configuration conf,
                Map<String, String> inputParameters, boolean skippedProcess,
                int exitCode, String log) {

            // gets <processorId>.param.* parameters with interpolated values
            // it allows to share properties among process and read helper values from process declaration (mime, output file, ...)
            String processorId = conf.getString("id");
            Map<String, String> hm = new HashMap<String, String>(inputParameters);
            addResultParamaters(conf, processorId, hm);

            ExitStrategy exit = new ExitStrategy();
            if (skippedProcess) {
                Configuration onSkip = conf.subset("onSkip");
                addResultParamaters(onSkip, processorId, hm);
                exit.setSkip(true);
                exit.setContinueWithProcessIds(Arrays.asList(onSkip.getStringArray("next")));
                return new ProcessResult(processorId, hm, exit);
            }

            exit.setExitCode(exitCode);
            String[] onExitIds = conf.getStringArray("onExits");
            Configuration onExitConf = conf.subset("onExit");
            boolean defaultExit = true;
            for (String onExitId : onExitIds) {
                if (isExitId(onExitId, exitCode)) {
                    Configuration onExitIdConf = onExitConf.subset(onExitId);
                    addResultParamaters(onExitIdConf, processorId, hm);
                    exit.setErrorMessage(onExitIdConf.getString("message"));
                    exit.setStop(onExitIdConf.getBoolean("stop", exitCode != 0));
                    exit.setContinueWithProcessIds(Arrays.asList(onExitIdConf.getStringArray("next")));
                    defaultExit = false;
                    break;
                }
            }
            if (defaultExit) {
                exit.setStop(exitCode != 0);
                exit.setErrorMessage(log);
            }
            return new ProcessResult(processorId, hm, exit);
        }

        private static Map<String, String> addResultParamaters(Configuration conf,
                String processorId, Map<String, String> result) {

            for (Iterator<String> it = conf.getKeys("param"); it.hasNext();) {
                String param = it.next();
                String value = interpolateParameters(conf.getString(param), result);
                String resultName = processorId == null ? param : processorId + '.' + param;
                result.put(resultName, value);
            }
            return result;
        }

        /**
         * Resolves whether the {@code exitExpression} matches to {@code exitCode}.
         * @param exitExpression syntax:
         *      {@code '*' | '>' exitCode | '<' exitCode | exitCode [',' exitCode]*}
         * @param exitCode
         * @return
         */
        private static boolean isExitId(String exitExpression, int exitCode) {
            boolean match = false;
            if ("*".equals(exitExpression)) {
                match = true;
            } else if (exitExpression.charAt(0) == '>') {
                try {
                    int code = Integer.parseInt(exitExpression.substring(1));
                    if (exitCode > code) {
                        match = true;
                    }
                } catch (NumberFormatException ex) {
                }
            } else if (exitExpression.charAt(0) == '<') {
                try {
                    int code = Integer.parseInt(exitExpression.substring(1));
                    if (exitCode < code) {
                        match = true;
                    }
                } catch (NumberFormatException ex) {
                }
            } else {
                String[] codes = exitExpression.split(",");
                for (String codeStr : codes) {
                    try {
                        int code = Integer.parseInt(codeStr.trim());
                        if (exitCode == code) {
                            match = true;
                        }
                    } catch (NumberFormatException ex) {
                    }
                }
            }
            return match;
        }
    }

    public static class ExitStrategy {

        private Integer exitCode;
        private boolean stop;
        private boolean skip;
        private List<String> continueWithProcessIds;
        private String errorMessage;

        public Integer getExitCode() {
            return exitCode;
        }

        public void setExitCode(Integer exitCode) {
            this.exitCode = exitCode;
        }

        public boolean isStop() {
            return stop;
        }

        public void setStop(boolean stop) {
            this.stop = stop;
        }

        public boolean isSkip() {
            return skip;
        }

        public void setSkip(boolean skip) {
            this.skip = skip;
        }

        public List<String> getContinueWithProcessIds() {
            return continueWithProcessIds;
        }

        public void setContinueWithProcessIds(List<String> continueWithProcessIds) {
            this.continueWithProcessIds = continueWithProcessIds;
        }

        public String getErrorMessage() {
            return errorMessage;
        }

        public void setErrorMessage(String errorMessage) {
            this.errorMessage = errorMessage;
        }

    }

}
