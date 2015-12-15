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
package cz.cas.lib.proarc.common.workflow;

import cz.cas.lib.proarc.common.fedora.DigitalObjectValidationException.ValidationResult;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowException extends Exception {

    public static final String INVALID_XML_MSG = "Workflow_InvalidXml_Msg";
    public static final String INVALID_XML_ID_MSG = "Workflow_InvalidXmlId_Msg";
    public static final String JOB_IS_CLOSED_MSG = "Workflow_JobIsClosed_Msg";
    public static final String JOB_NOT_FOUND_MSG = "Workflow_JobNotFound_Msg";
    public static final String MATERIAL_NOT_FOUND_MSG = "Workflow_MaterialNotFound_Msg";
    public static final String PARAM_UPDATE_FAILED_MSG = "Invalid parameter {0}!";
    public static final String PARAM_NOT_NUMBER_MSG = "Workflow_ParamNotNumber_Msg";
    public static final String PARAM_NOT_DATETIME_MSG = "Workflow_ParamNotDateTime_Msg";
    public static final String TASK_BLOCKED_MSG = "Workflow_TaskBlocked_Msg";
    public static final String TASK_CANNOT_WAIT_AGAIN_MSG = "Workflow_TaskCannotWaitAgain_Msg";
    public static final String TASK_DISABLED_MSG = "Workflow_TaskDisabled";
    public static final String TASK_NOT_FOUND_MSG = "Workflow_TaskNotFound_Msg";
    public static final String UNEXPECTED_ERROR_MSG_MSG = "Err_UnexpectedError_Msg";

    private final List<ValidationResult> validations = new ArrayList<ValidationResult>();

    public WorkflowException(String message) {
        super(message);
    }

    public WorkflowException(String message, Throwable cause) {
        super(message, cause);
    }

    public WorkflowException copy(WorkflowException ex) {
        validations.addAll(ex.getValidations());
        return this;
    }

    public WorkflowException addUnexpectedError() {
        return addValidation(null, UNEXPECTED_ERROR_MSG_MSG);
    }

    public WorkflowException addInvalidXml() {
        return addValidation(null, INVALID_XML_MSG);
    }

    public WorkflowException addInvalidXmlId(String xmlId) {
        return addValidation(null, INVALID_XML_ID_MSG, xmlId);
    }

    public WorkflowException addJobIsClosed() {
        return addValidation(null, JOB_IS_CLOSED_MSG);
    }

    public WorkflowException addJobNotFound(Number jobId) {
        return addValidation(null, JOB_NOT_FOUND_MSG, jobId);
    }

    public WorkflowException addMaterialNotFound(Number jobId) {
        return addValidation(null, MATERIAL_NOT_FOUND_MSG, jobId);
    }

    public WorkflowException addParamNumberFormat(String paramName, Object value) {
        return addValidation(null, PARAM_NOT_NUMBER_MSG, paramName, value);
    }

    public WorkflowException addParamDateTimeFormat(String paramName, Object value) {
        return addValidation(null, PARAM_NOT_DATETIME_MSG, paramName, value);
    }

    public WorkflowException addParamUpdateFailed(String paramName) {
        return addValidation(null, PARAM_UPDATE_FAILED_MSG, paramName);
    }

    public WorkflowException addTaskBlocked() {
        return addValidation(null, TASK_BLOCKED_MSG);
    }

    public WorkflowException addTaskCannotWaitAgain() {
        return addValidation(null, TASK_CANNOT_WAIT_AGAIN_MSG);
    }

    public WorkflowException addTaskDisabled(String taskXmlId) {
        return addValidation(null, TASK_DISABLED_MSG, taskXmlId);
    }

    public WorkflowException addTaskNotFound(Number taskId) {
        return addValidation(null, TASK_NOT_FOUND_MSG, taskId);
    }

    public WorkflowException addValidation(String name, String bundleKey, Object... values) {
        ValidationResult vr = new ValidationResult();
        vr.setName(name);
        vr.setBundleKey(bundleKey);
        vr.setValues(values);
        validations.add(vr);
        return this;
    }

    public List<ValidationResult> getValidations() {
        return validations;
    }

    @Override
    public String toString() {
        return super.toString();
    }

}
