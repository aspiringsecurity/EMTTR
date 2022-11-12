import React from 'react';
import { FormGroup, ControlLabel, FormControl, HelpBlock, InputGroup } from 'react-bootstrap';

/**
 * Creates a from group with label and form control with options for feedback, help, and addons.
 * 
 * @param {config} configObj
 *      * id              {String}          (required) The FormGroup controlId.
 *      * label           {String}          (required) The control's label text.
 *      * help            {String}          (optional) The help text below the control.
 *      * validationState {String}          (required) Sets the validation state of the control. One of: 'success', 'warning', 'error', null.
 *      * hasFeedback     {Boolean}            (optional) Whether or not to show the form control feedback (ie green check mark for 'success' validation state).
 *      * inputAddOn      {Object}          (optional) Input add on to be prepended or appended to the form control.
 *         ** location    {String}          (required) Placement of the addon. One of 'before', 'after'.
 *         ** addOn       {React.Component} (required) React component or string to represent the add on. ie '$' or <Button>Update</Button>.
 *      * ...props        {Any}             (optional) Any other props passed in will be appended as props to the FormControl component, ie type='file'.
 * 
 * @example
 * <FieldGroup
 *   type="text"
 *   value={this.state.username}
 *   disabled={isLoading}
 *   placeholder="germany2018champs"
 *   onChange={(e) => this._handleChange(e)}
 *   name="username"
 *   autoComplete="off"
 *   label="Desired username"
 *   validationState={validationState}
 *   hasFeedback={true}
 *   help={feedback}
 *   inputAddOn={
 *     {
 *       location: 'before',
 *       addOn: '$'
 *     }
 *   }
 * />
 * 
 * @returns {React.Component} The completed component to render.
 */
const FieldGroup = ({ id, label, help, validationState, hasFeedback, inputAddOn, ...props }) => {
  return (
    <React.Fragment>
      <FormGroup controlId={id} validationState={validationState}>
        <ControlLabel>{label}</ControlLabel>
        
        { inputAddOn ? 
          <InputGroup>
            { inputAddOn.location === 'before' ? <InputGroup.Addon>{ inputAddOn.addOn }</InputGroup.Addon> : '' }
            <FormControl {...props} />
            { inputAddOn.location === 'after' ? <InputGroup.Addon>{ inputAddOn.addOn }</InputGroup.Addon> : '' }
          </InputGroup>
          :
          <FormControl {...props}>{ props.children }</FormControl>
        }
        {hasFeedback ? <FormControl.Feedback /> : ''}
        {help && <HelpBlock>{help}</HelpBlock>}
      </FormGroup>
      
    </React.Fragment>
  );
}

export default FieldGroup;