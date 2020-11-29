$( document ).ready(function() {
  mdc.textField.MDCTextField.attachTo(document.querySelector('#startTimeField'));
  mdc.textField.MDCTextField.attachTo(document.querySelector('#endTimeField'));
  mdc.textField.MDCTextField.attachTo(document.querySelector('#tZeroField'));
  mdc.textField.MDCTextField.attachTo(document.querySelector('#dtField'));
  mdc.textField.MDCTextField.attachTo(document.querySelector('#sobolPointField'));

  //mdc.textField.MDCTextField.attachTo(document.querySelector('#zero'));
  /*
  const logScaleR0Checkbox = mdc.checkbox.MDCCheckbox(document.querySelector('#logScaleR0'));
  const logScaleR0FormField = mdc.formField.MDCFormField(document.querySelector('#logScaleR0Field'));
  logScaleR0FormField.input = checkbox;
*/
});
