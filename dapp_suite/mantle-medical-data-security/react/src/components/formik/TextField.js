import React from 'react'
import PropTypes from 'prop-types'
import MuiTextField from '@material-ui/core/TextField'
import { Field } from 'formik'
import { isEmpty, get } from 'lodash'

/* :: (object, object, object) -> object */
export const getTextFieldProps = (props, formikProps, formikForm) => {
  const { name, type } = props
  const fieldError = get(formikForm.errors, name)
  const showError = get(formikForm.touched, name) && !isEmpty(fieldError)

  const inputProps = [ 'email', 'password' ].includes(type)
    ? {
      ...props.inputProps,
      spellCheck: false,
      autoCapitalize: 'none',
      autoCorrect: 'off'
    }
    : props.inputProps

  const textFieldProps = {
    ...props,
    ...formikProps,
    inputProps,
    value: formikProps.value || '',
    id: props.id || props.name,
    error: showError,
    helperText: showError ? fieldError : props.helperText,
    disabled: formikForm.isSubmitting || props.disabled
  }

  return textFieldProps
}

const FormikTextField = props => (
  <Field name={props.name}>
    {({ field, form }) => (
      <MuiTextField {...getTextFieldProps(props, field, form)} />
    )}
  </Field>
)

FormikTextField.propTypes = {
  ...MuiTextField.propTypes,
  name: PropTypes.string.isRequired
}

export default FormikTextField
