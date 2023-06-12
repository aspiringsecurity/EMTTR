import React from 'react'
import PropTypes from 'prop-types'
import { Form, Formik } from 'formik'

const FormikForm = ({ children, initialValues, onSubmit, ...rest }) => (
  <Formik initialValues={initialValues} onSubmit={onSubmit} {...rest}>
    <Form>{children}</Form>
  </Formik>
)

FormikForm.propTypes = {
  children: PropTypes.node.isRequired,
  initialValues: PropTypes.object.isRequired,
  onSubmit: PropTypes.func.isRequired
}

export default FormikForm
