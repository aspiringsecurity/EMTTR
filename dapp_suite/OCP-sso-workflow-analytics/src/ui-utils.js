function getField(formName, fieldName) {
  return getFormField(document.getElementById(formName), fieldName);
}

function getFormField(form, fieldName) {
  return form.querySelector('[name=\'' + fieldName + '\']');
}

function getFormValue(form, fieldName) {
  return getFormField(form, fieldName)?.value;
}

function getSelectedLoginRadioButton(fieldName) {
  const inputs = document.getElementById('login').getElementsByTagName('input');
  for (const item of inputs) {
    if (item.type === 'radio' && item.name === fieldName && item.checked) {
      return item.value;
    }
  }
  return undefined;
}

function setSelectedLoginRadioButton(fieldName, value) {
  const inputs = document.getElementById('login').getElementsByTagName('input');
  for (const item of inputs) {
    if (item.type === 'radio' && item.name === fieldName && item.value === value) {
      item.checked = true;
      return;
    }
  }
}

function getLoginFormField(field) {
  return getField('login', field);
}

function getLoginFormValue(field) {
  return getLoginFormField(field).value;
}

function storeInputs(keys, prefix = null, ) {
  for (const key of keys) {
    const loginFormField = getLoginFormField(key);
    let v;
    switch (loginFormField.type) {
      case 'checkbox':
        v =  loginFormField.checked;
        break;
      case 'radio':
        v = getSelectedLoginRadioButton(key);
        break;
      default:
        v = loginFormField.value;
    }
    window.sessionStorage.setItem((prefix ? `${prefix}.` : '') + key, v);
  }
}

function restoreInputs(keys, prefix = null) {
  for (const key of keys) {
    const item = window.sessionStorage.getItem((prefix ? `${prefix}.` : '') + key);
    if (item != null) {
      const loginFormField = getLoginFormField(key);
      if (loginFormField.type === 'checkbox') {
        loginFormField.checked = item === 'true';
      } else if (loginFormField.type === 'radio') {
        setSelectedLoginRadioButton(key, item);
      } else {
        loginFormField.value = item;
      }
    }
  }
}
