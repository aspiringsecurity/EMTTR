const applicationJson = 'application/json';

function handleAnyResponse(xhr, form) {
  form.getElementsByTagName('input')['responseCode'].value = xhr.status;
  form.getElementsByTagName('textarea')['response'].value = prettyPrintJsonResponse(xhr);
}

function replaceAllPlaceholdersByFormCounterparts(formValue, form) {
  return formValue.replace(/{([^}]+)}/g, (s, group) => getFormValue(form, group) || getLoginFormValue(group));
}

async function createRequest(form, body = null) {
  const apiUrl = getLoginFormValue('apiUrl');
  const method = getFormValue(form, 'method');
  const path = replaceAllPlaceholdersByFormCounterparts(getFormValue(form, 'path'), form);
  let requestBody = body || getFormField(form, 'request')?.value || '';
  const accessToken = window.sessionStorage.getItem(accessTokenKey);

  const xhr = new XMLHttpRequest();
  if (getLoginFormField('invokeApiUrlDirectly').checked) {
    xhr.open(method, apiUrl + path, true);
    xhr.setRequestHeader('Authorization', 'Bearer ' + accessToken);
    if (requestBody && !(requestBody instanceof FormData)) {
      xhr.setRequestHeader('Content-Type', applicationJson);
    }
  } else {
    xhr.open('POST', 'proxy-call', true);
    xhr.setRequestHeader('Content-Type', applicationJson);
    const message = {
      accessToken: accessToken,
      method: method,
      apiUrl: apiUrl,
      path: path,
      body: requestBody
    };
    if (requestBody) {
      if (!(requestBody instanceof FormData)) {
        message.contentType = applicationJson;
      } else {
        message.body = await requestBody.entries().next().value[1].text();
      }
    }
    requestBody = JSON.stringify(message);
  }
  form.getElementsByTagName('input')['responseCode'].value = 'Waiting for response...';
  await invokeAsync(xhr, requestBody).then(() => handleAnyResponse(xhr, form), () => handleAnyResponse(xhr, form));

  return false;
}

function getRedirectUrl() {
  return window.location.origin + window.location.pathname;
}

async function doAuthorizationCodePkceFlow() {
  const authenticator = getLoginFormValue('authenticator');
  const clientId = getLoginFormValue('publicClientId');
  const codeVerifier = generateCodeVerifier();
  window.sessionStorage.setItem('codeVerifier', codeVerifier);

  let redirectUrl = getRedirectUrl();
  window.sessionStorage.setItem('redirectUrl', redirectUrl);

  const codeChallenge = await generateCodeChallengeFromVerifier(codeVerifier);
  const configuration = await getOpenIdConfiguration();

  const authenticationUrl = composePkceAuthenticationUrl(configuration, clientId, redirectUrl, codeChallenge, authenticator);
  window.location.assign(authenticationUrl);
  window.location.href = (authenticationUrl);
}

async function doPasswordFlow() {
  const configuration = await getOpenIdConfiguration();
  const formElement = document.getElementById('login');
  const clientId = getFormValue(formElement, 'confidentialClientId');
  const clientSecret = getFormValue(formElement, 'clientSecret');
  const username = getFormValue(formElement, 'username');
  const password = getFormValue(formElement, 'password');

  await promisePasswordFlow(configuration, clientId, clientSecret, username, password)
    .then(response => handleTokenEndpointResponse(response), failure => {
      alert(failure);
    });
}

function setOrClearSessionStorage(key, value) {
  if (value) {
    window.sessionStorage.setItem(key, value);
  } else {
    window.sessionStorage.removeItem(key);
  }
}

function handleTokenEndpointResponse(response) {
  window.sessionStorage.setItem(accessTokenKey, response.access_token);
  setOrClearSessionStorage(`${propertyScope}IdToken`, response.id_token);
  window.location.assign(getRedirectUrl());
}

async function login() {
  storeInputs(keys);
  storeInputs(['flow', 'confidentialClientId', 'clientSecret', 'username', 'password', 'publicClientId', 'authenticator'], propertyScope);

  if (getSelectedLoginRadioButton('flow') === 'password_flow') {
    await doPasswordFlow();
  } else {
    await doAuthorizationCodePkceFlow();
  }
  return false;
}

async function logout() {
  const configuration = await getOpenIdConfiguration();

  window.sessionStorage.removeItem(accessTokenKey);
  const endSessionEndpoint = new URL(configuration.end_session_endpoint);
  const idToken = window.sessionStorage.getItem(`${propertyScope}IdToken`);
  if (idToken) {
    endSessionEndpoint.searchParams.set('id_token_hint', idToken);
    endSessionEndpoint.searchParams.set('post_logout_redirect_uri', getRedirectUrl());
  }
  window.location.assign(endSessionEndpoint);
}

function setField(name, value) {
  const field = getField('login', name);
  if (field && value) {
    field.value = value;
  }
}

async function initialize(organization) {
  await loadProperties(organization);

  restoreInputs(keys);
  restoreInputs(['flow', 'confidentialClientId', 'clientSecret', 'username', 'password', 'publicClientId', 'authenticator'], propertyScope);

  const urlParams = new URLSearchParams(window.location.search);
  if (urlParams.has('code')) {
    const openIdConfiguration = await getOpenIdConfiguration();
    const codeVerifier = window.sessionStorage.getItem('codeVerifier');
    const redirectUrl = window.sessionStorage.getItem('redirectUrl');
    const clientId = getLoginFormValue('publicClientId');

    await promiseValidateToken(openIdConfiguration, clientId, urlParams, redirectUrl, codeVerifier)
      .then(response => handleTokenEndpointResponse(response));
  }
  const tokenField = getLoginFormField('token')
  tokenField.value = window.sessionStorage.getItem(accessTokenKey)

  let redirectUrlElement = document.getElementById('publicRedirectUrl');
  redirectUrlElement.value = `${window.location.origin}/.*`
}

async function loadProperties(organization) {
  const configuration = await httpGet(`${window.location.origin}/configuration.json`);
  if (configuration && configuration['properties']) {
    const properties = configuration['properties'];
    for (const field of ['organizationId', 'tenantId', 'apiUrl']) {
      setField(field, properties[field]);
    }
    const prefix = organization ? 'organization' : 'tenant';
    for (const [key, value] of Object.entries(properties[prefix])) {
      setField(key, value);
    }
  }
}
