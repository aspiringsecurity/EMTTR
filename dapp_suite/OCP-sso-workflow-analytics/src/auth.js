function promiseOpenIdConfiguration(infix) {
  const apiUrl = getLoginFormValue('apiUrl');
  const url = `${apiUrl}/${infix}/.well-known/openid-configuration`;
  return httpGet(url);
}

function promiseOrganizationOpenIdConfiguration(organizationId) {
  return promiseOpenIdConfiguration('organizations/' + organizationId);
}

function promiseTenantOpenIdConfiguration(tenantId) {
  return promiseOpenIdConfiguration('tenants/' + tenantId);
}

function composePkceAuthenticationUrl(configuration, clientId, redirectUrl, codeChallenge, authenticator) {
  return configuration.authorization_endpoint +
    '?client_id=' + encodeURIComponent(clientId) +
    '&response_type=code' +
    '&redirect_uri=' + encodeURIComponent(redirectUrl) +
    '&code_challenge=' + codeChallenge +
    '&code_challenge_method=S256' +
      (authenticator ? ('&authhandler=' + authenticator) : '');
}

function promisePasswordFlow(configuration, clientId, clientSecret, username, password) {
  const xhr = new XMLHttpRequest();
  xhr.open('POST', configuration.token_endpoint, true);
  xhr.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
  return invokeAsync(xhr, 'client_id=' + encodeURIComponent(clientId) +
    '&client_secret=' + encodeURIComponent(clientSecret) +
    '&grant_type=password' +
    '&username=' + encodeURIComponent(username) +
    '&password=' + encodeURIComponent(password)
  ).then(() => JSON.parse(xhr.responseText));
}

function promiseValidateToken(configuration, clientId, urlParams, redirectUrl, codeVerifier) {
  const xhr = new XMLHttpRequest();
  xhr.open('POST', configuration.token_endpoint, true);
  xhr.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
  return invokeAsync(xhr, 'client_id=' + encodeURIComponent(clientId) +
    '&grant_type=authorization_code' +
    '&code=' + encodeURIComponent(urlParams.get('code')) +
    '&redirect_uri=' + encodeURIComponent(redirectUrl) +
    '&code_verifier=' + encodeURIComponent(codeVerifier)
  ).then(() => JSON.parse(xhr.responseText));
}
