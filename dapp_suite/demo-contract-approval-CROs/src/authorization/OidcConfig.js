const OidcConfig = {
  authority: `${process.env.REACT_APP_BASE_SERVICE_URL}/tenants/${process.env.REACT_APP_TENANT_ID}`,
  client_id: process.env.REACT_APP_CLIENT_ID,
  redirect_uri: process.env.REACT_APP_REDIRECT_URI,
  response_type: 'code',
  scope: 'openid otds:groups',
  post_logout_redirect_uri: process.env.REACT_APP_REDIRECT_URI,
};

export default OidcConfig;
