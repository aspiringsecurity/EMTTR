# Description
This project contains a sample project that demonstrates the REST service calls required to login to OCP and 
access information management services (IMS) from OpenText.

The sample code is written in plain javascript and the user can choose whether to run the code in the browser 
or from a backend Node server.

When writing your own application you can also use existing OpenID clients, e.g. as published
on [https://openid.net/developers/certified/](https://openid.net/developers/certified/).

# Requirements
To run the sample webapp, the following tools are required to be installed:

1. Git client (e.g. from https://git-scm.com/download)
2. NPM version 6 or later and Node.js version 12 later (e.g. from https://nodejs.org/en/download/)

# How to start

Start a command prompt in the sample app root directory and make sure all npm dependencies are installed:

```shell
npm install
```

Run the sample application with:

```shell
npm start
```

You can configure the following options on startup (you can also set these in _properties.json_ before startup):
1. **hostname**: Provide your fully qualified hostname. Needed when using the built-in test idp, 
   otherwise the default value 'localhost' is fine. 
2. **https**: Do you want to run with https? (true/false) It is safer to use this. 
   When used you will get a security warning in your browser, which you will need to over-ride, 
   because the server runs with a self-signed certificate.

When all questions are answered it will print at the end the url of the sample application:

```text
Sample Application Running, you can now connect to: https://localhost:4000/
```

This indicates that the app is started.

# How to use
First some tips:
1. When going through the steps below always make sure you make a copy of the responses, or at least the _ids_ in the responses, so you can reuse and cleanup the objects you create.
2. Once you have completed the three stages, provide the configuration you have in the _properties.json_ file and restart the sample application. These fields are then pre-filled in the UI.
3. Open the stage pages in different tabs and keep them open, so you can see/copy previous calls.

Navigate to [https://localhost:4000/](https://localhost:4000/).
The page will show an introduction and three links to the different stages of the scenario:
1. In the _organization_ tab you can create apps and tenants and assign an app to a tenant.
2. In the _tenant_ tab you can create a user and assign them to an app. You can also optionally create an authenticator for your external Idp.
3. In the _user_ tab you can upload and download a css document for the user.

At all stages you first need to login. To do this for the _organization_ stage you need to have a develper account. If you do not have one you can 
sign up for the _Free Trail_ at [https://developer.opentext.com/plans](https://developer.opentext.com/plans). 
During signup you will get the organization service account password and be able to download the public and confidential client details for the Organization.

**Organization page**

The organization service account password can be reset to something more memorable via the Organization tab within the developer Console (Click on the little (i) after '_Organization service account_' to get a menu containing the reset).

If you want to use the '_Authorization Code with PKCE_' on organization level then add a wildcard url of the
sample application to the public service client of the organization in console. This is the url printed out
during startup of the app, with `.*` appended (e.g. `https://localhost:4000/.*`).

Depending on the chosen authentication method, different data needs to be provided:
1. **Password grant**: The confidential service client id and secret and the organization service account username and password
2. **Authorization Code with PKCE**: The public service client id and an optional authenticator (explained below)

Initially it is recommended that you attempt all Organization and Tenant level actions using '_Password grant_' 
as this is by far the simplest and requires little specialist set-up.

Having provided the required values, press login. In case of '_Password grant_' the _token_ field will be updated when login 
was successful. In the case of '_Authorization Code with PKCE_' you will be redirected to the identity provider to provide 
your organization username and password and after login redirected back to the sample application. 

After login create an app and create a tenant with the _associatedEmail_ property being an email address under your control. 
This user will be made the tenant administrator of the newly created tenant.

The response of these calls will contain their '_id_'. Use the tenant id and app id to assign the app to the tenant.
Next if you want to use the '_Password grant_' for _tenant_ management operations then go to the console, 
select the tenant and reset its service account password (Click on the little (i) after '_Tenant service account_' 
to get a menu containing the reset).

The response of the app creation also contains the _serviceClients_ structure with two entries. 
These can be used for _tenant_ and _user_ stage login. The one with '_confidential_' set to _true_ is 
the private client for the application for which the _consumerKey_ and _consumerSecret_ is used to login to the 
_tenant_ with 'Password grant'.  For the application client with the '_confidential_' value set to _false_ only 
the _consumerKey_ is needed for the _Authorization Code with PKCE_ login. 

For PKCE flow the browser need to be redirected to the sample application, therefore its url need to be added to the app's 
redirect urls. The '_Set Redirects_' request must contain the wildcard url of the sample application, 
typically something like `https://localhost:4000/.*`. 

By default the correct url for the sample is already provided in the request body.
Take care: Ensure also the `<apiUrl>/.*` is still kept in the payload.

**Tenant page  using built-in OCP authenticator**

Select one of the login flows and provide the corresponding service client values on the
_tenant_ including the created tenant id and its service account password to login.
Take care: The _Client Id_ is the _consumerKey_ and _Client Secret_ is the _consumerSecret_ of the application.

On the _tenant_ page you will create users and assign them to applications. You will also create an authenticator for your user. 
The _tenant_ page supports two different flows through the page.

By default the _tenant_ page assumes that the user to be created is a native user of the platform and that the built-in 
OCP authenticator will be used to authenticate the user. Hence the user can be created first without an authenticator 
identified in the request body, meaning it will default to being authenticated via the built-in OCP authenticator.

If you want to use an external IdP as your authenticator for the user, then you create the external authenticator _first_ and 
then create the user with the required external authenticator identified within the request body (see below). 
If using the built-in OCP authenticator you do not need to perform the create authenticator action on the _tenant_ page.

When logged in on _tenant_ page, create the user and assign the user to the app using their id's. 

**User page using built-in OCP authenticator**

Normally, when accessing OCP services from the browser you will use the '_Authorization Code with PKCE_' option: 
1. Set the organization and tenant id
2. Select '_Authorize Code with PKCE_'
3. Set '_Client Id_' to the app's public client _consumerKey_
4. Leave '_Authenticator_' blank or set it the same as _tenantId_
5. Click on '_Login_'

If the flow is working correctly for the user you should see the platform login flow and be able to enter your user's email 
address followed by their password and then be redirected back to the User page with a valid access token.

Now you can press _Upload_ to upload an auto generated document. 
Use the '_id_' value of the response as '_Content Id_' to download this document. You will see in
it the timestamp of the upload to verify the upload went fine.

**Tenant page using an external authenticator**

In the _tenant_ page IdP specific authentication can also be created. You can use a public one e.g. _SSOCircle_. 
What is important is that it supports SAML. 

In the 'Create Authenticator' set the _providerName_ to a unique name and the _providerUrl_ to the IdP metadata url 
of the IdP you are integrating with. Retain the returned authenticator _id_ for use below.

After creating the authenticator its metadata needs be retrieved from the metadata endpoint 
e.g. with the command below (replace _apiUrl_, _tenantId_ and_authenticatorId_):

```shell
curl --raw "<apiUrl>/otdstenant/<tenantId>/login?authhandler=<authenticatorId>&SAMLMetadata"
```
Your IdP then needs to be configured with this metadata.

If using _SSOCircle_:
1. Register here: [https://idp.ssocircle.com/sso/UI/Login]()
2. Login using registration link from mail
3. Register metadata after login here: [https://idp.ssocircle.com/sso/hos/ManageSPMetadata.js]():
  * Select 'Add new Service Provider'
  * Enter the FQDN of the ServiceProvider => use the domain name of the _apiUrl_, e.g. '_na-1-dev.api.opentext.com_'
  * Attributes sent in assertion => check EmailAddress
  * Insert the SAML Metadata information of your SP => paste the raw data retrived with the above curl command

Now on the _tenant_ page adjust and submit the create user request:
1. Set the '_email_' field to the email address you registered for the user within the external IdP
2. Add a field '_authenticatorId_' with the id of the authenticator created before

**User page using an external authenticator**

Finally, go to the _user_ page to login with the user:
1. Set the organization and tenant id
2. Select '_Authorize Code with PKCE_'
3. Set '_Client Id_' to the app's public client _consumerKey_
4. Set '_Authenticator_' to the _authenticatorId_ corresponding to the external Idp
5. Click on '_Login_'

If the flow is working correctly for the user you should see the IdP's login flow and be able to enter your user's email 
address followed by their password and then be redirected back to the User page with a valid access token.

# Caveats
1. The website uses `window.crypto.subtle`. In some browsers it only works in the browser when the page 
   is accessed either on `localhost` or over a secure connection (HTTPS).
2. Ngrok creates outbound connections. Some security scanning software will block or even kill the process.

# Clean up
Use [https://developer.opentext.com/console]() to delete apps and tenants.
For delete of tenant authenticators and users navigate to _Admin Center_ from console by selecting the tenant 
and click 'Manage'.
