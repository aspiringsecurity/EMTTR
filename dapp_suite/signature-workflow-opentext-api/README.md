# Signature workflow in Medical Documents using Open Text API

We are building a docusign type workflow for medical documents powered by Vue.js, flask, Open Text API. We are extending the core signature demo workflow solution.


## Project Setup for the Frontend
Setting up this application is simple - once cloned, [install NodeJS](https://nodejs.org/en/download/package-manager/) if you haven't already then simply do the following:

1. Add `signature.demo` to the `127.0.0.1` entry in your hosts file.
2. Run `npm install` to install dependencies
3. Run `npm run serve` to start the application

That's it! This will install necessary dependencies and compile the application with hot-reloading. The application will be available at `signature.demo:8080` in your browser.

In addition, you are able to:
* Compile and minify for production using `npm run build`
* Run the linter using `npm run lint`

## Project Setup for the Backend
To run the facade server you need to have Python 3 version greater than 3.4 installed. This will automamically have pip installed. If you don't have pip already installed, follow instructions [here](https://pip.pypa.io/en/stable/installation/) (for MacOS and Windows users).
You'll want to run a second terminal window for the backend.
Install pipenv using `pip install pipenv` for Windows users or `brew install pipenv` for Mac user using Homebrew. More information on pipenv installation can be found [here](https://pypi.org/project/pipenv/).
Install other dependencies from the Pipfile using `pipenv install`.

The following env var information has been added for convenience to the .env file in the root:
```bash
FLASK_APP=app_server.py
FLASK_ENV=development
FLASK_RUN_HOST=signature.demo
API_SERVER_HOST=https://sign.core.opentext.com/
```

Run `pipenv shell`
To start the server run the following command from the root of the project while in the shell `make server`

## Example JSON credential file format:
```json
{
  "site": "your_site",
  "clientID": "your_oauth_client_id",
  "clientSecret": "your_oauth_client_secret",
  "subscription": "subscription:coresignature-signingsite-123213",
  "username": "your_email@test.com",
  "password": "your_password",
  "authHost": "https://otdsauth.ot2.opentext.com",
  "apiHost": " http://signature.demo:5000/,
}
```


