Contract Approval Workflow Application for CROS and pharma organizations
--------

We are developing a contract approval application for CROS and pharma organizations using OpenText Cloud Platform. We are consuming IM services from the OpenText Cloud Platform and extending the demo example.  


## Install Visual Studio Code

Install VS Code from the following URL: https://code.visualstudio.com/download

## Install the VS Code Extension Pack

From the extensions View in VS Code, install the `OpenText Cloud Developer Tools - Extension Pack`.


## Configure Organization Profile

To allow deploying the application to your developer organization in the OpenText Cloud Platform, add an organization profile as described in the user guide. The user guide is available under `HELP AND FEEDBACK` in the OpenText Cloud Developer Tools view (OT icon in the VS Code Activity Bar). 

## Deployment

Deploy the application project (cf. user guide).

## Credentials

The API client credentials need to be configured from within the application code. Please replace the placeholders for the tenant id and client public id in the `.env` file with the tenant id and the client public id for the deployed application project (returned when deploying the project to the OpenText Cloud Platform).
  
## Download and install Node.js

Download and install Node.js (with npm) from https://nodejs.org/en/download

## Running the application

To run the application using Node.js follow these steps.

Open a terminal in VS Code and run the following commands (from the project root).

```
npm install
npm start
```

The application will be automatically opened in the browser at http://localhost:4000
