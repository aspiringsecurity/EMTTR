const fs = require('fs');
const http = require('http');
const https = require('https');
const path = require('path');
const replace = require('stream-replace');
const XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;
const invokeAsync = require('./xhr-utils').invokeAsync

const readline = require("readline");
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

(async function () {

  const hostname = await getProperty('hostname', 'localhost');
  const port = 4000;
  const enableHttps = await getProperty('https', true) === true;
  const serverUrl = (enableHttps ? 'https' : 'http') + '://' + hostname + ':'
      + port;
  const loginContent = fs.readFileSync('./src/login.html');

  function writeResponse(res, filename, contentType = 'text/html') {
    res.writeHead(200, {'Content-Type': contentType});
    let stream = fs.createReadStream(path.join('src', filename));
    if (contentType === 'text/html') {
      stream = stream.pipe(replace(/LOGIN_CONTENT/g, loginContent));
    }
    stream.pipe(res);
  }

  // Generate new key pair with command like this:
  // openssl req -x509 -new -newkey rsa:2048 -nodes -subj '/C=US/ST=State/L=City/O=Example/CN=Test' -keyout private-key.pem -out public-cert.pem -days 73000
  const options = {
    key: fs.readFileSync('private-key.pem'),
    cert: fs.readFileSync('public-cert.pem')
  };

  function getProperties() {
    return JSON.parse(fs.readFileSync('properties.json'));
  }

  async function getProperty(name, defaultValue) {
    if(process.env['CHROME_OPTION'] === '--headless') {
      return name === 'hostname' ? defaultValue : true;
    }
    const value = getProperties()[name];
    if (value) {
      return value;
    }
    const answer = await new Promise(resolve => {
      rl.question(
          "Use " + name + " with value '" + defaultValue
          + "'? (Press enter to use, else provide other value)\n",
          resolve)
    })
    return answer ? answer : defaultValue;
  }

  const server = (enableHttps ? https : http).createServer(options,
      (req, res) => {
        const baseURL = req.protocol + '://' + req.headers.host + '/';
        const urlInfo = new URL(req.url, baseURL);

        function writeConfiguration() {
          res.writeHead(200, {'Content-Type': 'application/json'});
          let configuration = {};
          if (fs.existsSync('properties.json')) {
            configuration['properties'] = getProperties();
          }
          res.write(JSON.stringify(configuration));
          res.end();
        }

        function writeProxy() {
          res.writeHead(200, {'Content-Type': 'application/json'});
          let buffer = '';
          req.on('data', (chunk => buffer += chunk));
          req.on('end', () => {
            const message = JSON.parse(buffer);
            const xhr = new XMLHttpRequest();
            const apiUrl = message.apiUrl;

            // Protect against open redirects
            if (!apiUrl.equals(getProperties().apiUrl) && !apiUrl.endsWith(
                '.opentext.com')) {
              throw new Error(
                  "Api url does not end with opentext.com: " + apiUrl);
            }

            xhr.open(message.method, apiUrl + message.path, true);
            xhr.setRequestHeader('Accept', 'application/json');
            if (message.contentType) {
              xhr.setRequestHeader('Content-Type', message.contentType);
            }
            xhr.setRequestHeader('Authorization',
                'Bearer ' + message.accessToken);
            const respond = () => {
              res.writeHead(xhr.status);
              res.write(xhr.responseText);
              res.end();
            };
            console.debug(
                'Forwarding ' + message.method + ' ' + message.path + ' to '
                + apiUrl);
            invokeAsync(xhr, message.body).then(respond, respond)
          });
        }

        switch (urlInfo.pathname) {
          case '/auth.js':
          case '/auth-utils.js':
          case '/shared-ui.js':
          case '/ui-utils.js':
          case '/xhr-utils.js':
            writeResponse(res, urlInfo.pathname.substr(1),
                'application/javascript');
            return;
          case '/style.css':
            writeResponse(res, urlInfo.pathname.substr(1), 'text/css');
            return;
          case '/':
            res.writeHead(307, {'Location': 'index.html'});
            res.end();
            return;
          case '/index.html':
          case '/organization.html':
          case '/tenant.html':
          case '/user.html':
            writeResponse(res, urlInfo.pathname.substr(1));
            return
          case '/configuration.json':
            writeConfiguration();
            return;
          case '/proxy-call':
            writeProxy();
            return;
          default:
            if (urlInfo.pathname.startsWith('/fonts/')) {
              writeResponse(res, urlInfo.pathname.substr(1), 'font/woff2');
              return;
            }
            res.writeHead(404).end();
            return;
        }
      });

  async function start() {

    server.listen(port);

    await new Promise(resolve => setTimeout(resolve, 2000));
    console.log('Sample Application Running, you can now connect to: ' + serverUrl + '/\n');
  }

  await start();
})();
