function httpGet(url, contentType = 'application/json') {
  const xhr = new XMLHttpRequest();
  xhr.open('GET', url, true);
  xhr.setRequestHeader('Accept', contentType);
  return invokeAsync(xhr).then(() => JSON.parse(xhr.responseText));
}

function invokeAsync(xhr, body = null) {
  return new Promise(function (resolve, reject) {
    xhr.onreadystatechange = function () {
      if (xhr.readyState === 4) {
        if (xhr.status >= 300) {
          reject('Error: status code = ' + xhr.status + '\n' + xhr.responseText)
        } else {
          resolve(xhr.responseText);
        }
      }
    }
    xhr.send(body);
  });
}

function prettyPrintJsonResponse(xhr) {
  return xhr.responseText && xhr.status < 300 ? JSON.stringify(JSON.parse(xhr.responseText), null, 2) : xhr.responseText;
}

// Expose some APIs for reuse in NodeJS
if (typeof module !== 'undefined') {
  module.exports = {
    invokeAsync: invokeAsync
  };
}
