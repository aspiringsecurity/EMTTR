import os

import requests
from flask import (Flask, Response, request,)

app = Flask(__name__)


def _proxy(*args, **kwargs):
    resp = requests.request(
        method=request.method,
        url=request.url.replace(request.host_url, os.getenv('API_SERVER_HOST')),
        headers={key: value for (key, value) in request.headers if key != 'Host'},
        data=request.get_data(),
        cookies=request.cookies,
        allow_redirects=False)

    excluded_headers = ['content-encoding', 'content-length', 'transfer-encoding', 'connection']
    headers = [(name, value) for (name, value) in resp.raw.headers.items()
               if name.lower() not in excluded_headers]

    response = Response(resp.content, resp.status_code, headers)
    response.headers.add('Access-Control-Allow-Origin', '*')
    return response


@app.after_request
def after_request(response):
    response.headers.add('Access-Control-Allow-Headers', 'Content-Type,Authorization')
    response.headers.add('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE,OPTIONS,HEAD')
    response.headers['Access-Control-Allow-Origin'] = '*'
    return response


@app.route('/api/v1/documents/', methods=['POST'])
@app.route('/api/v1/signature-requests/', methods=['POST'])
@app.route('/api/v1/events/', methods=['GET'])
def proxy():
    return _proxy()
