
import Web3 from '/Users/sahilgupta/Desktop/EthIndia2018/dappcon-workshop-dapp/.embark/versions/web3/1.0.0-beta.34/web3';

import web3 from 'Embark/web3';

var EmbarkJS = {
  onReady: function (cb) {
    if (typeof (__embarkContext) === 'undefined') {
      return cb();
    }
    return __embarkContext.execWhenReady(cb);
  }
};

EmbarkJS.isNewWeb3 = function (web3Obj) {
  var _web3 = web3Obj || (new Web3());
  if (typeof(_web3.version) === "string") {
    return true;
  }
  return parseInt(_web3.version.api.split('.')[0], 10) >= 1;
};

EmbarkJS.Contract = function (options) {
  var self = this;
  var i, abiElement;
  var ContractClass;

  this.abi = options.abi;
  this.address = options.address;
  this.gas = options.gas;
  this.code = '0x' + options.code;
  //this.web3 = options.web3 || web3;
  this.web3 = options.web3;
  if (!this.web3 && typeof (web3) !== 'undefined') {
    this.web3 = web3;
  } else if (!this.web3) {
    this.web3 = window.web3;
  }

  if (EmbarkJS.isNewWeb3(this.web3)) {
    ContractClass = new this.web3.eth.Contract(this.abi, this.address);
    ContractClass.setProvider(this.web3.currentProvider);
    ContractClass.options.data = this.code;
    ContractClass.options.from = this.from || this.web3.eth.defaultAccount;
    ContractClass.abi = ContractClass.options.abi;
    ContractClass.address = this.address;
    ContractClass.gas = this.gas;

    let originalMethods = Object.keys(ContractClass);

    ContractClass._jsonInterface.forEach((abi) => {
      if (originalMethods.indexOf(abi.name) >= 0) {
        console.log(abi.name + " is a reserved word and cannot be used as a contract method, property or event");
        return;
      }

      if (!abi.inputs) {
        return;
      }

      let numExpectedInputs = abi.inputs.length;

      if (abi.type === 'function' && abi.constant) {
        ContractClass[abi.name] = function () {
          let options = {}, cb = null, args = Array.from(arguments || []).slice(0, numExpectedInputs);
          if (typeof (arguments[numExpectedInputs]) === 'function') {
            cb = arguments[numExpectedInputs];
          } else if (typeof (arguments[numExpectedInputs]) === 'object') {
            options = arguments[numExpectedInputs];
            cb = arguments[numExpectedInputs + 1];
          }

          let ref = ContractClass.methods[abi.name];
          let call = ref.apply(ref, ...arguments).call;
          return call.apply(call, []);
        };
      } else if (abi.type === 'function') {
        ContractClass[abi.name] = function () {
          let options = {}, cb = null, args = Array.from(arguments || []).slice(0, numExpectedInputs);
          if (typeof (arguments[numExpectedInputs]) === 'function') {
            cb = arguments[numExpectedInputs];
          } else if (typeof (arguments[numExpectedInputs]) === 'object') {
            options = arguments[numExpectedInputs];
            cb = arguments[numExpectedInputs + 1];
          }

          let ref = ContractClass.methods[abi.name];
          let send = ref.apply(ref, args).send;
          return send.apply(send, [options, cb]);
        };
      } else if (abi.type === 'event') {
        ContractClass[abi.name] = function (options, cb) {
          let ref = ContractClass.events[abi.name];
          return ref.apply(ref, [options, cb]);
        };
      }
    });

    return ContractClass;
  } else {
    ContractClass = this.web3.eth.contract(this.abi);

    this.eventList = [];

    if (this.abi) {
      for (i = 0; i < this.abi.length; i++) {
        abiElement = this.abi[i];
        if (abiElement.type === 'event') {
          this.eventList.push(abiElement.name);
        }
      }
    }

    var messageEvents = function () {
      this.cb = function () {
      };
    };

    messageEvents.prototype.then = function (cb) {
      this.cb = cb;
    };

    messageEvents.prototype.error = function (err) {
      return err;
    };

    this._originalContractObject = ContractClass.at(this.address);
    this._methods = Object.getOwnPropertyNames(this._originalContractObject).filter(function (p) {
      // TODO: check for forbidden properties
      if (self.eventList.indexOf(p) >= 0) {

        self[p] = function () {
          var promise = new messageEvents();
          var args = Array.prototype.slice.call(arguments);
          args.push(function (err, result) {
            if (err) {
              promise.error(err);
            } else {
              promise.cb(result);
            }
          });

          self._originalContractObject[p].apply(self._originalContractObject[p], args);
          return promise;
        };
        return true;
      } else if (typeof self._originalContractObject[p] === 'function') {
        self[p] = function (_args) {
          var args = Array.prototype.slice.call(arguments);
          var fn = self._originalContractObject[p];
          var props = self.abi.find((x) => x.name == p);

          var promise = new Promise(function (resolve, reject) {
            args.push(function (err, transaction) {
              promise.tx = transaction;
              if (err) {
                return reject(err);
              }

              var getConfirmation = function () {
                self.web3.eth.getTransactionReceipt(transaction, function (err, receipt) {
                  if (err) {
                    return reject(err);
                  }

                  if (receipt !== null) {
                    return resolve(receipt);
                  }

                  setTimeout(getConfirmation, 1000);
                });
              };

              if (typeof transaction !== "string" || props.constant) {
                resolve(transaction);
              } else {
                getConfirmation();
              }
            });

            fn.apply(fn, args);
          });

          return promise;
        };
        return true;
      }
      return false;
    });
  }
};

EmbarkJS.Contract.prototype.deploy = function (args, _options) {
  var self = this;
  var contractParams;
  var options = _options || {};

  contractParams = args || [];

  contractParams.push({
    from: this.web3.eth.accounts[0],
    data: this.code,
    gas: options.gas || 800000
  });

  var contractObject = this.web3.eth.contract(this.abi);

  var promise = new Promise(function (resolve, reject) {
    contractParams.push(function (err, transaction) {
      if (err) {
        reject(err);
      } else if (transaction.address !== undefined) {
        resolve(new EmbarkJS.Contract({
          abi: self.abi,
          code: self.code,
          address: transaction.address
        }));
      }
    });

    // returns promise
    // deploys contract
    // wraps it around EmbarkJS.Contract
    contractObject["new"].apply(contractObject, contractParams);
  });


  return promise;
};

EmbarkJS.Contract.prototype.new = EmbarkJS.Contract.prototype.deploy;

EmbarkJS.Contract.prototype.at = function (address) {
  return new EmbarkJS.Contract({abi: this.abi, code: this.code, address: address});
};

EmbarkJS.Contract.prototype.send = function (value, unit, _options) {
  var options, wei;
  if (typeof unit === 'object') {
    options = unit;
    wei = value;
  } else {
    options = _options || {};
    wei = this.web3.toWei(value, unit);
  }

  options.to = this.address;
  options.value = wei;

  this.web3.eth.sendTransaction(options);
};

EmbarkJS.Storage = {};

EmbarkJS.Storage.Providers = {};

EmbarkJS.Storage.saveText = function (text) {
  if (!this.currentStorage) {
    throw new Error('Storage provider not set; e.g EmbarkJS.Storage.setProvider("ipfs")');
  }
  return this.currentStorage.saveText(text);
};

EmbarkJS.Storage.get = function (hash) {
  if (!this.currentStorage) {
    throw new Error('Storage provider not set; e.g EmbarkJS.Storage.setProvider("ipfs")');
  }
  return this.currentStorage.get(hash);
};

EmbarkJS.Storage.uploadFile = function (inputSelector) {
  if (!this.currentStorage) {
    throw new Error('Storage provider not set; e.g EmbarkJS.Storage.setProvider("ipfs")');
  }
  return this.currentStorage.uploadFile(inputSelector);
};

EmbarkJS.Storage.getUrl = function (hash) {
  if (!this.currentStorage) {
    throw new Error('Storage provider not set; e.g EmbarkJS.Storage.setProvider("ipfs")');
  }
  return this.currentStorage.getUrl(hash);
};

EmbarkJS.Storage.registerProvider = function (providerName, obj) {
  EmbarkJS.Storage.Providers[providerName] = obj;
};

EmbarkJS.Storage.setProvider = function (provider, options) {
  let providerObj = this.Providers[provider];

  if (!providerObj) {
    throw new Error('Unknown storage provider');
  }

  this.currentStorage = providerObj;

  return providerObj.setProvider(options);
};

EmbarkJS.Storage.isAvailable = function () {
  if (!this.currentStorage) {
    throw new Error('Storage provider not set; e.g EmbarkJS.Storage.setProvider("ipfs")');
  }
  return this.currentStorage.isAvailable();
};

EmbarkJS.Messages = {};

EmbarkJS.Messages.Providers = {};

EmbarkJS.Messages.registerProvider = function (providerName, obj) {
  EmbarkJS.Messages.Providers[providerName] = obj;
};

EmbarkJS.Messages.setProvider = function (provider, options) {
  let providerObj = this.Providers[provider];

  if (!providerObj) {
    throw new Error('Unknown messages provider');
  }

  this.currentMessages = providerObj;

  return providerObj.setProvider(options);
};

EmbarkJS.Messages.isAvailable = function () {
  return this.currentMessages.isAvailable();
};

EmbarkJS.Messages.sendMessage = function (options) {
  if (!this.currentMessages) {
    throw new Error('Messages provider not set; e.g EmbarkJS.Messages.setProvider("whisper")');
  }
  return this.currentMessages.sendMessage(options);
};

EmbarkJS.Messages.listenTo = function (options, callback) {
  if (!this.currentMessages) {
    throw new Error('Messages provider not set; e.g EmbarkJS.Messages.setProvider("whisper")');
  }
  return this.currentMessages.listenTo(options, callback);
};

EmbarkJS.Names = {};

EmbarkJS.Names.Providers = {};

EmbarkJS.Names.registerProvider = function (providerName, obj) {
  EmbarkJS.Names.Providers[providerName] = obj;
};

EmbarkJS.Names.setProvider = function (provider, options) {
  let providerObj = this.Providers[provider];

  if (!providerObj) {
    throw new Error('Unknown name system provider');
  }

  this.currentNameSystems = providerObj;

  return providerObj.setProvider(options);
};

// resolve resolves a name into an identifier of some kind
EmbarkJS.Names.resolve = function (name) {
  if (!this.currentNameSystems) {
    throw new Error('Name system provider not set; e.g EmbarkJS.Names.setProvider("ens")');
  }
  return this.currentNameSystems.resolve(name);
};

// the reverse of resolve, resolves using an identifier to get to a name
EmbarkJS.Names.lookup = function (identifier) {
  if (!this.currentNameSystems) {
    throw new Error('Name system provider not set; e.g EmbarkJS.Names.setProvider("ens")');
  }
  return this.currentNameSystems.lookup(identifier);
};

// To Implement

/*
// register a name
EmbarkJS.Names.register = function(name, options) {

}
*/

EmbarkJS.Utils = {
  fromAscii: function (str) {
    var _web3 = new Web3();
    return _web3.utils ? _web3.utils.fromAscii(str) : _web3.fromAscii(str);
  },
  toAscii: function (str) {
    var _web3 = new Web3();
    return _web3.utils.toAscii(str);
  }
};

export default EmbarkJS;



let __MessageEvents = function() {
  this.cb = function() {};
};

__MessageEvents.prototype.then = function(cb) {
  this.cb = cb;
};

__MessageEvents.prototype.error = function(err) {
  return err;
};

__MessageEvents.prototype.stop = function() {
  this.filter.stopWatching();
};


/*global EmbarkJS, Web3, __MessageEvents */

// for the whisper v5 and web3.js 1.0
let __embarkWhisperNewWeb3 = {};

__embarkWhisperNewWeb3.setProvider = function (options) {
  const self = this;
  let provider;
  if (options === undefined) {
    provider = "localhost:8546";
  } else {
    provider = options.server + ':' + options.port;
  }
  // TODO: take into account type
  self.web3 = new Web3(new Web3.providers.WebsocketProvider("ws://" + provider));
  self.getWhisperVersion(function (err, version) {
    if (err) {
      console.log("whisper not available");
    } else if (version >= 5) {
      self.web3.shh.newSymKey().then((id) => {
        self.symKeyID = id;
      });
      self.web3.shh.newKeyPair().then((id) => {
        self.sig = id;
      });
    } else {
      throw new Error("version of whisper not supported");
    }
    self.whisperVersion = self.web3.version.whisper;
  });
};

__embarkWhisperNewWeb3.sendMessage = function (options) {
  var topics, data, ttl, payload;
  topics = options.topic || options.topics;
  data = options.data || options.payload;
  ttl = options.ttl || 100;
  var powTime = options.powTime || 3;
  var powTarget = options.powTarget || 0.5;

  if (topics === undefined) {
    throw new Error("missing option: topic");
  }

  if (data === undefined) {
    throw new Error("missing option: data");
  }

  topics = this.web3.utils.toHex(topics).slice(0, 10);

  payload = JSON.stringify(data);

  let message = {
    symKeyID: this.symKeyID, // encrypts using the sym key ID
    sig: this.sig, // signs the message using the keyPair ID
    ttl: ttl,
    topic: topics,
    payload: EmbarkJS.Utils.fromAscii(payload),
    powTime: powTime,
    powTarget: powTarget
  };

  this.web3.shh.post(message, function () {
  });
};

__embarkWhisperNewWeb3.listenTo = function (options, callback) {
  var topics = options.topic || options.topics;

  let promise = new __MessageEvents();

  if (typeof topics === 'string') {
    topics = [this.web3.utils.toHex(topics).slice(0, 10)];
  } else {
    topics = topics.map((t) => this.web3.utils.toHex(t).slice(0, 10));
  }

  let filter = this.web3.shh.subscribe("messages", {
    symKeyID: this.symKeyID,
    topics: topics
  }).on('data', function (result) {
    var payload = JSON.parse(EmbarkJS.Utils.toAscii(result.payload));
    var data;
    data = {
      topic: EmbarkJS.Utils.toAscii(result.topic),
      data: payload,
      //from: result.from,
      time: result.timestamp
    };

    if (callback) {
      return callback(null, data);
    }
    promise.cb(payload, data, result);
  });

  promise.filter = filter;

  return promise;
};

__embarkWhisperNewWeb3.getWhisperVersion = function (cb) {
  this.web3.shh.getVersion(function (err, version) {
    cb(err, version);
  });
};

__embarkWhisperNewWeb3.isAvailable = function () {
  return new Promise((resolve, reject) => {
    if (!this.web3.shh) {
      return resolve(false);
    }
    try {
      this.getWhisperVersion((err) => {
        resolve(Boolean(!err));
      });
    }
    catch (err) {
      reject(err);
    }
  });
};


EmbarkJS.Messages.registerProvider('whisper', __embarkWhisperNewWeb3);
import IpfsApi from 'ipfs-api';

let __embarkIPFS = {};

__embarkIPFS.setProvider = function (options) {
  var self = this;
  var promise = new Promise(function (resolve, reject) {
    try {
      if (options === undefined) {
        self._config = options;
        self._ipfsConnection = IpfsApi('localhost', '5001');
        self._getUrl = "http://localhost:8080/ipfs/";
      } else {
        var ipfsOptions = {host: options.host || options.server, protocol: 'http'};
        if (options.protocol) {
          ipfsOptions.protocol = options.protocol;
        }
        if (options.port && options.port !== 'false') {
          ipfsOptions.port = options.port;
        }
        self._ipfsConnection = IpfsApi(ipfsOptions);
        self._getUrl = options.getUrl || "http://localhost:8080/ipfs/";
      }
      resolve(self);
    } catch (err) {
      console.error(err);
      self._ipfsConnection = null;
      reject(new Error('Failed to connect to IPFS'));
    }
  });
  return promise;
};

__embarkIPFS.saveText = function (text) {
  const self = this;
  var promise = new Promise(function (resolve, reject) {
    if (!self._ipfsConnection) {
      var connectionError = new Error('No IPFS connection. Please ensure to call Embark.Storage.setProvider()');
      reject(connectionError);
    }
    self._ipfsConnection.add(self._ipfsConnection.Buffer.from(text), function (err, result) {
      if (err) {
        reject(err);
      } else {
        resolve(result[0].path);
      }
    });
  });

  return promise;
};

__embarkIPFS.get = function (hash) {
  const self = this;
  // TODO: detect type, then convert if needed
  //var ipfsHash = web3.toAscii(hash);
  var promise = new Promise(function (resolve, reject) {
    if (!self._ipfsConnection) {
      var connectionError = new Error('No IPFS connection. Please ensure to call Embark.Storage.setProvider()');
      reject(connectionError);
    }
    self._ipfsConnection.get(hash, function (err, files) {
      if (err) {
        return reject(err);
      }
      resolve(files[0].content.toString());
    });
  });

  return promise;
};

__embarkIPFS.uploadFile = function (inputSelector) {
  const self = this;
  var file = inputSelector[0].files[0];

  if (file === undefined) {
    throw new Error('no file found');
  }

  var promise = new Promise(function (resolve, reject) {
    if (!self._ipfsConnection) {
      var connectionError = new Error('No IPFS connection. Please ensure to call Embark.Storage.setProvider()');
      reject(connectionError);
    }
    var reader = new FileReader();
    reader.onloadend = function () {
      var fileContent = reader.result;
      var buffer = self._ipfsConnection.Buffer.from(fileContent);
      self._ipfsConnection.add(buffer, function (err, result) {
        if (err) {
          reject(err);
        } else {
          resolve(result[0].path);
        }
      });
    };
    reader.readAsArrayBuffer(file);
  });

  return promise;
};

__embarkIPFS.isAvailable = function () {
  return new Promise((resolve) => {
    if (!this._ipfsConnection) {
      return resolve(false);
    }
    this._ipfsConnection.id()
      .then((id) => {
        resolve(Boolean(id));
      })
      .catch(() => {
        resolve(false);
      });
  });
};

__embarkIPFS.getUrl = function (hash) {
  return (this._getUrl || "http://localhost:8080/ipfs/") + hash;
};



EmbarkJS.Storage.registerProvider('ipfs', __embarkIPFS);

/* global EmbarkJS */

import {findSeries} from 'p-iteration';

let __embarkStorage = {};

__embarkStorage.setProviders = async function (dappConnOptions) {
    try {
      let workingConnFound = await findSeries(dappConnOptions, async (dappConn) => {
        if(dappConn === '$BZZ' || dappConn.provider === 'swarm'){
          let options = dappConn;
          if(dappConn === '$BZZ') options = {"useOnlyGivenProvider": true};
          try{
            await EmbarkJS.Storage.setProvider('swarm', options);
            let isAvailable = await EmbarkJS.Storage.isAvailable();
            return isAvailable;
          }catch(err){
            return false; // catch errors for when bzz object not initialised but config has requested it to be used
          }
        }
        else if(dappConn.provider === 'ipfs') {
          // set the provider then check the connection, if true, use that provider, else, check next provider
          try{
            await EmbarkJS.Storage.setProvider('ipfs', dappConn);
            let isAvailable =  await EmbarkJS.Storage.isAvailable();
            return isAvailable;
          } catch(err) {
            return false;
          }
        }
      });
      if(!workingConnFound) throw new Error('Could not connect to a storage provider using any of the dappConnections in the storage config');
    } catch (err) {
      throw new Error('Failed to connect to a storage provider: ' + err.message);
    }
  };

import namehash from 'eth-ens-namehash';

/*global web3, EmbarkJS*/
let __embarkENS = {};

// registry interface for later
__embarkENS.registryInterface = [
  {
    "constant": true,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      }
    ],
    "name": "resolver",
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "type": "function"
  },
  {
    "constant": true,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      }
    ],
    "name": "owner",
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      },
      {
        "name": "resolver",
        "type": "address"
      }
    ],
    "name": "setResolver",
    "outputs": [],
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      },
      {
        "name": "label",
        "type": "bytes32"
      },
      {
        "name": "owner",
        "type": "address"
      }
    ],
    "name": "setSubnodeOwner",
    "outputs": [],
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      },
      {
        "name": "owner",
        "type": "address"
      }
    ],
    "name": "setOwner",
    "outputs": [],
    "type": "function"
  }
];

__embarkENS.resolverInterface = [
  {
    "constant": true,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      }
    ],
    "name": "addr",
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "type": "function"
  },
  {
    "constant": true,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      }
    ],
    "name": "content",
    "outputs": [
      {
        "name": "",
        "type": "bytes32"
      }
    ],
    "type": "function"
  },
  {
    "constant": true,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      }
    ],
    "name": "name",
    "outputs": [
      {
        "name": "",
        "type": "string"
      }
    ],
    "type": "function"
  },
  {
    "constant": true,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      },
      {
        "name": "kind",
        "type": "bytes32"
      }
    ],
    "name": "has",
    "outputs": [
      {
        "name": "",
        "type": "bool"
      }
    ],
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      },
      {
        "name": "addr",
        "type": "address"
      }
    ],
    "name": "setAddr",
    "outputs": [],
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      },
      {
        "name": "hash",
        "type": "bytes32"
      }
    ],
    "name": "setContent",
    "outputs": [],
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      },
      {
        "name": "name",
        "type": "string"
      }
    ],
    "name": "setName",
    "outputs": [],
    "type": "function"
  },
  {
    "constant": true,
    "inputs": [
      {
        "name": "node",
        "type": "bytes32"
      },
      {
        "name": "contentType",
        "type": "uint256"
      }
    ],
    "name": "ABI",
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      },
      {
        "name": "",
        "type": "bytes"
      }
    ],
    "payable": false,
    "type": "function"
  }
];

__embarkENS.registryAddresses = {
  // Mainnet
  "1": "0x314159265dd8dbb310642f98f50c066173c1259b",
  // Ropsten
  "3": "0x112234455c3a32fd11230c42e7bccd4a84e02010",
  // Rinkeby
  "4": "0xe7410170f87102DF0055eB195163A03B7F2Bff4A"
};

__embarkENS.setProvider = function () {
  const self = this;
  // get network id and then assign ENS contract based on that 
  let registryAddresses = this.registryAddresses;
  this.ens = null;
  web3.eth.net.getId().then(id => {
    if (registryAddresses[id] !== undefined) {
      EmbarkJS.onReady(() => {
        self.ens = new EmbarkJS.Contract({abi: self.registryInterface, address: registryAddresses[id]});
      });
    }
    // todo: deploy at this point
  }).catch(e => {
    if (e.message.indexOf('Provider not set or invalid') > -1) {
      console.warn('ENS is not available in this chain');
      return;
    }
    console.error(e);
  });
};

__embarkENS.resolve = function(name) {
  const self = this;

  if (self.ens === undefined) return;

  let node = namehash.hash(name);
  
  return self.ens.methods.resolver(node).call().then((resolverAddress) => {
    let resolverContract = new EmbarkJS.Contract({abi: self.resolverInterface, address: resolverAddress});
    return resolverContract.methods.addr(node).call();
  }).then((addr) => {
    return addr;
  }).catch(err => err);
};

__embarkENS.lookup = function(address) {
  const self = this;

  if (self.ens === undefined) return;

  if (address.startsWith("0x")) address = address.slice(2);

  let node = namehash.hash(address.toLowerCase() + ".addr.reverse");

  return self.ens.methods.resolver(node).call().then((resolverAddress) => {
    let resolverContract = new EmbarkJS.Contract({abi: self.resolverInterface, address: resolverAddress});
    return resolverContract.methods.name(node).call();
  }).then((name) => {
    if (name === "" || name === undefined) throw Error("ENS name not found");
    return name;
  }).catch(err => err);
};

EmbarkJS.Names.registerProvider('ens', __embarkENS);
var whenEnvIsLoaded = function(cb) {
  if (typeof document !== 'undefined' && document !== null && !/comp|inter|loaded/.test(document.readyState)) {
      document.addEventListener('DOMContentLoaded', cb);
  } else {
    cb();
  }
}
whenEnvIsLoaded(function() {
  
EmbarkJS.Messages.setProvider('whisper',{"server":"localhost","port":8546,"type":"ws"});
});

var whenEnvIsLoaded = function(cb) {
  if (typeof document !== 'undefined' && document !== null && !/comp|inter|loaded/.test(document.readyState)) {
      document.addEventListener('DOMContentLoaded', cb);
  } else {
    cb();
  }
}
whenEnvIsLoaded(function() {
  
__embarkStorage.setProviders([{"provider":"ipfs","host":"localhost","port":5001,"getUrl":"http://localhost:8080/ipfs/"}]);
});

var whenEnvIsLoaded = function(cb) {
  if (typeof document !== 'undefined' && document !== null && !/comp|inter|loaded/.test(document.readyState)) {
      document.addEventListener('DOMContentLoaded', cb);
  } else {
    cb();
  }
}
whenEnvIsLoaded(function() {
  
EmbarkJS.Names.setProvider('ens',{});
});
