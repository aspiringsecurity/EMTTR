
var Web3 = require('web3');
var fs = require('fs');
var Q = require('q');
var moment = require('moment');
var ipfs = require('ipfs');

var ipfsNode = new ipfs();

var web3 = new Web3(new Web3.providers.HttpProvider('http://localhost:8547'));

var regulatorBin = fs.readFileSync('./Regulator.bin', 'utf8');
var regulatorAbi = require('./Regulator-abi.json');

var trialBin = fs.readFileSync('./ClinicalTrial.bin', 'utf8');
var trialAbi = require('./ClinicalTrial-abi.json');

var defaultGas = 4700000;

var regulatorAccount = '0x7125e93d19079c4174473651681a072311000415';
var croAccount = '0x0ed09b0147820eda3df5c736330a86a1d5378d18';

var regulatorContractAddress = '0x5396f993858ced55ff16e548f855f5620f312bc9';
var regulatorContract = web3.eth.contract(regulatorAbi);

var trialContractAddress = '0x6aeade68950e94107bad70a3478de44987bf638b';
var trialContract = web3.eth.contract(trialAbi);

var trialStartDate = 1475522433;
var trialEndDate   = 1475608833;

var trialProtocolDocument = "TrialProtocol.pdf";

var fixedSubjectId = 's0/1985-04-03';
var subjects = 500;
var dataPoints = 5;
var sideEffect = ['NONE','NAUSEA','VOMITTING','HEADACHE','HEARTBURN','COMA'];

console.log("Connecting to node");
console.log("===================================");

web3.eth.getBlockNumber(function(error, number){
   console.log("blockNumber= "+number);
});

function hex2string(hex) {
   hex = hex.slice(2);
   const str = new Buffer(hex, 'hex').toString();
   var ret= "";
   for(var i=0;i<str.length;i++) {
      if(str[i] == String.fromCharCode(0)) { break; }
      ret += str[i];
   }
   return ret;
}

function deployRegulator() {
   var regulator = regulatorContract.new( { from: regulatorAccount, data: regulatorBin, gas: defaultGas }, function (error1, contract1){
      if (error1) {
         console.error("could not mine regulator contract")
         process.exit(0);
      }
      if(contract1.address === undefined) {return;}
      console.log('Regulator contract mined! address: ' + contract1.address + ' transactionHash: ' + contract1.transactionHash);
   });
}

function addCroForApproval() {
   var regulator = regulatorContract.at(regulatorContractAddress);
   regulator.submitCro.sendTransaction("roche", "http://www.roche.com",
      {from: croAccount, gas: defaultGas},
      function (error1, txHash) {
         if(error1) {
            console.error("error submitting CRO for approval", error1);
            process.exit(1);
         }
         console.log("submitting CRO for approval",txHash);
      }
   )
}

function approveCroWithId0() {
   var regulator = regulatorContract.at(regulatorContractAddress);
   regulator.changeCroStatus.sendTransaction(croAccount, 1,
      {from: regulatorAccount, gas: defaultGas},
      function (error1, txHash) {
         if(error1) {
            console.error("error approving CRO with Id 0", error1);
            process.exit(1);
         }
         console.log("approving CRO with ID 0, "+txHash)
      }
   )
}

function uploadTrialProtocol(_callback) {
   var trialProtocolContent = fs.readFileSync("../data/" + trialProtocolDocument);
   ipfsNode.files.add({ path: trialProtocolDocument, content: trialProtocolContent }, function(err, result) {
      if (err) {
         console.error("error uploading Trial Protocol to IPFS", err);
         return _callback(err);
      }
      console.log("uploaded Trial Protocol successfully. Hash: " + result[0].hash);
      return _callback(null, result[0].hash);
   });
}

function deployClinicalTrial() {
   uploadTrialProtocol(function(err, hash) {
      if (err) {
         process.exit(1);
      }
      var trialContract = web3.eth.contract(trialAbi);
      var trial = trialContract.new(regulatorContractAddress, croAccount, 0, trialStartDate, trialEndDate, "Tamiflu", hash,
         { from: croAccount, data: trialBin, gas: defaultGas }, function (error1, contract1){
         if (error1) {
            console.error("could not mine clinical trial contract");
            process.exit(0);
         }
         if(contract1.address === undefined) {return;}

         console.log('Clinical trial contract mined! address: ' + contract1.address + ' transactionHash: ' + contract1.transactionHash);
      });
   });
}

function getRandomInt(min, max) {
   return (Math.floor(Math.random() * (max - min + 1)) + min).toString();
}

function addSubjects() {
   var trialContract = web3.eth.contract(trialAbi);
   var trial = trialContract.at(trialContractAddress);

   function addSubjectTransaction(sub) {
      var deferred = Q.defer();
      trial.addSubject.sendTransaction(sub,
      { from: croAccount, gas: defaultGas }, function (error1, txHash){
         if (error1) {
            console.error("could not add subject", error1);
            process.exit(0);
         }
         console.log('added subject', sub, "tx= ", txHash);
         deferred.resolve();
      });
      return deferred.promise;
   }

   console.log("adding subjects to clinical trial");

   addSubjectTransaction(fixedSubjectId);

   var subjectsRange = [];
   for (i = 1; i < subjects+1; i++) {
      subjectsRange.push(i);
   }

   subjectsRange.reduce(function(previousValue, currentValue){
      return previousValue.then(function() {
         var data = {};
         data['name'] = 's'+currentValue.toString();
         data['dob'] = getRandomInt(1960,2010)+'-'+getRandomInt(1,12)+'-'+getRandomInt(1,30);
         var sub = data['name']+'/'+data['dob'];
         return addSubjectTransaction(sub);
      })
   }, Q.resolve('start'));
}

function getSubjectById() {
   var trialContract = web3.eth.contract(trialAbi);
   var trial = trialContract.at(trialContractAddress);

   trial.getSubjectById(0, function (err, _subjId) {
      if(err) {
         console.log(err);
         process.exit(1);
      }
      console.log('getSubjectById should be: '+fixedSubjectId+" retrieved: "+hex2string(_subjId));
   })
}

function addDataPoints() {
   var trialContract = web3.eth.contract(trialAbi);
   var trial = trialContract.at(trialContractAddress);

   function addDataTransaction(_subjId, _json) {
      var deferred = Q.defer();
      trial.addDataPoint.sendTransaction(_subjId, _json,
      { from: croAccount, gas: defaultGas }, function (error1, txHash){
         if (error1) {
            console.error("could not add data point", error1);
            process.exit(0);
         }
         console.log('\t','added data point; subj ',_subjId, "data",_json, "tx:", txHash);
         deferred.resolve();
      });
      return deferred.promise;
   }

   var subjectsRange = [];
   for (i = 1; i < subjects+1; i++) {
      subjectsRange.push(i);
   }

   subjectsRange.reduce(function(previousValue, currentValue){
      return previousValue.then(function() {
         console.log("Adding data for subject number", currentValue);
         var promises = []
         for (j = 0; j < dataPoints; j++) {
            var data = {};
            data['dose'] = Math.floor((Math.random() * 100) + 1);
            data['units'] = 'mg';
            data['response'] = Math.floor((Math.random() * 100) + 1);
            data['side-effects'] = sideEffect[getRandomInt(0,5)];
            var _json = data['dose']+'/'+data['units']+'/'+data['response']+'/'+data['side-effects'];
            promises.push(addDataTransaction(currentValue, _json));
         }
         return Q.all(promises);
      })
   }, Q.resolve('start'));
}

function readFromRegulator() {
   var regulator = regulatorContract.at(regulatorContractAddress);
   regulator.getCroById(0, function (error1, data) {
      data[0] = hex2string(data[0]);
      data[1] = hex2string(data[1]);
      console.log("CRO with ID: 0; /name=",data[0]," /url=", data[1], " /address=", data[2], " /STATUS_ACCEPTED=", data[3]=="1");
   })
}

function readFromTrial() {
   var trial = trialContract.at(trialContractAddress);

   trial.drugName(function(error, _drugName){
      console.log("drug name= "+hex2string(_drugName));
   })

   trial.ipfsHash(function(err, _ipfsHash) {
      console.log("ipfs hash= "+hex2string(_ipfsHash));
   })

   function readDataPoint(_patientIdx, _dataIdx) {
      var deferred = Q.defer();
      trial.getDataPointForSubject(_patientIdx, _dataIdx, function (error, data) {
         data[1] = hex2string(data[1]);
         console.log("\t", "data= ", data[1], "added at", moment(parseInt(data[0])*1000).format());
         deferred.resolve();
      })
      return deferred.promise;
   }

   function readDataForPatient(_patientIdx) {
      var deferred = Q.defer();
      trial.getSubjectById(_patientIdx, function(error, _patient){
         console.log("patient ident= "+hex2string(_patient));

         var promises = [];
         trial.getDataCounterForSubject(_patientIdx, function (error, _counterForPatient) {
            for(var i=0; i<parseInt(_counterForPatient); i++) {
               promises.push(readDataPoint(_patientIdx, i));
            }
            Q.all(promises).then(function () {
               deferred.resolve();
            })
         })
      })

      return deferred.promise;
   }

   trial.getSubjectsCount(function(error, _count) {
      console.log("number of subjects= "+_count);

      var range = [];
      for(var i=0; i<_count;i++) {
         range.push(i);
      }

      range.reduce(function(previousValue, currentValue){
         return previousValue.then(function() {
            return readDataForPatient(currentValue);
         })
      }, Q.resolve('start'));
   })

}

function main() {
   if(process.argv.length < 3) {
      console.error("Please specify action to run");
      process.exit(1);
   }

   var cmd = process.argv[2];

   if(cmd === 'deployRegulator') {
      deployRegulator();
   } else
   if(cmd === 'addCroForApproval') {
      addCroForApproval();
   } else
   if(cmd === 'approveCroWithId0') {
      approveCroWithId0();
   } else
   if(cmd === 'deployClinicalTrial') {
      deployClinicalTrial();
   } else
   if(cmd === 'addSubjects') {
      addSubjects();
   } else
   if(cmd === 'getSubjectById'){
      getSubjectById();
   } else
   if(cmd === 'addDataPoints'){
      addDataPoints();
   } else
   if(cmd === 'readFromRegulator'){
      readFromRegulator();
   } else
   if(cmd === 'readFromTrial'){
      readFromTrial();
   }
}

main();
