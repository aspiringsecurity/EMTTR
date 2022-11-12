/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */

 // Import the page's CSS. Webpack will know what to do with it.
import '../styles/app.css'

// Import libraries we need.
import { default as Web3 } from 'web3'
import { default as contract } from 'truffle-contract'
import {sha256} from 'js-sha256';
import {default as EthCrypto} from 'eth-crypto';
import {default as ecies} from 'eth-ecies';

// Import our contract artifacts and turn them into usable abstractions.
import DrugTaskArtifact from '../../build/contracts/DrugTask.json'
import DrugEcosystemArtifact from '../../build/contracts/DrugEcosystem.json'

// drugTask is our usable abstraction, which we'll use through the code below.
const drugTask = contract(DrugTaskArtifact)
const drugEcosystem = contract(DrugEcosystemArtifact)

// The following code is simple to show off interacting with your contracts.
// As your needs grow you will likely need to change its form and structure.
// For application bootstrapping, check out window.addEventListener below.
let accounts
let account
let taskAddress = 0x0
let isTaskOwner

// fileReader for legitimation and data access management
let fileReader

const App = {
  start: function () {
    const self = this
    // Bootstrap the DrugTask abstraction for Use.
    drugTask.setProvider(web3.currentProvider)
    drugEcosystem.setProvider(web3.currentProvider)

    // Get the initial account balance so it can be displayed.
    web3.eth.getAccounts(function (err, accs) {
      if (err != null) {
        alert('There was an error fetching your accounts.')
        return
      }

      if (accs.length === 0) {
        alert("Couldn't get any accounts! Make sure your Ethereum client is configured correctly.")
        return
      }

      accounts = accs
      account = accounts[0]

      self.checkEcosystemStatus()
      self.listTaskPool()
    })
  },

  setStatus: function (message) {
    const status = document.getElementById('status')
    status.innerHTML = message
  },

  checkEcosystemStatus: function() {
    const self = this
    document.getElementById('DrugTaskPanel').style.display = 'none'

    let drugEcoInstance
    drugEcosystem.deployed().then(function (instance) {
      drugEcoInstance = instance
      return drugEcoInstance.getTaskCount()
    }).then(function (count) {
      if (0 == count.valueOf()) {
        document.getElementById('selectTask').disabled = true
      }
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error checking ecosystem status; see log.')
    })
  },

  listTaskPool: function() {
    const self = this
    let drugEcoInstance
    drugEcosystem.deployed().then(function (instance) {
      drugEcoInstance = instance
      return drugEcoInstance.getTaskCount()
    }).then(function (count) {
      let taskCount = count.valueOf()
      for (let i = 0; i <taskCount; i++) {
        let taskBonus
        let tokenSym
        drugEcosystem.deployed().then(function () {
          return drugEcoInstance.taskPool(i)
        }).then(function (tmpTaskAddr) {
          let taskInstance
          drugTask.at(tmpTaskAddr).then(function (instance){
            taskInstance = instance
            return taskInstance.bonus()
          }).then (function (bonus) {
            taskBonus = web3.fromWei(bonus.valueOf() , "ether")
            return taskInstance.symbol()
          }).then (function (symbol) {
            tokenSym = symbol
            return taskInstance.task()
          }).then(function(task){
            const taskDate = new Date(task[7].valueOf()*1000)
            const playerText = "<strong>id:</strong>" + i +
                              " <strong>Tool Name:</strong>" + task[1] +
                              " <strong>Max Players:</strong>" + task[3] + 
                              " <strong>salary:</strong>" + web3.fromWei(task[4], "ether") + "ETH" +
                              " <strong>bonus:</strong>" + taskBonus + tokenSym +
                              " <strong>finished:</strong>" + task[6] + 
                              " <strong>date:</strong>" + taskDate.toISOString()
            const ul = document.getElementById("taskList")
            const li = document.createElement("li")
            li.innerHTML = playerText
            ul.appendChild(li)
          })
        })
      }
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error listing players; see log.')
    })
  },

  createTask: function () {
    const self = this
    let drugEcoInstance
    drugEcosystem.deployed().then(function (instance) {
      drugEcoInstance = instance
      return drugEcoInstance.createTask({ from: account })
    }).then(function () {
      self.setStatus('Task has been created. Please refresh webpage.')
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error creating a task; see log.')
    })
  },

  selectTask: function() {
    const self = this
    let drugEcoInstance
    drugEcosystem.deployed().then(function (instance) {
      drugEcoInstance = instance
      const taskId = document.getElementById('taskId').value
      return drugEcoInstance.taskPool(taskId)
    }).then(function (addr) {
      taskAddress = addr

      self.checkAgreementStatus()
      self.checkTokenStatus()
      self.getTotolSupply()
      self.getAccountBalance()
      self.checkTaskStatus()
      self.getTaskInfo()
      self.listPlayerPool()
      self.listDataReqPool()

      document.getElementById('DrugTaskPanel').style.display = 'inline'
    }).then(function () {
      self.setStatus('Task has been selected.')
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error selecting a task; see log.')
    })
  },

  checkAgreementStatus: function() {
    const self = this
    let legitInstance
    drugTask.at(taskAddress).then(function (instance) {
      legitInstance = instance
      return legitInstance.LegiFiles(account)
    }).then(function (result) {
      if (result[4]) {
        const legiStatus = document.getElementById('legiStatus')
        legiStatus.innerHTML = "You have signed the agreement for using the Drug R&D Ecosystem!"
        legiStatus.style.backgroundColor = "#CCFFCC"

        /* Extract agreement info from blockchain */
        document.getElementById('sign').disabled = true
        document.getElementById('hashCode').value = result[1]
        document.getElementById('hashSign').value = result[2]
        const fileURL = document.getElementById('fileURL')
        fileURL.value = result[0]
        fileURL.readOnly = true

        document.getElementById('loyaltyTokenPanel').style.display = 'inline'
        document.getElementById('taskManagementPanel').style.display = 'inline'
      }
      else {
        const legiStatus = document.getElementById('legiStatus')
        legiStatus.innerHTML = "Please sign agreement before use the Drug R&D Ecosystem!"
        document.getElementById('hashCode').value = ""
        document.getElementById('hashSign').value = ""
        document.getElementById('fileURL').value = ""

        document.getElementById('loyaltyTokenPanel').style.display = 'none'
        document.getElementById('taskManagementPanel').style.display = 'none'
      }
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error checking legitimation status; see log.')
    })
  },

  openLegalFile: function(event) {
    const chosenFile = event.target.files[0]
    fileReader = new FileReader()
    fileReader.fileName = chosenFile.name
    fileReader.onloadend = this.hashLegalFile
    fileReader.readAsArrayBuffer(chosenFile)
  },

  hashLegalFile: function() {
    const content = fileReader.result
    const hashResult = sha256(content)
    const hashCode = document.getElementById('hashCode')
    hashCode.value = hashResult
    const hashSign = document.getElementById('hashSign')
    web3.eth.sign(account, "0x" + hashResult, function(error, result) {
      if(!error)
        hashSign.value = result;
      else
        console.error(error);
    })
  },

  signAgreement: function() {
    const self = this
    const fileURL = document.getElementById('fileURL')
    const hashCode = document.getElementById('hashCode')
    const hashSign = document.getElementById('hashSign')

    let legitInstance
    drugTask.at(taskAddress).then(function (instance) {
      legitInstance = instance
      return legitInstance.signAgreement(fileURL.value, hashCode.value, hashSign.value, { from: account })
    }).then(function () {
      self.setStatus('Agreement has been signed on blockchain!')
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error signing agreement; see log.')
    })
  },

  checkTokenStatus: function() {
    const self = this
    let loyaltyInstance
    drugTask.at(taskAddress).then(function (instance) {
      loyaltyInstance = instance
      return loyaltyInstance.isTokenInitialized()
    }).then(function (init) {
      if (init) {
        document.getElementById('tokenInitialization').style.display = 'none'
        document.getElementById('initToken').disabled = true
      }
      return loyaltyInstance.owner()
    }).then(function (owner) {
      if (0 != owner.localeCompare(account)) {
        document.getElementById('tokenInitialization').style.display = 'none'
        document.getElementById('initToken').disabled = true
      }
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error checking legitimation status; see log.')
    })
  },

  getTotolSupply: function () {
    const self = this
    let loyaltyInstance
    drugTask.at(taskAddress).then(function (instance) {
      loyaltyInstance = instance
      return loyaltyInstance.totalSupply()
    }).then(function (value) {
      const totalSupply = document.getElementById('totalSupply')
      totalSupply.innerHTML = web3.fromWei(value.valueOf(), "ether")
      return loyaltyInstance.bonus()
    }).then(function (value) {
      const bonusToken = document.getElementById('bonusToken')
      bonusToken.innerHTML = web3.fromWei(value.valueOf(), "ether")
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error getting total supply and bonus; see log.')
    })
  },

  getAccountBalance: function () {
    const self = this
    document.getElementById('accountAddress').innerHTML = account

    let loyaltyInstance
    drugTask.at(taskAddress).then(function (instance) {
      loyaltyInstance = instance
      return loyaltyInstance.balanceOf(account)
    }).then(function (result) {
      const accountBalance = document.getElementById('accountBalance')
      accountBalance.innerHTML = web3.fromWei(result.valueOf(), "ether")
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error getting account balance; see log.')
    })
  },

  initLoyaltyToken: function () {
    const self = this
    const initSupply = parseInt(document.getElementById('initSupply').value)
    const initBonus = parseInt(document.getElementById('initBonus').value)
    const initName = document.getElementById('initName').value
    const initSymbol = document.getElementById('initSymbol').value
    const decimal = 18

    this.setStatus('Initializing loyalty tokens... (please wait)')

    let loyaltyInstance
    drugTask.at(taskAddress).then(function (instance) {
      loyaltyInstance = instance
      return loyaltyInstance.initiateToken(web3.toWei(initSupply, "ether"),
                                           web3.toWei(initBonus, "ether"),
                                           initName, initSymbol, decimal,
                                           { from: account })
    }).then(function () {
      self.setStatus('Initialization complete!')
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error Initializing loyalty tokens; see log.')
    })
  },

  sendLoyaltyToken: function () {
    const self = this
    const amount = parseFloat(document.getElementById('amount').value)
    const receiver = document.getElementById('receiver').value

    this.setStatus('Initializing transaction... (please wait)')

    let loyaltyInstance
    drugTask.at(taskAddress).then(function (instance) {
      loyaltyInstance = instance
      return loyaltyInstance.transfer(receiver, web3.toWei(amount, "ether"), { from: account })
    }).then(function () {
      self.setStatus('Transaction complete!')
      self.getAccountBalance()
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error sending coin; see log.')
    })
  },

  generateDataCode: function() {
    const identity = EthCrypto.createIdentity();
    const dataAccessCode = document.getElementById('dataAccessCode')
    dataAccessCode.value = identity.privateKey.substring(2)
    const dataEncryptCode = document.getElementById('comPublicKey')
    dataEncryptCode.value = EthCrypto.publicKey.compress(identity.publicKey)
  },

  saveFile: function(filename, data) {
    let blob = new Blob([data], {type: "application/octet-stream"});
  
    if(window.navigator.msSaveOrOpenBlob) {
        window.navigator.msSaveBlob(blob, filename);
    }
    else{
        var elem = window.document.createElement('a');
        elem.href = window.URL.createObjectURL(blob);
        elem.download = filename;        
        document.body.appendChild(elem);
        elem.click();        
        document.body.removeChild(elem);
    }
  },

  openDataFile: function(event) {
    const chosenFile = event.target.files[0]
    fileReader = new FileReader()
    fileReader.fileName = chosenFile.name
    fileReader.readAsArrayBuffer(chosenFile)
  },

  encryptDataFile: function() {
    const self = this
    const data = fileReader.result
    let dataEncryptCode

    /* Encrpte data file */
    if (isTaskOwner) {
      dataEncryptCode = document.getElementById('ownerEncryptCode').value
    }
    else {
      dataEncryptCode = document.getElementById('playerEncryptCode').value
    }

    let publicKey = EthCrypto.publicKey.decompress(dataEncryptCode)
    let buffPublicKey = Buffer.from(publicKey, 'hex')
    let bufferData = Buffer.from(data)
    let encryptedData = ecies.encrypt(buffPublicKey, bufferData)

    /* compute hash */
    const hashResult = sha256(encryptedData)
    if (isTaskOwner) {
      document.getElementById('ownerDataEncryptHash').value = hashResult
    }
    else {
      document.getElementById('playerDataEncryptHash').value = hashResult
    }

    /* save to a file */
    self.saveFile("Encrypted_" + fileReader.fileName, encryptedData)
  },

  decryptDataFile: function() {
    const self = this
    const encryptedData = fileReader.result

    /* check whether the hash is consistent */
    const hashResult = sha256(encryptedData)
    let   dataEncryptHash
    if (isTaskOwner) {
      dataEncryptHash = document.getElementById('ownerDataDecryptHash')
    }
    else {
      dataEncryptHash = document.getElementById('playerDataDecryptHash')
    }
  
    /* verify data file with hash */
    if (0 != dataEncryptHash.value.localeCompare(hashResult)) {
      alert("BE CAREFUL! Data file hash is NOT consistent with hash on blockchain!")
    }

    /* decrpte data file */
    const dataAccessCode = document.getElementById('dataAccessCode')
    let buffPrivateKey = Buffer.from(dataAccessCode.value, 'hex')
    let buffEncryptedData = Buffer.from(encryptedData)
    try {
      let decryptedData = ecies.decrypt(buffPrivateKey, buffEncryptedData)
      self.saveFile("Decrypted_" + fileReader.fileName, decryptedData)
    }
    catch(e) {
      console.log(e)
      self.setStatus('Incorrect data access code; see log.')
      alert("Fail to decrypt data: Incorrect data access code!")
    }
  },

  listDataReqPool: function() {
    const self = this
    let dataInstance
    drugTask.at(taskAddress).then(function (instance) {
      dataInstance = instance
      return dataInstance.getRequestCount()
    }).then(function (count) {
      let reqCount = count.valueOf()
      for (let i = 0; i <reqCount; i++) {
        drugTask.at(taskAddress).then(function () {
          return dataInstance.dataReqPool(i)
        }).then(function (req) {
          const reqDate = new Date(req[5].valueOf()*1000)
          const reqText = "<strong>id:</strong>" + i + " <strong>address:</strong>" + req[0] +
                          " <strong>approved:</strong>" + req[4] + " <strong>date:</strong>" + reqDate.toISOString()
          const ul = document.getElementById("dataReqPool")
          const li = document.createElement("li")
          li.innerHTML = reqText
          ul.appendChild(li)
        })
      }
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error listing requests; see log.')
    })
  },

getDataInfo: function() {
    const self = this
    let dataInstance
    drugTask.at(taskAddress).then(function (instance) {
      dataInstance = instance
      const playerId = document.getElementById('playerId').value
      return dataInstance.playerPool(playerId)
    }).then(function (player) {
      if (isTaskOwner) {
        const result = web3.fromWei(player[3].valueOf(), 'ether')
        document.getElementById('ownerResult').value = result
        return dataInstance.dataReqPool(player[2])
      }
      else {
        return dataInstance.dataReqPool(player[1])
      }
    }).then(function (data) {
      if (isTaskOwner) {
        document.getElementById('ownerDataDecryptURL').value = data[2]
        document.getElementById('ownerDataDecryptHash').value = data[3]
      }
      else {
        document.getElementById('playerDataDecryptURL').value = data[2]
        document.getElementById('playerDataDecryptHash').value = data[3]
      }
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error locating data info; see log.')
    })
  },

  getEncryptCode: function() {
    const self = this
    let dataInstance
    drugTask.at(taskAddress).then(function (instance) {
      dataInstance = instance
      const playerId = document.getElementById('playerId').value
      return dataInstance.playerPool(playerId)
    }).then(function (player) {
      if (isTaskOwner) {
        return dataInstance.dataReqPool(player[1])
      }
      else {
        return dataInstance.dataReqPool(player[2])
      }
    }).then(function (data) {
      if (isTaskOwner) {
        document.getElementById('ownerEncryptCode').value = data[1]
      }
      else {
        document.getElementById('playerEncryptCode').value = data[1]
      }
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error locating request info; see log.')
    })
  },

  checkTaskStatus: function() {
    const self = this
    let taskInstance

    drugTask.at(taskAddress).then(function (instance) {
      taskInstance = instance
      return taskInstance.isTaskInitialized()
    }).then(function (init) {
      if (init) {
        document.getElementById('taskInitialization').style.display = "none"
        document.getElementById('initTask').disabled = true
      }
      return taskInstance.owner()
    }).then(function (owner) {
      if (0 == owner.localeCompare(account)) {
        isTaskOwner = true;
        document.getElementById('taskOwnerPanel').style.display = "inline"
        document.getElementById('toolPlayerPanel').style.display = "none"
      }
      else {
        isTaskOwner = false;
        document.getElementById('toolPlayerPanel').style.display = "inline"
        document.getElementById('taskOwnerPanel').style.display = "none"
        document.getElementById('taskInitialization').style.display = "none"
        document.getElementById('initTask').disabled = true
      }
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error checking task status; see log.')
    })
  },

  initTaskManage: function() {
    const self = this
    const initToolName = document.getElementById('initToolName').value
    const initMaxPlayer = parseInt(document.getElementById('initMaxPlayer').value)
    const initPlayerSalary = parseFloat(document.getElementById('initPlayerSalary').value)
    const totalSalary = web3.toWei(initMaxPlayer*initPlayerSalary, "ether")

    this.setStatus('Initializing task management... (please wait)')

    let taskInstance
    drugTask.at(taskAddress).then(function (instance) {
      taskInstance = instance
      return taskInstance.initiateTask(initToolName, initMaxPlayer,
                                       web3.toWei(initPlayerSalary, "ether"), true,
                                       {from: account, value: totalSalary})
    }).then(function () {   
      self.setStatus('Initialization complete!')
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error Initializing task management; see log.')
    })
  },

  getTaskInfo: function() {
    const self = this
    let taskInstance
    drugTask.at(taskAddress).then(function (instance) {
      taskInstance = instance
      return taskInstance.task()
    }).then(function (task) {
      document.getElementById('taskAddress').innerHTML = taskInstance.address
      document.getElementById('taskName').innerHTML = task[0]
      document.getElementById('toolName').innerHTML = task[1]
      document.getElementById('maxPlayers').innerHTML = task[3].valueOf()
      document.getElementById('playerSalary').innerHTML = web3.fromWei(task[4].valueOf(), "ether"),

      web3.eth.getBalance(taskInstance.address, function(err, balance) {
        document.getElementById('taskBalance').innerHTML = web3.fromWei(balance.valueOf(), "ether")
      })
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error getting task info; see log.')
    })
  },

  listPlayerPool: function() {
    const self = this
    let taskInstance
    drugTask.at(taskAddress).then(function (instance) {
      taskInstance = instance
      return taskInstance.getPlayerCount()
    }).then(function (count) {
      let playerCount = count.valueOf()
      for (let i = 0; i <playerCount; i++) {
        drugTask.at(taskAddress).then(function () {
          return taskInstance.playerPool(i)
        }).then(function (player) {
          const playerDate = new Date(player[5].valueOf()*1000)
          const playerText = "<strong>id:</strong>" + i +
                             " <strong>address:</strong>" + player[0] +
                             " <strong>inputId:</strong>" + player[1] + 
                             " <strong>outputId:</strong>" + player[2] + 
                             " <strong>result:</strong>" + web3.fromWei(player[3], "ether") + 
                             " <strong>status:</strong>" + player[4] + 
                             " <strong>date:</strong>" + playerDate.toISOString()
          const ul = document.getElementById("playerList")
          const li = document.createElement("li")
          li.innerHTML = playerText
          ul.appendChild(li)
        })
      }
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error listing players; see log.')
    })
  },

  submitPlayerRequest: function() {
    const self = this
    const dataAccessCode = document.getElementById('dataAccessCode')
    const dataPublicKey = EthCrypto.publicKeyByPrivateKey('0x' + dataAccessCode.value)
    const dataEncryptCode = EthCrypto.publicKey.compress(dataPublicKey)

    let taskInstance
    drugTask.at(taskAddress).then(function (instance) {
      taskInstance = instance
      return taskInstance.owner()
    }).then(function(ownerAddr) {
      return taskInstance.submitTaskRequest(ownerAddr, dataEncryptCode, { from: account })
    }).then(function () {
      self.setStatus('Player request submitted!')
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error submitting player request; see log.')
    })
  },

  submitPlayerResult: function() {
    const self = this
    let taskInstance
    drugTask.at(taskAddress).then(function (instance) {
      taskInstance = instance
      const playerId = document.getElementById('playerId').value
      const dataEncryptHash = document.getElementById('playerDataEncryptHash').value
      const dataEncryptURL = document.getElementById('playerDataEncryptURL').value
      const playerResult = web3.toWei(document.getElementById('playerResult').value,  "ether")
      return taskInstance.submitTaskResult(playerId, dataEncryptURL,
                                             dataEncryptHash,playerResult,
                                             { from: account })
    }).then(function () {
      self.setStatus('Player result submitted!')
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error submitting player result; see log.')
    })
  },

  approvePlayerRequest: function() {
    const self = this
    const dataAccessCode = document.getElementById('dataAccessCode')
    const dataPublicKey = EthCrypto.publicKeyByPrivateKey('0x' + dataAccessCode.value)
    const dataEncryptCode = EthCrypto.publicKey.compress(dataPublicKey)

    let taskInstance
    drugTask.at(taskAddress).then(function (instance) {
      taskInstance = instance
      const playerId = document.getElementById('playerId').value
      const dataEncryptHash = document.getElementById('ownerDataEncryptHash').value
      const dataEncryptURL = document.getElementById('ownerDataEncryptURL').value
      return taskInstance.approveTaskRequest(playerId, dataEncryptCode,
                                               dataEncryptURL, dataEncryptHash,
                                               { from: account })
    }).then(function () {
      self.setStatus('Player request approved!')
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error approving player request; see log.')
    })
  },

  approvePlayerResult: function() {
    const self = this
    const isBest = 0
    let taskInstance
    drugTask.at(taskAddress).then(function (instance) {
      taskInstance = instance
      const playerId = document.getElementById('playerId').value
      return taskInstance.approveTaskResult(playerId, { from: account })
    }).then(function () {
      self.setStatus('Player result is approved. Payment sent!')
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error approving player result; see log.')
    })
  },

  rejectPlayerResult: function() {
    const self = this
    let taskInstance
    drugTask.at(taskAddress).then(function (instance) {
      taskInstance = instance
      const playerId = document.getElementById('playerId').value
      return taskInstance.rejectTaskResult(playerId, { from: account })
    }).then(function () {
      self.setStatus('Player result is rejected!')
    }).catch(function (e) {
      console.log(e)
      self.setStatus('Error rejecting player result; see log.')
    })
  },
}

window.App = App

window.addEventListener('load', function () {
  // Checking if Web3 has been injected by the browser (Mist/MetaMask)
  if (typeof web3 !== 'undefined') {
    console.warn(
      'Using web3 detected from external source.' +
      ' If you find that your accounts don\'t appear or you have 0 MetaCoin,' +
      ' ensure you\'ve configured that source properly.' +
      ' If using MetaMask, see the following link.' +
      ' Feel free to delete this warning. :)' +
      ' http://truffleframework.com/tutorials/truffle-and-metamask'
    )
    // Use Mist/MetaMask's provider
    window.web3 = new Web3(web3.currentProvider)
  } else {
    console.warn(
      'No web3 detected. Falling back to http://127.0.0.1:8545.' +
      ' You should remove this fallback when you deploy live, as it\'s inherently insecure.' +
      ' Consider switching to Metamask for development.' +
      ' More info here: http://truffleframework.com/tutorials/truffle-and-metamask'
    )
    // fallback - use your fallback strategy (local node / hosted node + in-dapp id mgmt / fail)
    window.web3 = new Web3(new Web3.providers.HttpProvider('http://127.0.0.1:8545'))
  }

  App.start()
})
