import time
from unilib import testWallet,sendTransaction,contractFunction,contractFunctionCall

checkWallet = testWallet()

if(checkWallet != ''):
    print("Wallet Provide: "+checkWallet)
else:
    print("Wtf are you doing? node index.js. Start the UniWallet")
    
# acctToSend = input("Enter Address to Send Ether: ")
# sT = sendTransaction(acctToSend)
# print(sT)
print('-----------------------------------------------------------')
print('Smart Contract Interaction')
print('-----------------------------------------------------------')
abi = '''[
	{
		"constant": true,
		"inputs": [],
		"name": "getValue",
		"outputs": [
			{
				"name": "",
				"type": "uint256"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	},
	{
		"constant": false,
		"inputs": [
			{
				"name": "_value",
				"type": "uint256"
			}
		],
		"name": "setValue",
		"outputs": [],
		"payable": false,
		"stateMutability": "nonpayable",
		"type": "function"
	}
]'''
contractAddress = "0xe78a0f7e598cc8b0bb87894b0f60dd2a88d6a8ab"
setValueInput = input("Enter value to Set: ")
setValueInContract = contractFunction(abi,contractAddress,"setValue("+str(setValueInput)+")")
print(setValueInContract)
print('Adding 1 sec delay for transaction to mine')
time.sleep(1)
print('----------------------------------------------------')
print('Getting The Value from the same contract')
print('----------------------------------------------------')
getValueFromContract = contractFunctionCall(abi,contractAddress,"getValue()")
print(getValueFromContract)