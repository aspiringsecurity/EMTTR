const sc = require('./scInteraction');

var testParam = {
    abi:[
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
        },
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
        }
    ],
    contractAddress: "0xe78a0f7e598cc8b0bb87894b0f60dd2a88d6a8ab"
}

testParam.encodedCall =  "instance.methods.setValue(60).encodeABI()";
sc.doInteractionWithSC(testParam);