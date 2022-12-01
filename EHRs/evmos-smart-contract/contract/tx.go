package contract

import (
	"context"
	"crypto/ecdsa"
	"fmt"
	"math/big"

	"github.com/ethereum/go-ethereum/accounts/abi/bind"
	"github.com/ethereum/go-ethereum/common"
	ethTypes "github.com/ethereum/go-ethereum/core/types"
	ethCrypto "github.com/ethereum/go-ethereum/crypto"
	"github.com/ethereum/go-ethereum/ethclient"
)

// This function transacts an Ethereum contract
// it receives the path to the compiled contract in JSON, its address, a private key
// and a nodeURI with method to call and its parameters if there is any in Json format
// then it calls the given method and passes the parameters which suppose to change the state
// This function supports simple types as method parameters for the contract e.g numbers and strings
// Ex: SimpleTx( "./contracts/compiled_contracts/HelloWorld.json", "0xe0112de09D54E3b5909A60C807BB60c973a20678", <Private_Key>, "http://localhost:8545", "update", "{\"params\":[\"new msg\"]}")
func SimpleTx(contractJsonFilePath string, contractAddress string, privateKey *ecdsa.PrivateKey, nodeURI string, method string, paramsJson string) (*ethTypes.Transaction, error) {

	if paramsJson == "" {
		return execTx(contractJsonFilePath, contractAddress, privateKey, nodeURI, method)
	}

	paramsToPass, err := ParseJsonParams(paramsJson)
	if err != nil {
		return nil, err
	}
	return execTx(contractJsonFilePath, contractAddress, privateKey, nodeURI, method, paramsToPass...)
}

// This function transfers tokens from one account to another account on a smart contract
func Transfer(contractJsonFilePath string, contractAddress string, privateKey *ecdsa.PrivateKey, toAddress string, amount *big.Int, nodeURI string) (*ethTypes.Transaction, error) {

	toAddr := common.HexToAddress(toAddress)
	return execTx(contractJsonFilePath, contractAddress, privateKey, nodeURI, METHOD_TRANSFER, toAddr, amount)
}

// This function executes a state changing transaction on a smart contract
// It receives the method parameters as a list of interface{}
func execTx(contractJsonFilePath string, contractAddress string, privateKey *ecdsa.PrivateKey, nodeURI string, method string, params ...interface{}) (*ethTypes.Transaction, error) {

	ec, err := ethclient.Dial(nodeURI)
	if err != nil {
		return nil, err
	}

	/*--------*/

	publicKey := privateKey.Public()
	publicKeyECDSA, ok := publicKey.(*ecdsa.PublicKey)
	if !ok {
		return nil, fmt.Errorf("invalid key")
	}

	fromAddress := ethCrypto.PubkeyToAddress(*publicKeyECDSA)
	nonce, err := ec.PendingNonceAt(context.Background(), fromAddress)
	if err != nil {
		return nil, err
	}

	chainID, err := ec.ChainID(context.Background())
	if err != nil {
		return nil, err
	}

	auth, err := bind.NewKeyedTransactorWithChainID(privateKey, chainID)
	if err != nil {
		return nil, err
	}
	auth.Nonce = big.NewInt(int64(nonce))
	auth.Value = big.NewInt(0) // in wei
	//TODO: These need to be parameterized
	auth.GasLimit = uint64(3000000) // in units
	auth.GasPrice = big.NewInt(1000000)

	/*--------*/

	address := common.HexToAddress(contractAddress)
	instance, err := Instance(contractJsonFilePath, address, ec)
	if err != nil {
		return nil, err
	}

	return instance.Transact(auth, method, params...)
}
