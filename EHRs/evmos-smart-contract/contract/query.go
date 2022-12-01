package contract

import (
	"math/big"

	"github.com/ethereum/go-ethereum/accounts/abi"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/ethclient"

	sdk "github.com/cosmos/cosmos-sdk/types"
)

// This function queries an Ethereum contract
// it receives the path to the compiled contract in JSON, a private key
// and a nodeURI then it deploys the smart contract to the chain via the given evmos node
// This function supports simple types as method parameters for the contract e.g numbers and strings
// Ex: SimpleQuery( "./contracts/compiled_contracts/HelloWorld.json", "0xe0112de09D54E3b5909A60C807BB60c973a20678", "http://localhost:8545", "message", "{\"params\":[51, \"test msg\"]}")
func SimpleQuery(contractJsonFilePath string, contractAddress string, nodeURI string, method string, paramsJson string) ([]interface{}, error) {

	if paramsJson == "" {
		return execQuery(contractJsonFilePath, contractAddress, nodeURI, method)
	}

	paramsToPass, err := ParseJsonParams(paramsJson)
	if err != nil {
		return nil, err
	}

	return execQuery(contractJsonFilePath, contractAddress, nodeURI, method, paramsToPass...)
}

// This function queries a balance of an account on a smart contract
func QueryBalance(contractJsonFilePath string, contractAddress string, accountAddress string, nodeURI string) (sdk.DecCoin, error) {

	out, err := execQuery(contractJsonFilePath, contractAddress, nodeURI, METHOD_SYMBOL)
	if err != nil {
		return sdk.DecCoin{}, err
	}
	symbol := *abi.ConvertType(out[0], new(string)).(*string)

	accAddr := common.HexToAddress(accountAddress)
	out, err = execQuery(contractJsonFilePath, contractAddress, nodeURI, METHOD_BALANCE_OF, accAddr)
	if err != nil {
		return sdk.DecCoin{}, err
	}
	balance := *abi.ConvertType(out[0], new(*big.Int)).(**big.Int)

	// out, err = execQuery(contractJsonFilePath, contractAddress, nodeURI, METHOD_DECIMALS)
	// if err != nil {
	// 	return sdk.DecCoin{}, err
	// }
	// decimals := *abi.ConvertType(out[0], new(uint8)).(*uint8)
	// fbalance := new(big.Float)
	// fbalance.SetString(balance.String())
	// value := new(big.Float).Quo(fbalance, big.NewFloat(math.Pow10(int(decimals))))

	return sdk.NewDecCoin(symbol, sdk.NewIntFromBigInt(balance)), nil

}

// This function executes a query (read) on a smart contract
// It receives the method parameters as a list of interface{}
func execQuery(contractJsonFilePath string, contractAddress string, nodeURI string, method string, params ...interface{}) (out []interface{}, err error) {
	ec, err := ethclient.Dial(nodeURI)
	if err != nil {
		return nil, err
	}

	address := common.HexToAddress(contractAddress)
	instance, err := Instance(contractJsonFilePath, address, ec)
	if err != nil {
		return nil, err
	}

	err = instance.Call(nil, &out, method, params...)
	return out, err
}
