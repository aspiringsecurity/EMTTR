package contract

import (
	"context"
	"crypto/ecdsa"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"math/big"
	"strings"

	"github.com/ethereum/go-ethereum/accounts/abi"
	"github.com/ethereum/go-ethereum/accounts/abi/bind"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/ethclient"

	ethCrypto "github.com/ethereum/go-ethereum/crypto"
)

// This function deploys an Ethereum contract
// it receives the path to the compiled contract in JSON, a private key
// and a nodeURI then it deploys the smart contract to the chain via the given evmos node
func Deploy(contractJsonFilePath string, privateKey *ecdsa.PrivateKey, nodeURI string, initParamsJson string) (common.Address, *types.Transaction, *bind.BoundContract, error) {

	ec, err := ethclient.Dial(nodeURI)
	if err != nil {
		return common.Address{}, nil, nil, err
	}

	/*--------*/

	publicKey := privateKey.Public()
	publicKeyECDSA, ok := publicKey.(*ecdsa.PublicKey)
	if !ok {
		return common.Address{}, nil, nil, fmt.Errorf("invalid key")
	}

	fromAddress := ethCrypto.PubkeyToAddress(*publicKeyECDSA)
	nonce, err := ec.PendingNonceAt(context.Background(), fromAddress)
	if err != nil {
		return common.Address{}, nil, nil, err
	}

	chainID, err := ec.ChainID(context.Background())
	if err != nil {
		return common.Address{}, nil, nil, err
	}

	auth, err := bind.NewKeyedTransactorWithChainID(privateKey, chainID)
	if err != nil {
		return common.Address{}, nil, nil, err
	}
	auth.Nonce = big.NewInt(int64(nonce))
	auth.Value = big.NewInt(0) // in wei
	//TODO: These need to be parameterized
	auth.GasLimit = uint64(3000000) // in units
	auth.GasPrice = big.NewInt(1000000)

	/*--------*/

	metaData, err := bindMetaData(contractJsonFilePath)
	if err != nil {
		return common.Address{}, nil, nil, err
	}

	parsed, err := metaData.GetAbi()
	if err != nil {
		return common.Address{}, nil, nil, err
	}
	if parsed == nil {
		return common.Address{}, nil, nil, errors.New("GetAbi() returned nil")
	}

	if initParamsJson == "" {
		return bind.DeployContract(auth, *parsed, common.FromHex(metaData.Bin), ec)
	}

	/*--------*/

	paramsToPass, err := ParseJsonParams(initParamsJson)
	if err != nil {
		return common.Address{}, nil, nil, err
	}

	/*--------*/

	return bind.DeployContract(auth, *parsed, common.FromHex(metaData.Bin), ec, paramsToPass...)
}

func Instance(contractJsonFilePath string, address common.Address, backend bind.ContractBackend) (*bind.BoundContract, error) {

	metaData, err := bindMetaData(contractJsonFilePath)
	if err != nil {
		return nil, err
	}

	parsed, err := abi.JSON(strings.NewReader(metaData.ABI))
	if err != nil {
		return nil, err
	}
	return bind.NewBoundContract(address, parsed, backend, backend, backend), nil
}

// This function receives the path of a compiled contract in JSON format
// Retrieve the data from the JSON file and binds it in a metadata variable
// To get prepared for deployment
func bindMetaData(contractJsonFilePath string) (*bind.MetaData, error) {

	bytes, err := ioutil.ReadFile(contractJsonFilePath)
	if err != nil {
		return nil, err
	}

	var contract CompiledContract

	err = json.Unmarshal(bytes, &contract)
	if err != nil {
		return nil, err
	}

	return &bind.MetaData{
		ABI: contract.ABI,
		Bin: contract.Bin,
	}, nil
}
