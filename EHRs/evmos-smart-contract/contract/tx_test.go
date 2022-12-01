package contract_test

import (
	"math/big"
	"reflect"
	"testing"
	"time"

	"github.com/cosmos/cosmos-sdk/crypto/keyring"
	sdk "github.com/cosmos/cosmos-sdk/types"
	"github.com/evmos/ethermint/crypto/ethsecp256k1"
	"github.com/mojtaba-esk/evmos-smart-contract/cmd"
	"github.com/mojtaba-esk/evmos-smart-contract/contract"
	"github.com/stretchr/testify/require"
)

func TestSimpleTx(t *testing.T) {

	tt := []struct {
		name           string
		contractName   string
		deployParams   string
		txCallMethod   string
		txCallParams   string
		queryMethod    string
		queryParams    string
		wantErr        bool
		wantOutput     []interface{}
		evaluateOutput func([]interface{}, []interface{}) bool
	}{
		{
			name:         "Contract without parameters, increment call",
			contractName: "Counter1",
			deployParams: ``,
			txCallMethod: "increment",
			txCallParams: ``,
			queryMethod:  "count",
			wantErr:      false,
			wantOutput:   []interface{}{big.NewInt(2)},
			evaluateOutput: func(gotOutput, wantOutput []interface{}) bool {
				return reflect.DeepEqual(wantOutput[0].(*big.Int), gotOutput[0].(*big.Int))
			},
		},
		{
			name:         "Contract with a string parameter",
			contractName: "HelloWorld",
			deployParams: `{"params":["Ciao message"]}`,
			txCallMethod: "update",
			txCallParams: `{"params":["modified message"]}`,
			queryMethod:  "message",
			wantErr:      false,
			wantOutput:   []interface{}{"modified message"},
			evaluateOutput: func(gotOutput, wantOutput []interface{}) bool {
				return reflect.DeepEqual(wantOutput[0].(string), gotOutput[0].(string))
			},
		},
		{
			name:         "Contract with two parameters, increment call with init",
			contractName: "Counter2",
			deployParams: `{"params":[51, "Test message"]}`,
			txCallMethod: "increment",
			txCallParams: ``,
			queryMethod:  "count",
			wantErr:      false,
			wantOutput:   []interface{}{big.NewInt(52)},
			evaluateOutput: func(gotOutput, wantOutput []interface{}) bool {
				return reflect.DeepEqual(wantOutput[0].(*big.Int), gotOutput[0].(*big.Int))
			},
		},
	}

	privateKey, err := cmd.ExportPrivateKey(TestKey, keyring.BackendTest, TestDataDir, "", cmd.AppName, nil)
	if err != nil {
		t.Fatalf("Cannot get the private key: %v", err)
	}

	for _, tc := range tt {

		t.Run(tc.name, func(t *testing.T) {

			jsonFilePath, err := GetContractJsonFilePath(tc.contractName)
			if err != nil {
				t.Fatalf("Error getting contract json file path: %v", err)
			}

			address, _, _, err := contract.Deploy(jsonFilePath, privateKey, TestNodeURI, tc.deployParams)
			if err != nil {
				t.Fatalf("contract.Deploy() error = %v", err)
			}
			if address.Bytes() == nil || address.Hex() == "" {
				t.Fatalf("contract.Deploy() error = Got empty address")
			}

			// We need to wait for the deploy tx to be included in a block and get processed
			//TODO: Find a better way to determine how long to wait or use sync mode block for deployment
			time.Sleep(1 * time.Second)

			_, err = contract.SimpleTx(jsonFilePath, address.Hex(), privateKey, TestNodeURI, tc.txCallMethod, tc.txCallParams)
			if err != nil {
				t.Fatalf("contract.SimpleTx() error = %v", err)
			}

			time.Sleep(1 * time.Second)

			gotOutput, err := contract.SimpleQuery(jsonFilePath, address.Hex(), TestNodeURI, tc.queryMethod, tc.queryParams)
			if (err != nil) != tc.wantErr {
				t.Fatalf("contract.SimpleQuery() error = %v, wantErr %v", err, tc.wantErr)
			}

			if !tc.evaluateOutput(gotOutput, tc.wantOutput) {
				t.Fatalf("contract.SimpleQuery() = %v, want %v", gotOutput, tc.wantOutput)
			}

		})
	}
}

func TestTransfer(t *testing.T) {

	gkey, _ := ethsecp256k1.GenerateKey()

	tt := []struct {
		name           string
		contractName   string
		deployParams   string
		accountAddress string
		transferAmount *big.Int
		wantErr        bool
		wantOutput     sdk.DecCoin
		evaluateOutput func([]interface{}, []interface{}) bool
	}{
		{
			name:           "ERC20 Contract",
			contractName:   "MyTestToken",
			deployParams:   ``,
			accountAddress: gkey.PubKey().Address().String(),
			transferAmount: big.NewInt(123),
			wantOutput:     sdk.NewDecCoin("MTT", sdk.NewInt(123)),
			wantErr:        false,
		},
	}

	privateKey, err := cmd.ExportPrivateKey(TestKey, keyring.BackendTest, TestDataDir, "", cmd.AppName, nil)
	if err != nil {
		t.Fatalf("cannot get the private key: %v", err)
	}

	for _, tc := range tt {

		t.Run(tc.name, func(t *testing.T) {

			jsonFilePath, err := GetContractJsonFilePath(tc.contractName)
			if err != nil {
				t.Fatalf("cannot get contract json file path: %v", err)
			}

			address, _, _, err := contract.Deploy(jsonFilePath, privateKey, TestNodeURI, tc.deployParams)
			if err != nil {
				t.Fatalf("contract.Deploy() error = %v", err)
			}
			if address.Bytes() == nil || address.Hex() == "" {
				t.Fatalf("contract.Deploy() error = Got empty address")
			}

			// We need to wait for the deploy tx to be included in a block and get processed
			//TODO: Find a better way to determine how long to wait or use sync mode block for deployment
			time.Sleep(1 * time.Second)

			_, err = contract.Transfer(jsonFilePath, address.Hex(), privateKey, tc.accountAddress, tc.transferAmount, TestNodeURI)
			if err != nil {
				t.Fatalf("contract.Transfer() error = %v", err)
			}

			time.Sleep(1 * time.Second)

			gotOutput, err := contract.QueryBalance(jsonFilePath, address.Hex(), tc.accountAddress, TestNodeURI)
			if !tc.wantErr {
				require.NoError(t, err)
			}

			if !gotOutput.Equal(tc.wantOutput) {
				t.Fatalf("contract.QueryBalance() = %v, want %v", gotOutput, tc.wantOutput)
			}
		})
	}
}
