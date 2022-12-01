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

func TestSimpleQuery(t *testing.T) {

	tt := []struct {
		name           string
		contractName   string
		deployParams   string
		queryMethod    string
		queryParams    string
		wantErr        bool
		wantOutput     []interface{}
		evaluateOutput func([]interface{}, []interface{}) bool
	}{
		{
			name:         "Contract without parameters",
			contractName: "Counter1",
			deployParams: ``,
			queryMethod:  "count",
			wantErr:      false,
			wantOutput:   []interface{}{big.NewInt(1)},
			evaluateOutput: func(gotOutput, wantOutput []interface{}) bool {
				return reflect.DeepEqual(wantOutput[0].(*big.Int), gotOutput[0].(*big.Int))
			},
		},
		{
			name:           "Missing method name in query",
			contractName:   "Counter1",
			deployParams:   ``,
			queryMethod:    "",
			wantErr:        true,
			wantOutput:     []interface{}{},
			evaluateOutput: func(gotOutput, wantOutput []interface{}) bool { return true },
		},
		{
			name:         "Contract with a string parameter",
			contractName: "HelloWorld",
			deployParams: `{"params":["Ciao message"]}`,
			queryMethod:  "message",
			wantErr:      false,
			wantOutput:   []interface{}{"Ciao message"},
			evaluateOutput: func(gotOutput, wantOutput []interface{}) bool {
				return reflect.DeepEqual(wantOutput[0].(string), gotOutput[0].(string))
			},
		},
		{
			name:         "Contract with two parameters, query message",
			contractName: "Counter2",
			deployParams: `{"params":[51, "Test message"]}`,
			queryMethod:  "message",
			wantErr:      false,
			wantOutput:   []interface{}{"Test message"},
			evaluateOutput: func(gotOutput, wantOutput []interface{}) bool {
				return reflect.DeepEqual(wantOutput[0].(string), gotOutput[0].(string))
			},
		},
		{
			name:         "Contract with two parameters, query count",
			contractName: "Counter2",
			deployParams: `{"params":[51, "Test message"]}`,
			queryMethod:  "count",
			wantErr:      false,
			wantOutput:   []interface{}{big.NewInt(51)},
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

			gotOutput, err := contract.SimpleQuery(jsonFilePath, address.Hex(), TestNodeURI, tc.queryMethod, tc.queryParams)
			if !tc.wantErr {
				require.NoError(t, err)
			}

			if !tc.evaluateOutput(gotOutput, tc.wantOutput) {
				t.Fatalf("contract.SimpleQuery() = %v, want %v", gotOutput, tc.wantOutput)
			}

		})
	}
}

func TestQueryBalance(t *testing.T) {

	gkey, _ := ethsecp256k1.GenerateKey()

	tt := []struct {
		name           string
		contractName   string
		deployParams   string
		accountAddress string
		queryParams    string
		wantErr        bool
		wantOutput     sdk.DecCoin
		evaluateOutput func([]interface{}, []interface{}) bool
	}{
		{
			name:           "ERC20 Contract",
			contractName:   "MyTestToken",
			deployParams:   ``,
			accountAddress: gkey.PubKey().Address().String(),
			wantOutput:     sdk.NewDecCoin("MTT", sdk.NewInt(0)),
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

			address, tx, _, err := contract.Deploy(jsonFilePath, privateKey, TestNodeURI, tc.deployParams)
			if err != nil {
				t.Fatalf("contract.Deploy() error = %v", err)
			}
			if address.Bytes() == nil || address.Hex() == "" {
				t.Fatalf("contract.Deploy() error = Got empty address")
			}

			// We need to wait for the deploy tx to be included in a block and get processed
			//TODO: Find a better way to determine how long to wait or use sync mode block for deployment
			time.Sleep(1 * time.Second)

			_ = tx
			gotOutput, err := contract.QueryBalance(jsonFilePath, address.Hex(), tc.accountAddress, TestNodeURI)
			if (err != nil) != tc.wantErr {
				t.Fatalf("contract.SimpleQuery() error = %v, wantErr %v", err, tc.wantErr)
			}

			if !gotOutput.Equal(tc.wantOutput) {
				t.Fatalf("contract.SimpleQuery() = %v, want %v", gotOutput, tc.wantOutput)
			}
		})
	}
}
