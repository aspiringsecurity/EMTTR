package contract_test

import (
	"testing"

	"github.com/cosmos/cosmos-sdk/crypto/keyring"
	"github.com/mojtaba-esk/evmos-smart-contract/cmd"
	"github.com/mojtaba-esk/evmos-smart-contract/contract"
	"github.com/stretchr/testify/require"
)

func TestDeploy(t *testing.T) {

	tt := []struct {
		name         string
		contractName string
		deployParams string
		wantErr      bool
	}{
		{
			name:         "Contract with empty code",
			contractName: "NoneExistingContract",
			deployParams: ``,
			wantErr:      true,
		},
		{
			name:         "Contract without parameters",
			contractName: "Counter1",
			deployParams: ``,
			wantErr:      false,
		},
		{
			name:         "Contract with a string parameter",
			contractName: "HelloWorld",
			deployParams: `{"params":["Ciao message"]}`,
			wantErr:      false,
		},
		{
			name:         "Contract with missing params",
			contractName: "HelloWorld",
			deployParams: ``,
			wantErr:      true,
		},
		{
			name:         "Contract with two parameters int and string",
			contractName: "Counter2",
			deployParams: `{"params":[51, "Test message"]}`,
			wantErr:      false,
		},
		{
			name:         "Contract with mismatch parameter types",
			contractName: "Counter2",
			deployParams: `{"params":["text", 51]}`,
			wantErr:      true,
		},
		{
			name:         "Contract with missing only one parameter",
			contractName: "Counter2",
			deployParams: `{"params":[51]}`,
			wantErr:      true,
		},
		{
			name:         "ERC20 Contract",
			contractName: "MyTestToken",
			deployParams: ``,
			wantErr:      false,
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

			address, tx, _, err := contract.Deploy(jsonFilePath, privateKey, TestNodeURI, tc.deployParams)

			if !tc.wantErr {
				require.NoError(t, err)
			}

			if err == nil {
				if address.Bytes() == nil || address.Hex() == "" {
					t.Fatalf("contract.Deploy() error = Got empty address")
				}
				if tx == nil || tx.Hash().Hex() == "" {
					t.Fatalf("contract.Deploy() error = Got empty tx")
				}
			}
		})
	}
}
