package cmd

import (
	"fmt"
	"path/filepath"

	"github.com/cosmos/cosmos-sdk/client"
	cosmosFlags "github.com/cosmos/cosmos-sdk/client/flags"
	"github.com/mojtaba-esk/evmos-smart-contract/contract"

	"github.com/spf13/cobra"
)

func init() {
	_rootCmd.AddCommand(deployContractsCmd)
	deployContractsCmd.Flags().String(cosmosFlags.FlagFrom, "", "account address to sign the deployment tx")
}

var deployContractsCmd = &cobra.Command{
	Use:   "deploy [contract_name] [init_params_json]",
	Short: "Deploy a contract to the evmos chain",
	Long:  `This command receives a contract name and deploys it on the evmos chain`,
	Args:  cobra.RangeArgs(1, 2),
	RunE: func(cmd *cobra.Command, args []string) error {

		clientCtx, err := client.GetClientTxContext(cmd)
		if err != nil {
			return err
		}

		keyName := clientCtx.GetFromName()
		if keyName == "" {
			return fmt.Errorf("no key name provided in `--from`")
		}
		privateKey, err := getPrivateKeyFromCmd(cmd, keyName)
		if err != nil {
			return err
		}

		contractJsonFilePath := getCompiledContractPath(cmd, args[0])
		initParamsJson := ""
		if len(args) == 2 {
			initParamsJson = args[1] // This is received in JSON format
		}
		address, tx, _, err := contract.Deploy(contractJsonFilePath, privateKey, clientCtx.NodeURI, initParamsJson)
		if err != nil {
			return err
		}

		fmt.Println("Contract Address: ", address.Hex())
		fmt.Println("TX Hash: ", tx.Hash().Hex())

		return nil
	},
}

func getCompiledContractPath(cmd *cobra.Command, contractName string) string {
	compiledContractsDir, _ := cmd.Flags().GetString(FlagCompiledContractsPath)
	return filepath.Join(compiledContractsDir, contractName+".json")
}
