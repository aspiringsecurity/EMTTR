package cmd

import (
	"fmt"

	"github.com/cosmos/cosmos-sdk/client"
	"github.com/mojtaba-esk/evmos-smart-contract/contract"
	"github.com/spf13/cobra"
)

var queryContractsCmd = &cobra.Command{
	Use:   "query [contract_name] [contract_address] [method_to_call] [method_params_json]",
	Short: "Query a contract to the evmos chain",
	Long:  `This command receives a contract address and queries it`,
	Args:  cobra.RangeArgs(3, 4),
	RunE: func(cmd *cobra.Command, args []string) error {

		clientCtx, err := client.GetClientTxContext(cmd)
		if err != nil {
			return err
		}

		contractJsonFilePath := getCompiledContractPath(cmd, args[0])

		methodParams := ""
		if len(args) == 4 {
			methodParams = args[3]
		}

		out, err := contract.SimpleQuery(contractJsonFilePath, args[1], clientCtx.NodeURI, args[2], methodParams)
		if err != nil {
			return err
		}

		fmt.Printf("output: %v\n", out)

		return nil
	},
}

var balanceContractsCmd = &cobra.Command{
	Use:   "balance [contract_name] [contract_address] [account_address]",
	Short: "Query the balance of a contract",
	Long:  `This command receives a contract address and queries its balance `,
	Args:  cobra.ExactArgs(3),
	RunE: func(cmd *cobra.Command, args []string) error {

		clientCtx, err := client.GetClientTxContext(cmd)
		if err != nil {
			return err
		}

		contractJsonFilePath := getCompiledContractPath(cmd, args[0])
		out, err := contract.QueryBalance(contractJsonFilePath, args[1], args[2], clientCtx.NodeURI)
		if err != nil {
			return err
		}

		fmt.Printf("Amount: %f\n", out.Amount)
		fmt.Printf("Denom: %s\n", out.Denom)

		return nil
	},
}

func init() {
	_rootCmd.AddCommand(queryContractsCmd)
	_rootCmd.AddCommand(balanceContractsCmd)
}
