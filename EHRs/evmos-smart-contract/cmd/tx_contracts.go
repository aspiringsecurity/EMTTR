package cmd

import (
	"bytes"
	"encoding/json"
	"fmt"
	"math/big"
	"strconv"

	"github.com/cosmos/cosmos-sdk/client"
	cosmosFlags "github.com/cosmos/cosmos-sdk/client/flags"
	"github.com/mojtaba-esk/evmos-smart-contract/contract"

	"github.com/spf13/cobra"
)

var txContractsCmd = &cobra.Command{
	Use:   "tx [contract_name] [contract_address] [method_to_call] [method_params_json]",
	Short: "Call a method of a contract that modifies the state",
	Args:  cobra.RangeArgs(3, 4),
	RunE: func(cmd *cobra.Command, args []string) error {

		clientCtx, err := client.GetClientTxContext(cmd)
		if err != nil {
			return err
		}

		keyName := clientCtx.GetFromName()
		privateKey, err := getPrivateKeyFromCmd(cmd, keyName)
		if err != nil {
			return err
		}

		contractJsonFilePath := getCompiledContractPath(cmd, args[0])
		contractAddress := args[1]
		method := args[2]
		methodParamsJson := ""
		if len(args) == 4 {
			methodParamsJson = args[3]
		}

		tx, err := contract.SimpleTx(contractJsonFilePath, contractAddress, privateKey, clientCtx.NodeURI, method, methodParamsJson)
		if err != nil {
			return err
		}

		fmt.Println("TX Hash: ", tx.Hash().Hex())
		txJson, err := tx.MarshalJSON()
		if err != nil {
			return err
		}

		var prettyJSON bytes.Buffer
		err = json.Indent(&prettyJSON, txJson, "", "  ")
		if err != nil {
			fmt.Printf("%s", txJson)
		} else {
			fmt.Printf("%s", prettyJSON.Bytes())
		}

		return nil
	},
}

var transferContractsCmd = &cobra.Command{
	Use:   "transfer [contract_name] [contract_address] [to_address] [amount]",
	Short: "Transfer a contract tokens to an address",
	Args:  cobra.ExactArgs(4),
	RunE: func(cmd *cobra.Command, args []string) error {

		clientCtx, err := client.GetClientTxContext(cmd)
		if err != nil {
			return err
		}

		keyName := clientCtx.GetFromName()
		privateKey, err := getPrivateKeyFromCmd(cmd, keyName)
		if err != nil {
			return err
		}

		contractJsonFilePath := getCompiledContractPath(cmd, args[0])
		contractAddress := args[1]
		toAddress := args[2]

		amountUint, err := strconv.ParseInt(args[3], 10, 64)
		if err != nil {
			return err
		}
		amount := big.NewInt(amountUint)

		tx, err := contract.Transfer(contractJsonFilePath, contractAddress, privateKey, toAddress, amount, clientCtx.NodeURI)
		if err != nil {
			return err
		}

		fmt.Println("TX Hash: ", tx.Hash().Hex())
		txJson, err := tx.MarshalJSON()
		if err != nil {
			return err
		}

		var prettyJSON bytes.Buffer
		err = json.Indent(&prettyJSON, txJson, "", "  ")
		if err != nil {
			fmt.Printf("%s", txJson)
		} else {
			fmt.Printf("%s", prettyJSON.Bytes())
		}

		return nil
	},
}

func init() {

	_rootCmd.AddCommand(txContractsCmd)
	_rootCmd.AddCommand(transferContractsCmd)

	txContractsCmd.Flags().String(cosmosFlags.FlagFrom, "", "account address to sign the tx")
	transferContractsCmd.Flags().String(cosmosFlags.FlagFrom, "", "account address to sends funds from")
}
