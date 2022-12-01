package cmd

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/cosmos/cosmos-sdk/client"
	cosmosFlags "github.com/cosmos/cosmos-sdk/client/flags"
	evmoskr "github.com/evmos/evmos/v6/crypto/keyring"

	// ethermintclient "github.com/evmos/ethermint/client"
	"github.com/spf13/cobra"
	"github.com/tendermint/tendermint/libs/cli"

	"github.com/evmos/ethermint/crypto/hd"
	"github.com/evmos/ethermint/encoding"
)

var _defaultNodeHome = os.ExpandEnv("$HOME/") + ".evmosd"

const (
	EnvPrefix                 = "EVMOS"
	AppName                   = "evmosd"
	FlagCompiledContractsPath = "compiled-contracts-path"
)

var _rootCmd = &cobra.Command{
	Use:   "evmos-smart-contract",
	Short: "Deploy and manage a smart contract on evmos",
}

func init() {

	_rootCmd.PersistentFlags().String(cosmosFlags.FlagKeyringBackend, "os", "Keyring backend to use, default value is: os")
	_rootCmd.PersistentFlags().String(cosmosFlags.FlagKeyringDir, _defaultNodeHome, "Keyring backend directory")
	_rootCmd.PersistentFlags().String(cli.OutputFlag, "text", "Output format (text|json)")
	_rootCmd.PersistentFlags().String(cosmosFlags.FlagKeyAlgorithm, string(hd.EthSecp256k1Type), "The algorithm used to generate the keys")
	_rootCmd.PersistentFlags().String(cosmosFlags.FlagNode, "http://localhost:8545", "The evmos node to connect to")
	_rootCmd.PersistentFlags().String(cosmosFlags.FlagChainID, "evmos_9000-1", "The evmos chain id")
	_rootCmd.PersistentFlags().String(FlagCompiledContractsPath, filepath.Join(GetRootPath(), "contracts", "compiled_contracts"), "The path to the compiled contracts in json format")
}

func Execute() {

	clientContext := GetClientContext()
	ctx := context.Background()
	ctx = context.WithValue(ctx, client.ClientContextKey, &clientContext)
	if err := _rootCmd.ExecuteContext(ctx); err != nil {
		_, _ = fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

// This function retrieves the root path of where the binary is being executed
func GetRootPath() string {
	dir, err := filepath.Abs(filepath.Dir(os.Args[0]))
	if err != nil {
		panic(err)
	}
	return dir
}

func GetClientContext() client.Context {
	encodingConfig := encoding.MakeConfig(nil)
	return client.Context{}.
		WithInput(os.Stdin).
		WithHomeDir(_defaultNodeHome).
		WithKeyringOptions(evmoskr.Option()).
		WithViper(EnvPrefix).
		WithLegacyAmino(encodingConfig.Amino)
}
