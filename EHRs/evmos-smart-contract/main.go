package main

import (
	sdk "github.com/cosmos/cosmos-sdk/types"
	"github.com/cosmos/cosmos-sdk/version"
	evmoscfg "github.com/evmos/evmos/v6/cmd/config"
	"github.com/mojtaba-esk/evmos-smart-contract/cmd"
)

func main() {
	config := sdk.GetConfig()
	evmoscfg.SetBech32Prefixes(config)
	version.Name = cmd.AppName
	config.Seal()

	cmd.Execute()
}
