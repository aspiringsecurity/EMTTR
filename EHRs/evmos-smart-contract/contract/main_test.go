package contract_test

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
	"testing"

	sdk "github.com/cosmos/cosmos-sdk/types"
	"github.com/cosmos/cosmos-sdk/version"

	evmoscfg "github.com/evmos/evmos/v6/cmd/config"
	"github.com/mojtaba-esk/evmos-smart-contract/cmd"
)

const (
	dataDirFileContainerPath = "/tmp/evmos-test-data-dir" // The path to the data dir in /tmp is written into this file on build
	TestKey                  = "mykey"
	TestNodeURI              = "http://localhost:8545"
)

var TestDataDir = ""

func init() {

	// Get the data dir for the test running node in the tmp dir
	bytes, err := ioutil.ReadFile(dataDirFileContainerPath)
	if err != nil {
		panic(fmt.Errorf("dataDirFileContainerPath: %v", err))
	}

	TestDataDir = strings.Trim(string(bytes), "\r\n\t ")
	if TestDataDir == "" {
		panic(fmt.Errorf("dataDirFileContainerPath: the path is empty"))
	}

	config := sdk.GetConfig()
	evmoscfg.SetBech32Prefixes(config)
	version.Name = cmd.AppName
	config.Seal()

	cmd.GetClientContext() // We need it here to have the codecs configured properly for tests as well
}

func TestMain(m *testing.M) {
	// Disabling logs
	log.SetOutput(ioutil.Discard)

	os.Exit(m.Run())
}
