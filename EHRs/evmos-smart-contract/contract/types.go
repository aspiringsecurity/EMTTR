package contract

import (
	"encoding/json"
	"fmt"
	"math/big"
	"reflect"
)

type CompiledContract struct {
	ABI  string `json:"abi"`
	Bin  string `json:"bin"`
	Name string `json:"contractName"`
}

const (
	METHOD_NAME       = "name"
	METHOD_BALANCE_OF = "balanceOf"
	METHOD_SYMBOL     = "symbol"
	METHOD_DECIMALS   = "decimals"
	METHOD_TRANSFER   = "transfer"
)

// This function parses the given json value for contract method's parameters
// It extracts the params in the same given order and converts the simple types
// such as numbers and strings
// Example: {"params":[51, "test msg"]}
func ParseJsonParams(paramsJson string) ([]interface{}, error) {

	if paramsJson == "" {
		return nil, nil
	}

	var params map[string]interface{}
	if err := json.Unmarshal([]byte(paramsJson), &params); err != nil {
		return nil, fmt.Errorf("unmarshaling json params: %v", err)
	}

	var paramsToPass []interface{}
	// Let's range over it to let user to enter any key name they want for params
	for i := range params {
		var ok bool
		if paramsToPass, ok = params[i].([]interface{}); !ok {
			return nil, fmt.Errorf("casting params: %v", params[i])
		}
		break
	}

	// Fix the numbers to make it compatible with int ptr
	for i, v := range paramsToPass {
		if reflect.TypeOf(v).Kind() == reflect.Float64 {
			numParam := &big.Int{}
			floatVal, ok := v.(float64)
			if !ok {
				return nil, fmt.Errorf("casting numeric param: %v", v)
			}
			numParam.SetInt64(int64(floatVal))
			paramsToPass[i] = numParam
		}
	}

	return paramsToPass, nil

}
