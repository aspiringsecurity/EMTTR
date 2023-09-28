# fEVM deployment for OSS spreadsheet

We are extending the base 'HelloWorld' template available for testing the deployment pipeline for the fEVM. We leverage  the [foundry](https://github.com/foundry-rs/foundry) toolkit. To ascertain the contract is functioning as intended: 

```bash 
forge test --gas-report
```

You should see the following output: 

```bash 
| src/HelloWorld.sol:HelloWorld contract |                 |     |        |     |         |
|----------------------------------------|-----------------|-----|--------|-----|---------|
| Deployment Cost                        | Deployment Size |     |        |     |         |
| 45499                                  | 258             |     |        |     |         |
| Function Name                          | min             | avg | median | max | # calls |
| sayHelloWorld                          | 444             | 444 | 444    | 444 | 1       |

```

To deploy the contract we recommend using a ledger with the _Ethereum_ application [installed](https://support.ledger.com/hc/en-us/articles/4404382258961-Install-uninstall-and-update-apps?docs=true). To activate the address on the specific [network](https://docs.filecoin.io/networks/mainnet/details/) you're using, ensure the address has received FIL before and has FIL available to pay for deployment gas costs (either via a facuet for testnets or readl deal FIL for mainnet). 


We can then deploy the contract using 

```bash 
forge create HelloWorld --rpc-url=<INSERT NETWORK RPC URL> -l --retries 10
```



