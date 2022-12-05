import requests
walletUrl = "http://localhost:8080"
# r = requests.get(walletUrl+"/testWallet")
# print(r.content)

# r = requests.get(walletUrl+"/tx/sendTransaction?accountToTransfer=0xffcf8fdee72ac11b5c542428b35eef5769c409f0")
# print(r.content)
def testWallet():
    r = requests.get(walletUrl+"/testWallet")
    return r.content.decode('utf8')

def sendTransaction(accountToSend):
    r = requests.get(walletUrl + "/tx/sendTransaction?accountToTransfer="+accountToSend)
    return r.content.decode('utf8')
def contractFunction(abi,contractAddress,contractFunction):
    r = requests.get(walletUrl+"/tx/contractFunction?abi="+abi+"&contractAddress="+contractAddress+"&contractFunction="+contractFunction)
    return r.content.decode('utf8')
def contractFunctionCall(abi,contractAddress,contractFunction):
    r = requests.get(walletUrl+"/tx/contractFunction/call?abi="+abi+"&contractAddress="+contractAddress+"&contractFunction="+contractFunction)
    return r.content.decode('utf8')