import { defineConfig } from '@dethcrypto/eth-sdk'

export default defineConfig({
   contracts: {
      rinkeby: {
         zoraTransferHelper: '0x029AA5a949C9C90916729D50537062cb73b5Ac92',
         zoraModuleManager: '0xa248736d3b73A231D95A5F99965857ebbBD42D85',
         zoraAsksV1_1Module: '0xA98D3729265C88c5b3f861a0c501622750fF4806',
         lostandFoundContract: '0x0E0e37De35471924F50598d55F7b69f93703fA01',
         lostandFoundContract2: '0x47686F3CE340bcb8609a5D65016d99B1299835e8',
         lostandFoundContract3: '0x288FC01ACcf7E053cD594AA18eff3e2D549600b7',
         lostandFoundContract4: '0xa4248aC1a4Fc557134802f39cddF830Fde6DdA06',
      },
      ropsten: {
         zeroExERC721OrdersFeature: "0x72657b338391c6A55120EB786a2b4FecED7D3Be2"
      },
      optimism: {
         zeroExERC721OrdersFeature: "0x0C58C1170f1DEd633862A1166f52107490a9C594"
      }
   },
})