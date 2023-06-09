// This tx is used for blocklister to blocklist a resource.
// This will fail: 
// - if the blocklister does not have delegated capability given by the BlocklistExecutor
// - if the resource has already been blocklisted

import FiatToken from 0x{{.FiatToken}}

transaction(resourceId: UInt64) {
    prepare (blocklister: AuthAccount) {
        let blocklister = blocklister.borrow<&FiatToken.Blocklister>(from: FiatToken.BlocklisterStoragePath) ?? panic("cannot borrow own private path")
        blocklister.blocklist(resourceId: resourceId);
    } 

    post {
        FiatToken.getBlocklist(resourceId:resourceId)! != nil: "Resource not blocklisted";
        FiatToken.getBlocklist(resourceId:resourceId) == getCurrentBlock().height : "Blocklisted on incorrect height";

    }
}
