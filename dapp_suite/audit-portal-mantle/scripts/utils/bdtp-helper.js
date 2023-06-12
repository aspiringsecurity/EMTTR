var bdtp_helper= {
    parsePointer: function (pointer){
        if(pointer == ""){
            return null
        }
    
        return {chain: pointer.substring(0,3), add: pointer.substring(3)}
    },
    parseInt: function (array) {
        var value = 0;
        for (var i = 0; i < array.length; i++) {
            value = (value * 256) + array[i];
        }
        return value;
    },
    stringToBytes: function(str){
        var bytes = []
    
        for (var i = 0; i < str.length; ++i) {
            var code = str.charCodeAt(i)
            bytes = bytes.concat([code])
        }
    
        return bytes
    }
};
