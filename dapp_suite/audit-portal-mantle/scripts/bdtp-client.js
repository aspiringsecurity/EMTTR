/******Process bdtp chunk because google sockets uses 4096bytes chunks  */
var remainingBytes = []
var dataSize = -1
var readCount = 0

function computeHashAndDisplaybytes(bytes){

    h = base58.sha256(bytes).then(h => {
        $("#bdtp-data").append(`<span id="${h}" class="bdtp-block text-font">${bytes}</span>`)
        var string = "";
        bytes.forEach(c => string+=String.fromCharCode(c))
        $(`#${h}`).text(string)
    })
}

async function processChunk(chunk){
    var currentChunk = remainingBytes.concat(Array.from(chunk))

    var offset = 0
    for (var i = 0; i<30;i++){

        if(offset>currentChunk.length){
            break
        }

        if(currentChunk.length < offset + 140){
            if (readCount + 140 < dataSize){
                return currentChunk.slice(offset, currentChunk.length)
            }
            end = currentChunk.length
            
        }else{
            end = offset +140
        }
        var bytes = currentChunk.slice(offset, end)
        computeHashAndDisplaybytes(bytes)

        offset += 140
        readCount +=140
    }
   return []
}
function fetchAndDisplayBytes(data){
    disableFetchBtn()
    $("#bdtp-data").empty()
    
    //call bdtp
    processChunk(data).then(remaining => {
        remainingBytes = Array.from(remaining)
    })

    enableValidateBtn()
    return
}


/*****socket********/

$("#bdtp").click(async function(e){
    remainingBytes = []
    dataSize = -1
    readCount = 0
    
    if (chrome.sockets == undefined){
        console.log("google socket undefined")
        return
    }

    if ($("#pointer").val()==""){
        return
    }
    socket.connect()
})

var socket = {

    listenerIsInit: false,
    SOCKET_ID: 0,
    
    connect: function (){
        console.log("trying to set up socket...")
        chrome.sockets.tcp.create({}, function(createInfo) {
            this.SOCKET_ID = createInfo.socketId
            chrome.sockets.tcp.connect(this.SOCKET_ID,"localhost", 4444, socket.onAccept)
        })
        if(!this.listenerIsInit){
            chrome.sockets.tcp.onReceive.addListener(socket.handler);
            chrome.sockets.tcp.onReceiveError.addListener(e => console.log(e))
            this.listenerIsInit = true;
        }    
    },
    handler: function (info){
        if (info.socketId == this.SOCKET_ID){
            //4096bytes
            var data = new Uint8Array(info.data)
            if(dataSize === -1){
                var s = bdtp_helper.parseInt(data.slice(0,4))
                dataSize = s
                data = data.slice(4)
            }
    
            console.log("try to display bytes")
            fetchAndDisplayBytes(data)
            chrome.sockets.tcp.disconnect(info.socketId)        
        }  
    },
    onAccept: function(){    
        console.log("accepted")
        var ptrStr = bdtp_helper.parsePointer($("#pointer").val())
        if (ptrStr == null){
            return
        }
        ptrStr.add = base58.decode(ptrStr.add)
    
        buff = new ArrayBuffer(ptrStr.chain.length + ptrStr.add.length + 4)
        
        buffArr = new Uint8Array(buff)
        for  (i = 0; i< buffArr.length; i++){
            if (i<3){
                buffArr[i] = ptrStr.chain.charCodeAt(i)
            }
            if(i>=3 && i < 29){
              buffArr[i] = ptrStr.add[i-3]
            }
            if (i>= 29){
              buffArr[i] =0
            }
        }
    
        chrome.sockets.tcp.send(this.SOCKET_ID, buffArr, socket.listen)
    },
    listen: function (){
        console.log("ready to receive data")
        chrome.sockets.tcp.getInfo(this.SOCKET_ID, socket.handleDisconnect)
    },
    handleDisconnect: function (info){
        if(!info.connected){
            bdtpError("BDTP connection closed by peer")
        }
    }

}