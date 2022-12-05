//importing my own ABIs that I generated with eth-sdk

const ZEROEXERC721ORDERSFEATUREABI = [
    {
        inputs: [
            {
                internalType: "address",
                name: "zeroExAddress",
                type: "address",
            },
            {
                internalType: "contract IEtherTokenV06",
                name: "weth",
                type: "address",
            },
        ],
        stateMutability: "nonpayable",
        type: "constructor",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: false,
                internalType: "address",
                name: "maker",
                type: "address",
            },
            {
                indexed: false,
                internalType: "uint256",
                name: "nonce",
                type: "uint256",
            },
        ],
        name: "ERC721OrderCancelled",
        type: "event",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: false,
                internalType: "enum LibNFTOrder.TradeDirection",
                name: "direction",
                type: "uint8",
            },
            {
                indexed: false,
                internalType: "address",
                name: "maker",
                type: "address",
            },
            {
                indexed: false,
                internalType: "address",
                name: "taker",
                type: "address",
            },
            {
                indexed: false,
                internalType: "uint256",
                name: "nonce",
                type: "uint256",
            },
            {
                indexed: false,
                internalType: "contract IERC20TokenV06",
                name: "erc20Token",
                type: "address",
            },
            {
                indexed: false,
                internalType: "uint256",
                name: "erc20TokenAmount",
                type: "uint256",
            },
            {
                indexed: false,
                internalType: "contract IERC721Token",
                name: "erc721Token",
                type: "address",
            },
            {
                indexed: false,
                internalType: "uint256",
                name: "erc721TokenId",
                type: "uint256",
            },
            {
                indexed: false,
                internalType: "address",
                name: "matcher",
                type: "address",
            },
        ],
        name: "ERC721OrderFilled",
        type: "event",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: false,
                internalType: "enum LibNFTOrder.TradeDirection",
                name: "direction",
                type: "uint8",
            },
            {
                indexed: false,
                internalType: "address",
                name: "maker",
                type: "address",
            },
            {
                indexed: false,
                internalType: "address",
                name: "taker",
                type: "address",
            },
            {
                indexed: false,
                internalType: "uint256",
                name: "expiry",
                type: "uint256",
            },
            {
                indexed: false,
                internalType: "uint256",
                name: "nonce",
                type: "uint256",
            },
            {
                indexed: false,
                internalType: "contract IERC20TokenV06",
                name: "erc20Token",
                type: "address",
            },
            {
                indexed: false,
                internalType: "uint256",
                name: "erc20TokenAmount",
                type: "uint256",
            },
            {
                components: [
                    {
                        internalType: "address",
                        name: "recipient",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "amount",
                        type: "uint256",
                    },
                    {
                        internalType: "bytes",
                        name: "feeData",
                        type: "bytes",
                    },
                ],
                indexed: false,
                internalType: "struct LibNFTOrder.Fee[]",
                name: "fees",
                type: "tuple[]",
            },
            {
                indexed: false,
                internalType: "contract IERC721Token",
                name: "erc721Token",
                type: "address",
            },
            {
                indexed: false,
                internalType: "uint256",
                name: "erc721TokenId",
                type: "uint256",
            },
            {
                components: [
                    {
                        internalType: "contract IPropertyValidator",
                        name: "propertyValidator",
                        type: "address",
                    },
                    {
                        internalType: "bytes",
                        name: "propertyData",
                        type: "bytes",
                    },
                ],
                indexed: false,
                internalType: "struct LibNFTOrder.Property[]",
                name: "erc721TokenProperties",
                type: "tuple[]",
            },
        ],
        name: "ERC721OrderPreSigned",
        type: "event",
    },
    {
        inputs: [],
        name: "EIP712_DOMAIN_SEPARATOR",
        outputs: [
            {
                internalType: "bytes32",
                name: "",
                type: "bytes32",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "FEATURE_NAME",
        outputs: [
            {
                internalType: "string",
                name: "",
                type: "string",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "FEATURE_VERSION",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order",
                name: "sellOrder",
                type: "tuple",
            },
            {
                components: [
                    {
                        internalType: "enum LibSignature.SignatureType",
                        name: "signatureType",
                        type: "uint8",
                    },
                    {
                        internalType: "uint8",
                        name: "v",
                        type: "uint8",
                    },
                    {
                        internalType: "bytes32",
                        name: "r",
                        type: "bytes32",
                    },
                    {
                        internalType: "bytes32",
                        name: "s",
                        type: "bytes32",
                    },
                ],
                internalType: "struct LibSignature.Signature",
                name: "signature",
                type: "tuple",
            },
            {
                internalType: "uint256",
                name: "ethAvailable",
                type: "uint256",
            },
            {
                internalType: "bytes",
                name: "takerCallbackData",
                type: "bytes",
            },
        ],
        name: "_buyERC721",
        outputs: [],
        stateMutability: "payable",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order[]",
                name: "sellOrders",
                type: "tuple[]",
            },
            {
                components: [
                    {
                        internalType: "enum LibSignature.SignatureType",
                        name: "signatureType",
                        type: "uint8",
                    },
                    {
                        internalType: "uint8",
                        name: "v",
                        type: "uint8",
                    },
                    {
                        internalType: "bytes32",
                        name: "r",
                        type: "bytes32",
                    },
                    {
                        internalType: "bytes32",
                        name: "s",
                        type: "bytes32",
                    },
                ],
                internalType: "struct LibSignature.Signature[]",
                name: "signatures",
                type: "tuple[]",
            },
            {
                internalType: "bytes[]",
                name: "callbackData",
                type: "bytes[]",
            },
            {
                internalType: "bool",
                name: "revertIfIncomplete",
                type: "bool",
            },
        ],
        name: "batchBuyERC721s",
        outputs: [
            {
                internalType: "bool[]",
                name: "successes",
                type: "bool[]",
            },
        ],
        stateMutability: "payable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256[]",
                name: "orderNonces",
                type: "uint256[]",
            },
        ],
        name: "batchCancelERC721Orders",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order[]",
                name: "sellOrders",
                type: "tuple[]",
            },
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order[]",
                name: "buyOrders",
                type: "tuple[]",
            },
            {
                components: [
                    {
                        internalType: "enum LibSignature.SignatureType",
                        name: "signatureType",
                        type: "uint8",
                    },
                    {
                        internalType: "uint8",
                        name: "v",
                        type: "uint8",
                    },
                    {
                        internalType: "bytes32",
                        name: "r",
                        type: "bytes32",
                    },
                    {
                        internalType: "bytes32",
                        name: "s",
                        type: "bytes32",
                    },
                ],
                internalType: "struct LibSignature.Signature[]",
                name: "sellOrderSignatures",
                type: "tuple[]",
            },
            {
                components: [
                    {
                        internalType: "enum LibSignature.SignatureType",
                        name: "signatureType",
                        type: "uint8",
                    },
                    {
                        internalType: "uint8",
                        name: "v",
                        type: "uint8",
                    },
                    {
                        internalType: "bytes32",
                        name: "r",
                        type: "bytes32",
                    },
                    {
                        internalType: "bytes32",
                        name: "s",
                        type: "bytes32",
                    },
                ],
                internalType: "struct LibSignature.Signature[]",
                name: "buyOrderSignatures",
                type: "tuple[]",
            },
        ],
        name: "batchMatchERC721Orders",
        outputs: [
            {
                internalType: "uint256[]",
                name: "profits",
                type: "uint256[]",
            },
            {
                internalType: "bool[]",
                name: "successes",
                type: "bool[]",
            },
        ],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order",
                name: "sellOrder",
                type: "tuple",
            },
            {
                components: [
                    {
                        internalType: "enum LibSignature.SignatureType",
                        name: "signatureType",
                        type: "uint8",
                    },
                    {
                        internalType: "uint8",
                        name: "v",
                        type: "uint8",
                    },
                    {
                        internalType: "bytes32",
                        name: "r",
                        type: "bytes32",
                    },
                    {
                        internalType: "bytes32",
                        name: "s",
                        type: "bytes32",
                    },
                ],
                internalType: "struct LibSignature.Signature",
                name: "signature",
                type: "tuple",
            },
            {
                internalType: "bytes",
                name: "callbackData",
                type: "bytes",
            },
        ],
        name: "buyERC721",
        outputs: [],
        stateMutability: "payable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "orderNonce",
                type: "uint256",
            },
        ],
        name: "cancelERC721Order",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order",
                name: "order",
                type: "tuple",
            },
        ],
        name: "getERC721OrderHash",
        outputs: [
            {
                internalType: "bytes32",
                name: "orderHash",
                type: "bytes32",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order",
                name: "order",
                type: "tuple",
            },
        ],
        name: "getERC721OrderStatus",
        outputs: [
            {
                internalType: "enum LibNFTOrder.OrderStatus",
                name: "status",
                type: "uint8",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "maker",
                type: "address",
            },
            {
                internalType: "uint248",
                name: "nonceRange",
                type: "uint248",
            },
        ],
        name: "getERC721OrderStatusBitVector",
        outputs: [
            {
                internalType: "uint256",
                name: "bitVector",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order",
                name: "sellOrder",
                type: "tuple",
            },
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order",
                name: "buyOrder",
                type: "tuple",
            },
            {
                components: [
                    {
                        internalType: "enum LibSignature.SignatureType",
                        name: "signatureType",
                        type: "uint8",
                    },
                    {
                        internalType: "uint8",
                        name: "v",
                        type: "uint8",
                    },
                    {
                        internalType: "bytes32",
                        name: "r",
                        type: "bytes32",
                    },
                    {
                        internalType: "bytes32",
                        name: "s",
                        type: "bytes32",
                    },
                ],
                internalType: "struct LibSignature.Signature",
                name: "sellOrderSignature",
                type: "tuple",
            },
            {
                components: [
                    {
                        internalType: "enum LibSignature.SignatureType",
                        name: "signatureType",
                        type: "uint8",
                    },
                    {
                        internalType: "uint8",
                        name: "v",
                        type: "uint8",
                    },
                    {
                        internalType: "bytes32",
                        name: "r",
                        type: "bytes32",
                    },
                    {
                        internalType: "bytes32",
                        name: "s",
                        type: "bytes32",
                    },
                ],
                internalType: "struct LibSignature.Signature",
                name: "buyOrderSignature",
                type: "tuple",
            },
        ],
        name: "matchERC721Orders",
        outputs: [
            {
                internalType: "uint256",
                name: "profit",
                type: "uint256",
            },
        ],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [],
        name: "migrate",
        outputs: [
            {
                internalType: "bytes4",
                name: "success",
                type: "bytes4",
            },
        ],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "operator",
                type: "address",
            },
            {
                internalType: "address",
                name: "",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
            {
                internalType: "bytes",
                name: "data",
                type: "bytes",
            },
        ],
        name: "onERC721Received",
        outputs: [
            {
                internalType: "bytes4",
                name: "success",
                type: "bytes4",
            },
        ],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order",
                name: "order",
                type: "tuple",
            },
        ],
        name: "preSignERC721Order",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order",
                name: "buyOrder",
                type: "tuple",
            },
            {
                components: [
                    {
                        internalType: "enum LibSignature.SignatureType",
                        name: "signatureType",
                        type: "uint8",
                    },
                    {
                        internalType: "uint8",
                        name: "v",
                        type: "uint8",
                    },
                    {
                        internalType: "bytes32",
                        name: "r",
                        type: "bytes32",
                    },
                    {
                        internalType: "bytes32",
                        name: "s",
                        type: "bytes32",
                    },
                ],
                internalType: "struct LibSignature.Signature",
                name: "signature",
                type: "tuple",
            },
            {
                internalType: "uint256",
                name: "erc721TokenId",
                type: "uint256",
            },
            {
                internalType: "bool",
                name: "unwrapNativeToken",
                type: "bool",
            },
            {
                internalType: "bytes",
                name: "callbackData",
                type: "bytes",
            },
        ],
        name: "sellERC721",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order",
                name: "order",
                type: "tuple",
            },
            {
                internalType: "uint256",
                name: "erc721TokenId",
                type: "uint256",
            },
        ],
        name: "validateERC721OrderProperties",
        outputs: [],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                components: [
                    {
                        internalType: "enum LibNFTOrder.TradeDirection",
                        name: "direction",
                        type: "uint8",
                    },
                    {
                        internalType: "address",
                        name: "maker",
                        type: "address",
                    },
                    {
                        internalType: "address",
                        name: "taker",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "expiry",
                        type: "uint256",
                    },
                    {
                        internalType: "uint256",
                        name: "nonce",
                        type: "uint256",
                    },
                    {
                        internalType: "contract IERC20TokenV06",
                        name: "erc20Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc20TokenAmount",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "address",
                                name: "recipient",
                                type: "address",
                            },
                            {
                                internalType: "uint256",
                                name: "amount",
                                type: "uint256",
                            },
                            {
                                internalType: "bytes",
                                name: "feeData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Fee[]",
                        name: "fees",
                        type: "tuple[]",
                    },
                    {
                        internalType: "contract IERC721Token",
                        name: "erc721Token",
                        type: "address",
                    },
                    {
                        internalType: "uint256",
                        name: "erc721TokenId",
                        type: "uint256",
                    },
                    {
                        components: [
                            {
                                internalType: "contract IPropertyValidator",
                                name: "propertyValidator",
                                type: "address",
                            },
                            {
                                internalType: "bytes",
                                name: "propertyData",
                                type: "bytes",
                            },
                        ],
                        internalType: "struct LibNFTOrder.Property[]",
                        name: "erc721TokenProperties",
                        type: "tuple[]",
                    },
                ],
                internalType: "struct LibNFTOrder.ERC721Order",
                name: "order",
                type: "tuple",
            },
            {
                components: [
                    {
                        internalType: "enum LibSignature.SignatureType",
                        name: "signatureType",
                        type: "uint8",
                    },
                    {
                        internalType: "uint8",
                        name: "v",
                        type: "uint8",
                    },
                    {
                        internalType: "bytes32",
                        name: "r",
                        type: "bytes32",
                    },
                    {
                        internalType: "bytes32",
                        name: "s",
                        type: "bytes32",
                    },
                ],
                internalType: "struct LibSignature.Signature",
                name: "signature",
                type: "tuple",
            },
        ],
        name: "validateERC721OrderSignature",
        outputs: [],
        stateMutability: "view",
        type: "function",
    },
];

const LOSTANDFOUNDCONTRACT4ABI = [
    {
        inputs: [
            {
                internalType: "string",
                name: "customBaseURI_",
                type: "string",
            },
        ],
        stateMutability: "nonpayable",
        type: "constructor",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: true,
                internalType: "address",
                name: "owner",
                type: "address",
            },
            {
                indexed: true,
                internalType: "address",
                name: "approved",
                type: "address",
            },
            {
                indexed: true,
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "Approval",
        type: "event",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: true,
                internalType: "address",
                name: "owner",
                type: "address",
            },
            {
                indexed: true,
                internalType: "address",
                name: "operator",
                type: "address",
            },
            {
                indexed: false,
                internalType: "bool",
                name: "approved",
                type: "bool",
            },
        ],
        name: "ApprovalForAll",
        type: "event",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: true,
                internalType: "address",
                name: "previousOwner",
                type: "address",
            },
            {
                indexed: true,
                internalType: "address",
                name: "newOwner",
                type: "address",
            },
        ],
        name: "OwnershipTransferred",
        type: "event",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: true,
                internalType: "address",
                name: "from",
                type: "address",
            },
            {
                indexed: true,
                internalType: "address",
                name: "to",
                type: "address",
            },
            {
                indexed: true,
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "Transfer",
        type: "event",
    },
    {
        inputs: [],
        name: "MAX_MULTIMINT",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "MAX_SUPPLY",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "MINT_LIMIT_PER_WALLET",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "PRICE",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "minter",
                type: "address",
            },
        ],
        name: "allowedMintCount",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "to",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "approve",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "owner",
                type: "address",
            },
        ],
        name: "balanceOf",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "contractURI",
        outputs: [
            {
                internalType: "string",
                name: "",
                type: "string",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "getApproved",
        outputs: [
            {
                internalType: "address",
                name: "",
                type: "address",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "owner",
                type: "address",
            },
            {
                internalType: "address",
                name: "operator",
                type: "address",
            },
        ],
        name: "isApprovedForAll",
        outputs: [
            {
                internalType: "bool",
                name: "",
                type: "bool",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "count",
                type: "uint256",
            },
        ],
        name: "mint",
        outputs: [],
        stateMutability: "payable",
        type: "function",
    },
    {
        inputs: [],
        name: "name",
        outputs: [
            {
                internalType: "string",
                name: "",
                type: "string",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "owner",
        outputs: [
            {
                internalType: "address",
                name: "",
                type: "address",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "ownerOf",
        outputs: [
            {
                internalType: "address",
                name: "",
                type: "address",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "renounceOwnership",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
            {
                internalType: "uint256",
                name: "salePrice",
                type: "uint256",
            },
        ],
        name: "royaltyInfo",
        outputs: [
            {
                internalType: "address",
                name: "receiver",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "royaltyAmount",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "from",
                type: "address",
            },
            {
                internalType: "address",
                name: "to",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "safeTransferFrom",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "from",
                type: "address",
            },
            {
                internalType: "address",
                name: "to",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
            {
                internalType: "bytes",
                name: "_data",
                type: "bytes",
            },
        ],
        name: "safeTransferFrom",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "operator",
                type: "address",
            },
            {
                internalType: "bool",
                name: "approved",
                type: "bool",
            },
        ],
        name: "setApprovalForAll",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "string",
                name: "customBaseURI_",
                type: "string",
            },
        ],
        name: "setBaseURI",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "string",
                name: "customContractURI_",
                type: "string",
            },
        ],
        name: "setContractURI",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "bytes4",
                name: "interfaceId",
                type: "bytes4",
            },
        ],
        name: "supportsInterface",
        outputs: [
            {
                internalType: "bool",
                name: "",
                type: "bool",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "symbol",
        outputs: [
            {
                internalType: "string",
                name: "",
                type: "string",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "tokenURI",
        outputs: [
            {
                internalType: "string",
                name: "",
                type: "string",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "totalSupply",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "from",
                type: "address",
            },
            {
                internalType: "address",
                name: "to",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "transferFrom",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "newOwner",
                type: "address",
            },
        ],
        name: "transferOwnership",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [],
        name: "withdraw",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "contract IERC20",
                name: "token",
                type: "address",
            },
        ],
        name: "withdrawTokens",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        stateMutability: "payable",
        type: "receive",
    },
];

const LOSTANDFOUNDCONTRACT3ABI = [
    {
        inputs: [
            {
                internalType: "string",
                name: "customBaseURI_",
                type: "string",
            },
        ],
        stateMutability: "nonpayable",
        type: "constructor",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: true,
                internalType: "address",
                name: "owner",
                type: "address",
            },
            {
                indexed: true,
                internalType: "address",
                name: "approved",
                type: "address",
            },
            {
                indexed: true,
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "Approval",
        type: "event",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: true,
                internalType: "address",
                name: "owner",
                type: "address",
            },
            {
                indexed: true,
                internalType: "address",
                name: "operator",
                type: "address",
            },
            {
                indexed: false,
                internalType: "bool",
                name: "approved",
                type: "bool",
            },
        ],
        name: "ApprovalForAll",
        type: "event",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: true,
                internalType: "address",
                name: "previousOwner",
                type: "address",
            },
            {
                indexed: true,
                internalType: "address",
                name: "newOwner",
                type: "address",
            },
        ],
        name: "OwnershipTransferred",
        type: "event",
    },
    {
        anonymous: false,
        inputs: [
            {
                indexed: true,
                internalType: "address",
                name: "from",
                type: "address",
            },
            {
                indexed: true,
                internalType: "address",
                name: "to",
                type: "address",
            },
            {
                indexed: true,
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "Transfer",
        type: "event",
    },
    {
        inputs: [],
        name: "MAX_MULTIMINT",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "MAX_SUPPLY",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "MINT_LIMIT_PER_WALLET",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "PRICE",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "minter",
                type: "address",
            },
        ],
        name: "allowedMintCount",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "to",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "approve",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "owner",
                type: "address",
            },
        ],
        name: "balanceOf",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "contractURI",
        outputs: [
            {
                internalType: "string",
                name: "",
                type: "string",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "getApproved",
        outputs: [
            {
                internalType: "address",
                name: "",
                type: "address",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "owner",
                type: "address",
            },
            {
                internalType: "address",
                name: "operator",
                type: "address",
            },
        ],
        name: "isApprovedForAll",
        outputs: [
            {
                internalType: "bool",
                name: "",
                type: "bool",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "count",
                type: "uint256",
            },
        ],
        name: "mint",
        outputs: [],
        stateMutability: "payable",
        type: "function",
    },
    {
        inputs: [],
        name: "name",
        outputs: [
            {
                internalType: "string",
                name: "",
                type: "string",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "owner",
        outputs: [
            {
                internalType: "address",
                name: "",
                type: "address",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "ownerOf",
        outputs: [
            {
                internalType: "address",
                name: "",
                type: "address",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "renounceOwnership",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
            {
                internalType: "uint256",
                name: "salePrice",
                type: "uint256",
            },
        ],
        name: "royaltyInfo",
        outputs: [
            {
                internalType: "address",
                name: "receiver",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "royaltyAmount",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "from",
                type: "address",
            },
            {
                internalType: "address",
                name: "to",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "safeTransferFrom",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "from",
                type: "address",
            },
            {
                internalType: "address",
                name: "to",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
            {
                internalType: "bytes",
                name: "_data",
                type: "bytes",
            },
        ],
        name: "safeTransferFrom",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "operator",
                type: "address",
            },
            {
                internalType: "bool",
                name: "approved",
                type: "bool",
            },
        ],
        name: "setApprovalForAll",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "string",
                name: "customBaseURI_",
                type: "string",
            },
        ],
        name: "setBaseURI",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "string",
                name: "customContractURI_",
                type: "string",
            },
        ],
        name: "setContractURI",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "bytes4",
                name: "interfaceId",
                type: "bytes4",
            },
        ],
        name: "supportsInterface",
        outputs: [
            {
                internalType: "bool",
                name: "",
                type: "bool",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "symbol",
        outputs: [
            {
                internalType: "string",
                name: "",
                type: "string",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "tokenURI",
        outputs: [
            {
                internalType: "string",
                name: "",
                type: "string",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [],
        name: "totalSupply",
        outputs: [
            {
                internalType: "uint256",
                name: "",
                type: "uint256",
            },
        ],
        stateMutability: "view",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "from",
                type: "address",
            },
            {
                internalType: "address",
                name: "to",
                type: "address",
            },
            {
                internalType: "uint256",
                name: "tokenId",
                type: "uint256",
            },
        ],
        name: "transferFrom",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "address",
                name: "newOwner",
                type: "address",
            },
        ],
        name: "transferOwnership",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [],
        name: "withdraw",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        inputs: [
            {
                internalType: "contract IERC20",
                name: "token",
                type: "address",
            },
        ],
        name: "withdrawTokens",
        outputs: [],
        stateMutability: "nonpayable",
        type: "function",
    },
    {
        stateMutability: "payable",
        type: "receive",
    },
];

const LOSTANDFOUNDCONTRACT2ABI = [
  {
    inputs: [
        {
            internalType: "string",
            name: "customBaseURI_",
            type: "string",
        },
    ],
    stateMutability: "nonpayable",
    type: "constructor",
},
{
    anonymous: false,
    inputs: [
        {
            indexed: true,
            internalType: "address",
            name: "owner",
            type: "address",
        },
        {
            indexed: true,
            internalType: "address",
            name: "approved",
            type: "address",
        },
        {
            indexed: true,
            internalType: "uint256",
            name: "tokenId",
            type: "uint256",
        },
    ],
    name: "Approval",
    type: "event",
},
{
    anonymous: false,
    inputs: [
        {
            indexed: true,
            internalType: "address",
            name: "owner",
            type: "address",
        },
        {
            indexed: true,
            internalType: "address",
            name: "operator",
            type: "address",
        },
        {
            indexed: false,
            internalType: "bool",
            name: "approved",
            type: "bool",
        },
    ],
    name: "ApprovalForAll",
    type: "event",
},
{
    anonymous: false,
    inputs: [
        {
            indexed: true,
            internalType: "address",
            name: "previousOwner",
            type: "address",
        },
        {
            indexed: true,
            internalType: "address",
            name: "newOwner",
            type: "address",
        },
    ],
    name: "OwnershipTransferred",
    type: "event",
},
{
    anonymous: false,
    inputs: [
        {
            indexed: true,
            internalType: "address",
            name: "from",
            type: "address",
        },
        {
            indexed: true,
            internalType: "address",
            name: "to",
            type: "address",
        },
        {
            indexed: true,
            internalType: "uint256",
            name: "tokenId",
            type: "uint256",
        },
    ],
    name: "Transfer",
    type: "event",
},
{
    inputs: [],
    name: "MAX_MULTIMINT",
    outputs: [
        {
            internalType: "uint256",
            name: "",
            type: "uint256",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [],
    name: "MAX_SUPPLY",
    outputs: [
        {
            internalType: "uint256",
            name: "",
            type: "uint256",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [],
    name: "MINT_LIMIT_PER_WALLET",
    outputs: [
        {
            internalType: "uint256",
            name: "",
            type: "uint256",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [],
    name: "PRICE",
    outputs: [
        {
            internalType: "uint256",
            name: "",
            type: "uint256",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [
        {
            internalType: "address",
            name: "minter",
            type: "address",
        },
    ],
    name: "allowedMintCount",
    outputs: [
        {
            internalType: "uint256",
            name: "",
            type: "uint256",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [
        {
            internalType: "address",
            name: "to",
            type: "address",
        },
        {
            internalType: "uint256",
            name: "tokenId",
            type: "uint256",
        },
    ],
    name: "approve",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    inputs: [
        {
            internalType: "address",
            name: "owner",
            type: "address",
        },
    ],
    name: "balanceOf",
    outputs: [
        {
            internalType: "uint256",
            name: "",
            type: "uint256",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [],
    name: "contractURI",
    outputs: [
        {
            internalType: "string",
            name: "",
            type: "string",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [
        {
            internalType: "uint256",
            name: "tokenId",
            type: "uint256",
        },
    ],
    name: "getApproved",
    outputs: [
        {
            internalType: "address",
            name: "",
            type: "address",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [
        {
            internalType: "address",
            name: "owner",
            type: "address",
        },
        {
            internalType: "address",
            name: "operator",
            type: "address",
        },
    ],
    name: "isApprovedForAll",
    outputs: [
        {
            internalType: "bool",
            name: "",
            type: "bool",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [
        {
            internalType: "uint256",
            name: "count",
            type: "uint256",
        },
    ],
    name: "mint",
    outputs: [],
    stateMutability: "payable",
    type: "function",
},
{
    inputs: [],
    name: "name",
    outputs: [
        {
            internalType: "string",
            name: "",
            type: "string",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [],
    name: "owner",
    outputs: [
        {
            internalType: "address",
            name: "",
            type: "address",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [
        {
            internalType: "uint256",
            name: "tokenId",
            type: "uint256",
        },
    ],
    name: "ownerOf",
    outputs: [
        {
            internalType: "address",
            name: "",
            type: "address",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [],
    name: "renounceOwnership",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    inputs: [
        {
            internalType: "uint256",
            name: "",
            type: "uint256",
        },
        {
            internalType: "uint256",
            name: "salePrice",
            type: "uint256",
        },
    ],
    name: "royaltyInfo",
    outputs: [
        {
            internalType: "address",
            name: "receiver",
            type: "address",
        },
        {
            internalType: "uint256",
            name: "royaltyAmount",
            type: "uint256",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [
        {
            internalType: "address",
            name: "from",
            type: "address",
        },
        {
            internalType: "address",
            name: "to",
            type: "address",
        },
        {
            internalType: "uint256",
            name: "tokenId",
            type: "uint256",
        },
    ],
    name: "safeTransferFrom",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    inputs: [
        {
            internalType: "address",
            name: "from",
            type: "address",
        },
        {
            internalType: "address",
            name: "to",
            type: "address",
        },
        {
            internalType: "uint256",
            name: "tokenId",
            type: "uint256",
        },
        {
            internalType: "bytes",
            name: "_data",
            type: "bytes",
        },
    ],
    name: "safeTransferFrom",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    inputs: [
        {
            internalType: "address",
            name: "operator",
            type: "address",
        },
        {
            internalType: "bool",
            name: "approved",
            type: "bool",
        },
    ],
    name: "setApprovalForAll",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    inputs: [
        {
            internalType: "string",
            name: "customBaseURI_",
            type: "string",
        },
    ],
    name: "setBaseURI",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    inputs: [
        {
            internalType: "string",
            name: "customContractURI_",
            type: "string",
        },
    ],
    name: "setContractURI",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    inputs: [
        {
            internalType: "bytes4",
            name: "interfaceId",
            type: "bytes4",
        },
    ],
    name: "supportsInterface",
    outputs: [
        {
            internalType: "bool",
            name: "",
            type: "bool",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [],
    name: "symbol",
    outputs: [
        {
            internalType: "string",
            name: "",
            type: "string",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [
        {
            internalType: "uint256",
            name: "tokenId",
            type: "uint256",
        },
    ],
    name: "tokenURI",
    outputs: [
        {
            internalType: "string",
            name: "",
            type: "string",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [],
    name: "totalSupply",
    outputs: [
        {
            internalType: "uint256",
            name: "",
            type: "uint256",
        },
    ],
    stateMutability: "view",
    type: "function",
},
{
    inputs: [
        {
            internalType: "address",
            name: "from",
            type: "address",
        },
        {
            internalType: "address",
            name: "to",
            type: "address",
        },
        {
            internalType: "uint256",
            name: "tokenId",
            type: "uint256",
        },
    ],
    name: "transferFrom",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    inputs: [
        {
            internalType: "address",
            name: "newOwner",
            type: "address",
        },
    ],
    name: "transferOwnership",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    inputs: [],
    name: "withdraw",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    inputs: [
        {
            internalType: "contract IERC20",
            name: "token",
            type: "address",
        },
    ],
    name: "withdrawTokens",
    outputs: [],
    stateMutability: "nonpayable",
    type: "function",
},
{
    stateMutability: "payable",
    type: "receive",
},


]


const LOSTANDFOUNDCONTRACTABI = [
  {
      inputs: [
          {
              internalType: "string",
              name: "customBaseURI_",
              type: "string",
          },
      ],
      stateMutability: "nonpayable",
      type: "constructor",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "owner",
              type: "address",
          },
          {
              indexed: true,
              internalType: "address",
              name: "approved",
              type: "address",
          },
          {
              indexed: true,
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
      ],
      name: "Approval",
      type: "event",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "owner",
              type: "address",
          },
          {
              indexed: true,
              internalType: "address",
              name: "operator",
              type: "address",
          },
          {
              indexed: false,
              internalType: "bool",
              name: "approved",
              type: "bool",
          },
      ],
      name: "ApprovalForAll",
      type: "event",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "previousOwner",
              type: "address",
          },
          {
              indexed: true,
              internalType: "address",
              name: "newOwner",
              type: "address",
          },
      ],
      name: "OwnershipTransferred",
      type: "event",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "from",
              type: "address",
          },
          {
              indexed: true,
              internalType: "address",
              name: "to",
              type: "address",
          },
          {
              indexed: true,
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
      ],
      name: "Transfer",
      type: "event",
  },
  {
      inputs: [],
      name: "MAX_MULTIMINT",
      outputs: [
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "MAX_SUPPLY",
      outputs: [
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "MINT_LIMIT_PER_WALLET",
      outputs: [
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "PRICE",
      outputs: [
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "minter",
              type: "address",
          },
      ],
      name: "allowedMintCount",
      outputs: [
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "to",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
      ],
      name: "approve",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "owner",
              type: "address",
          },
      ],
      name: "balanceOf",
      outputs: [
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "contractURI",
      outputs: [
          {
              internalType: "string",
              name: "",
              type: "string",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
      ],
      name: "getApproved",
      outputs: [
          {
              internalType: "address",
              name: "",
              type: "address",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "owner",
              type: "address",
          },
          {
              internalType: "address",
              name: "operator",
              type: "address",
          },
      ],
      name: "isApprovedForAll",
      outputs: [
          {
              internalType: "bool",
              name: "",
              type: "bool",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "uint256",
              name: "count",
              type: "uint256",
          },
      ],
      name: "mint",
      outputs: [],
      stateMutability: "payable",
      type: "function",
  },
  {
      inputs: [],
      name: "name",
      outputs: [
          {
              internalType: "string",
              name: "",
              type: "string",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "owner",
      outputs: [
          {
              internalType: "address",
              name: "",
              type: "address",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
      ],
      name: "ownerOf",
      outputs: [
          {
              internalType: "address",
              name: "",
              type: "address",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "renounceOwnership",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
          {
              internalType: "uint256",
              name: "salePrice",
              type: "uint256",
          },
      ],
      name: "royaltyInfo",
      outputs: [
          {
              internalType: "address",
              name: "receiver",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "royaltyAmount",
              type: "uint256",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "from",
              type: "address",
          },
          {
              internalType: "address",
              name: "to",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
      ],
      name: "safeTransferFrom",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "from",
              type: "address",
          },
          {
              internalType: "address",
              name: "to",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
          {
              internalType: "bytes",
              name: "_data",
              type: "bytes",
          },
      ],
      name: "safeTransferFrom",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "operator",
              type: "address",
          },
          {
              internalType: "bool",
              name: "approved",
              type: "bool",
          },
      ],
      name: "setApprovalForAll",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "string",
              name: "customBaseURI_",
              type: "string",
          },
      ],
      name: "setBaseURI",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "string",
              name: "customContractURI_",
              type: "string",
          },
      ],
      name: "setContractURI",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "bytes4",
              name: "interfaceId",
              type: "bytes4",
          },
      ],
      name: "supportsInterface",
      outputs: [
          {
              internalType: "bool",
              name: "",
              type: "bool",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "symbol",
      outputs: [
          {
              internalType: "string",
              name: "",
              type: "string",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
      ],
      name: "tokenURI",
      outputs: [
          {
              internalType: "string",
              name: "",
              type: "string",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "totalSupply",
      outputs: [
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "from",
              type: "address",
          },
          {
              internalType: "address",
              name: "to",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
      ],
      name: "transferFrom",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "newOwner",
              type: "address",
          },
      ],
      name: "transferOwnership",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [],
      name: "withdraw",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
];

const ZORAASKSV1_1MODULEABI = [
  {
      inputs: [
          {
              internalType: "address",
              name: "_erc20TransferHelper",
              type: "address",
          },
          {
              internalType: "address",
              name: "_erc721TransferHelper",
              type: "address",
          },
          {
              internalType: "address",
              name: "_royaltyEngine",
              type: "address",
          },
          {
              internalType: "address",
              name: "_protocolFeeSettings",
              type: "address",
          },
          {
              internalType: "address",
              name: "_wethAddress",
              type: "address",
          },
      ],
      stateMutability: "nonpayable",
      type: "constructor",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "tokenContract",
              type: "address",
          },
          {
              indexed: true,
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
          {
              components: [
                  {
                      internalType: "address",
                      name: "seller",
                      type: "address",
                  },
                  {
                      internalType: "address",
                      name: "sellerFundsRecipient",
                      type: "address",
                  },
                  {
                      internalType: "address",
                      name: "askCurrency",
                      type: "address",
                  },
                  {
                      internalType: "uint16",
                      name: "findersFeeBps",
                      type: "uint16",
                  },
                  {
                      internalType: "uint256",
                      name: "askPrice",
                      type: "uint256",
                  },
              ],
              indexed: false,
              internalType: "struct AsksV1_1.Ask",
              name: "ask",
              type: "tuple",
          },
      ],
      name: "AskCanceled",
      type: "event",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "tokenContract",
              type: "address",
          },
          {
              indexed: true,
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
          {
              components: [
                  {
                      internalType: "address",
                      name: "seller",
                      type: "address",
                  },
                  {
                      internalType: "address",
                      name: "sellerFundsRecipient",
                      type: "address",
                  },
                  {
                      internalType: "address",
                      name: "askCurrency",
                      type: "address",
                  },
                  {
                      internalType: "uint16",
                      name: "findersFeeBps",
                      type: "uint16",
                  },
                  {
                      internalType: "uint256",
                      name: "askPrice",
                      type: "uint256",
                  },
              ],
              indexed: false,
              internalType: "struct AsksV1_1.Ask",
              name: "ask",
              type: "tuple",
          },
      ],
      name: "AskCreated",
      type: "event",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "tokenContract",
              type: "address",
          },
          {
              indexed: true,
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
          {
              indexed: true,
              internalType: "address",
              name: "buyer",
              type: "address",
          },
          {
              indexed: false,
              internalType: "address",
              name: "finder",
              type: "address",
          },
          {
              components: [
                  {
                      internalType: "address",
                      name: "seller",
                      type: "address",
                  },
                  {
                      internalType: "address",
                      name: "sellerFundsRecipient",
                      type: "address",
                  },
                  {
                      internalType: "address",
                      name: "askCurrency",
                      type: "address",
                  },
                  {
                      internalType: "uint16",
                      name: "findersFeeBps",
                      type: "uint16",
                  },
                  {
                      internalType: "uint256",
                      name: "askPrice",
                      type: "uint256",
                  },
              ],
              indexed: false,
              internalType: "struct AsksV1_1.Ask",
              name: "ask",
              type: "tuple",
          },
      ],
      name: "AskFilled",
      type: "event",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "tokenContract",
              type: "address",
          },
          {
              indexed: true,
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
          {
              components: [
                  {
                      internalType: "address",
                      name: "seller",
                      type: "address",
                  },
                  {
                      internalType: "address",
                      name: "sellerFundsRecipient",
                      type: "address",
                  },
                  {
                      internalType: "address",
                      name: "askCurrency",
                      type: "address",
                  },
                  {
                      internalType: "uint16",
                      name: "findersFeeBps",
                      type: "uint16",
                  },
                  {
                      internalType: "uint256",
                      name: "askPrice",
                      type: "uint256",
                  },
              ],
              indexed: false,
              internalType: "struct AsksV1_1.Ask",
              name: "ask",
              type: "tuple",
          },
      ],
      name: "AskPriceUpdated",
      type: "event",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "userA",
              type: "address",
          },
          {
              indexed: true,
              internalType: "address",
              name: "userB",
              type: "address",
          },
          {
              components: [
                  {
                      internalType: "address",
                      name: "tokenContract",
                      type: "address",
                  },
                  {
                      internalType: "uint256",
                      name: "tokenId",
                      type: "uint256",
                  },
                  {
                      internalType: "uint256",
                      name: "amount",
                      type: "uint256",
                  },
              ],
              indexed: false,
              internalType: "struct UniversalExchangeEventV1.ExchangeDetails",
              name: "a",
              type: "tuple",
          },
          {
              components: [
                  {
                      internalType: "address",
                      name: "tokenContract",
                      type: "address",
                  },
                  {
                      internalType: "uint256",
                      name: "tokenId",
                      type: "uint256",
                  },
                  {
                      internalType: "uint256",
                      name: "amount",
                      type: "uint256",
                  },
              ],
              indexed: false,
              internalType: "struct UniversalExchangeEventV1.ExchangeDetails",
              name: "b",
              type: "tuple",
          },
      ],
      name: "ExchangeExecuted",
      type: "event",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "tokenContract",
              type: "address",
          },
          {
              indexed: true,
              internalType: "uint256",
              name: "tokenId",
              type: "uint256",
          },
          {
              indexed: true,
              internalType: "address",
              name: "recipient",
              type: "address",
          },
          {
              indexed: false,
              internalType: "uint256",
              name: "amount",
              type: "uint256",
          },
      ],
      name: "RoyaltyPayout",
      type: "event",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_tokenContract",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "_tokenId",
              type: "uint256",
          },
          {
              internalType: "uint256",
              name: "_amount",
              type: "uint256",
          },
          {
              internalType: "address",
              name: "_payoutCurrency",
              type: "address",
          },
      ],
      name: "_handleRoyaltyEnginePayout",
      outputs: [
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
      ],
      stateMutability: "payable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
      ],
      name: "askForNFT",
      outputs: [
          {
              internalType: "address",
              name: "seller",
              type: "address",
          },
          {
              internalType: "address",
              name: "sellerFundsRecipient",
              type: "address",
          },
          {
              internalType: "address",
              name: "askCurrency",
              type: "address",
          },
          {
              internalType: "uint16",
              name: "findersFeeBps",
              type: "uint16",
          },
          {
              internalType: "uint256",
              name: "askPrice",
              type: "uint256",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_tokenContract",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "_tokenId",
              type: "uint256",
          },
      ],
      name: "cancelAsk",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_tokenContract",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "_tokenId",
              type: "uint256",
          },
          {
              internalType: "uint256",
              name: "_askPrice",
              type: "uint256",
          },
          {
              internalType: "address",
              name: "_askCurrency",
              type: "address",
          },
          {
              internalType: "address",
              name: "_sellerFundsRecipient",
              type: "address",
          },
          {
              internalType: "uint16",
              name: "_findersFeeBps",
              type: "uint16",
          },
      ],
      name: "createAsk",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [],
      name: "erc20TransferHelper",
      outputs: [
          {
              internalType: "contract ERC20TransferHelper",
              name: "",
              type: "address",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "erc721TransferHelper",
      outputs: [
          {
              internalType: "contract ERC721TransferHelper",
              name: "",
              type: "address",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_tokenContract",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "_tokenId",
              type: "uint256",
          },
          {
              internalType: "address",
              name: "_fillCurrency",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "_fillAmount",
              type: "uint256",
          },
          {
              internalType: "address",
              name: "_finder",
              type: "address",
          },
      ],
      name: "fillAsk",
      outputs: [],
      stateMutability: "payable",
      type: "function",
  },
  {
      inputs: [],
      name: "name",
      outputs: [
          {
              internalType: "string",
              name: "",
              type: "string",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "registrar",
      outputs: [
          {
              internalType: "address",
              name: "",
              type: "address",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_tokenContract",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "_tokenId",
              type: "uint256",
          },
          {
              internalType: "uint256",
              name: "_askPrice",
              type: "uint256",
          },
          {
              internalType: "address",
              name: "_askCurrency",
              type: "address",
          },
      ],
      name: "setAskPrice",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_royaltyEngine",
              type: "address",
          },
      ],
      name: "setRoyaltyEngineAddress",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
];

const ZORAMODULEMANAGERABI = [
  {
      inputs: [
          {
              internalType: "address",
              name: "_registrar",
              type: "address",
          },
          {
              internalType: "address",
              name: "_feeToken",
              type: "address",
          },
      ],
      stateMutability: "nonpayable",
      type: "constructor",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "user",
              type: "address",
          },
          {
              indexed: true,
              internalType: "address",
              name: "module",
              type: "address",
          },
          {
              indexed: false,
              internalType: "bool",
              name: "approved",
              type: "bool",
          },
      ],
      name: "ModuleApprovalSet",
      type: "event",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "module",
              type: "address",
          },
      ],
      name: "ModuleRegistered",
      type: "event",
  },
  {
      anonymous: false,
      inputs: [
          {
              indexed: true,
              internalType: "address",
              name: "newRegistrar",
              type: "address",
          },
      ],
      name: "RegistrarChanged",
      type: "event",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_user",
              type: "address",
          },
          {
              internalType: "address",
              name: "_module",
              type: "address",
          },
      ],
      name: "isModuleApproved",
      outputs: [
          {
              internalType: "bool",
              name: "",
              type: "bool",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [],
      name: "moduleFeeToken",
      outputs: [
          {
              internalType: "contract ZoraProtocolFeeSettings",
              name: "",
              type: "address",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "",
              type: "address",
          },
      ],
      name: "moduleRegistered",
      outputs: [
          {
              internalType: "bool",
              name: "",
              type: "bool",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_module",
              type: "address",
          },
      ],
      name: "registerModule",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [],
      name: "registrar",
      outputs: [
          {
              internalType: "address",
              name: "",
              type: "address",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_module",
              type: "address",
          },
          {
              internalType: "bool",
              name: "_approved",
              type: "bool",
          },
      ],
      name: "setApprovalForModule",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_module",
              type: "address",
          },
          {
              internalType: "address",
              name: "_user",
              type: "address",
          },
          {
              internalType: "bool",
              name: "_approved",
              type: "bool",
          },
          {
              internalType: "uint256",
              name: "_deadline",
              type: "uint256",
          },
          {
              internalType: "uint8",
              name: "v",
              type: "uint8",
          },
          {
              internalType: "bytes32",
              name: "r",
              type: "bytes32",
          },
          {
              internalType: "bytes32",
              name: "s",
              type: "bytes32",
          },
      ],
      name: "setApprovalForModuleBySig",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address[]",
              name: "_modules",
              type: "address[]",
          },
          {
              internalType: "bool",
              name: "_approved",
              type: "bool",
          },
      ],
      name: "setBatchApprovalForModules",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_registrar",
              type: "address",
          },
      ],
      name: "setRegistrar",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "",
              type: "address",
          },
      ],
      name: "sigNonces",
      outputs: [
          {
              internalType: "uint256",
              name: "",
              type: "uint256",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "",
              type: "address",
          },
          {
              internalType: "address",
              name: "",
              type: "address",
          },
      ],
      name: "userApprovals",
      outputs: [
          {
              internalType: "bool",
              name: "",
              type: "bool",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
];

const ZORATRANSFERHELPERABI = [
  {
      inputs: [
          {
              internalType: "address",
              name: "_approvalsManager",
              type: "address",
          },
      ],
      stateMutability: "nonpayable",
      type: "constructor",
  },
  {
      inputs: [],
      name: "ZMM",
      outputs: [
          {
              internalType: "contract ZoraModuleManager",
              name: "",
              type: "address",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_user",
              type: "address",
          },
      ],
      name: "isModuleApproved",
      outputs: [
          {
              internalType: "bool",
              name: "",
              type: "bool",
          },
      ],
      stateMutability: "view",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_token",
              type: "address",
          },
          {
              internalType: "address",
              name: "_from",
              type: "address",
          },
          {
              internalType: "address",
              name: "_to",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "_tokenId",
              type: "uint256",
          },
      ],
      name: "safeTransferFrom",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
  {
      inputs: [
          {
              internalType: "address",
              name: "_token",
              type: "address",
          },
          {
              internalType: "address",
              name: "_from",
              type: "address",
          },
          {
              internalType: "address",
              name: "_to",
              type: "address",
          },
          {
              internalType: "uint256",
              name: "_tokenId",
              type: "uint256",
          },
      ],
      name: "transferFrom",
      outputs: [],
      stateMutability: "nonpayable",
      type: "function",
  },
];

const ERC20ABI = [
  {
    constant: true,
    inputs: [],
    name: "name",
    outputs: [
      {
        name: "",
        type: "string",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        name: "_spender",
        type: "address",
      },
      {
        name: "_value",
        type: "uint256",
      },
    ],
    name: "approve",
    outputs: [
      {
        name: "",
        type: "bool",
      },
    ],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: true,
    inputs: [],
    name: "totalSupply",
    outputs: [
      {
        name: "",
        type: "uint256",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        name: "_from",
        type: "address",
      },
      {
        name: "_to",
        type: "address",
      },
      {
        name: "_value",
        type: "uint256",
      },
    ],
    name: "transferFrom",
    outputs: [
      {
        name: "",
        type: "bool",
      },
    ],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: true,
    inputs: [],
    name: "decimals",
    outputs: [
      {
        name: "",
        type: "uint8",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: true,
    inputs: [
      {
        name: "_owner",
        type: "address",
      },
    ],
    name: "balanceOf",
    outputs: [
      {
        name: "balance",
        type: "uint256",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: true,
    inputs: [],
    name: "symbol",
    outputs: [
      {
        name: "",
        type: "string",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        name: "_to",
        type: "address",
      },
      {
        name: "_value",
        type: "uint256",
      },
    ],
    name: "transfer",
    outputs: [
      {
        name: "",
        type: "bool",
      },
    ],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: true,
    inputs: [
      {
        name: "_owner",
        type: "address",
      },
      {
        name: "_spender",
        type: "address",
      },
    ],
    name: "allowance",
    outputs: [
      {
        name: "",
        type: "uint256",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    payable: true,
    stateMutability: "payable",
    type: "fallback",
  },
  {
    anonymous: false,
    inputs: [
      {
        indexed: true,
        name: "owner",
        type: "address",
      },
      {
        indexed: true,
        name: "spender",
        type: "address",
      },
      {
        indexed: false,
        name: "value",
        type: "uint256",
      },
    ],
    name: "Approval",
    type: "event",
  },
  {
    anonymous: false,
    inputs: [
      {
        indexed: true,
        name: "from",
        type: "address",
      },
      {
        indexed: true,
        name: "to",
        type: "address",
      },
      {
        indexed: false,
        name: "value",
        type: "uint256",
      },
    ],
    name: "Transfer",
    type: "event",
  },
];
const DAIABI = [
  {
    inputs: [
      {
        internalType: "uint256",
        name: "chainId_",
        type: "uint256",
      },
    ],
    payable: false,
    stateMutability: "nonpayable",
    type: "constructor",
  },
  {
    anonymous: false,
    inputs: [
      {
        indexed: true,
        internalType: "address",
        name: "src",
        type: "address",
      },
      {
        indexed: true,
        internalType: "address",
        name: "guy",
        type: "address",
      },
      {
        indexed: false,
        internalType: "uint256",
        name: "wad",
        type: "uint256",
      },
    ],
    name: "Approval",
    type: "event",
  },
  {
    anonymous: true,
    inputs: [
      {
        indexed: true,
        internalType: "bytes4",
        name: "sig",
        type: "bytes4",
      },
      {
        indexed: true,
        internalType: "address",
        name: "usr",
        type: "address",
      },
      {
        indexed: true,
        internalType: "bytes32",
        name: "arg1",
        type: "bytes32",
      },
      {
        indexed: true,
        internalType: "bytes32",
        name: "arg2",
        type: "bytes32",
      },
      {
        indexed: false,
        internalType: "bytes",
        name: "data",
        type: "bytes",
      },
    ],
    name: "LogNote",
    type: "event",
  },
  {
    anonymous: false,
    inputs: [
      {
        indexed: true,
        internalType: "address",
        name: "src",
        type: "address",
      },
      {
        indexed: true,
        internalType: "address",
        name: "dst",
        type: "address",
      },
      {
        indexed: false,
        internalType: "uint256",
        name: "wad",
        type: "uint256",
      },
    ],
    name: "Transfer",
    type: "event",
  },
  {
    constant: true,
    inputs: [],
    name: "DOMAIN_SEPARATOR",
    outputs: [
      {
        internalType: "bytes32",
        name: "",
        type: "bytes32",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: true,
    inputs: [],
    name: "PERMIT_TYPEHASH",
    outputs: [
      {
        internalType: "bytes32",
        name: "",
        type: "bytes32",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: true,
    inputs: [
      {
        internalType: "address",
        name: "",
        type: "address",
      },
      {
        internalType: "address",
        name: "",
        type: "address",
      },
    ],
    name: "allowance",
    outputs: [
      {
        internalType: "uint256",
        name: "",
        type: "uint256",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "usr",
        type: "address",
      },
      {
        internalType: "uint256",
        name: "wad",
        type: "uint256",
      },
    ],
    name: "approve",
    outputs: [
      {
        internalType: "bool",
        name: "",
        type: "bool",
      },
    ],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: true,
    inputs: [
      {
        internalType: "address",
        name: "",
        type: "address",
      },
    ],
    name: "balanceOf",
    outputs: [
      {
        internalType: "uint256",
        name: "",
        type: "uint256",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "usr",
        type: "address",
      },
      {
        internalType: "uint256",
        name: "wad",
        type: "uint256",
      },
    ],
    name: "burn",
    outputs: [],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: true,
    inputs: [],
    name: "decimals",
    outputs: [
      {
        internalType: "uint8",
        name: "",
        type: "uint8",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "guy",
        type: "address",
      },
    ],
    name: "deny",
    outputs: [],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "usr",
        type: "address",
      },
      {
        internalType: "uint256",
        name: "wad",
        type: "uint256",
      },
    ],
    name: "mint",
    outputs: [],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "src",
        type: "address",
      },
      {
        internalType: "address",
        name: "dst",
        type: "address",
      },
      {
        internalType: "uint256",
        name: "wad",
        type: "uint256",
      },
    ],
    name: "move",
    outputs: [],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: true,
    inputs: [],
    name: "name",
    outputs: [
      {
        internalType: "string",
        name: "",
        type: "string",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: true,
    inputs: [
      {
        internalType: "address",
        name: "",
        type: "address",
      },
    ],
    name: "nonces",
    outputs: [
      {
        internalType: "uint256",
        name: "",
        type: "uint256",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "holder",
        type: "address",
      },
      {
        internalType: "address",
        name: "spender",
        type: "address",
      },
      {
        internalType: "uint256",
        name: "nonce",
        type: "uint256",
      },
      {
        internalType: "uint256",
        name: "expiry",
        type: "uint256",
      },
      {
        internalType: "bool",
        name: "allowed",
        type: "bool",
      },
      {
        internalType: "uint8",
        name: "v",
        type: "uint8",
      },
      {
        internalType: "bytes32",
        name: "r",
        type: "bytes32",
      },
      {
        internalType: "bytes32",
        name: "s",
        type: "bytes32",
      },
    ],
    name: "permit",
    outputs: [],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "usr",
        type: "address",
      },
      {
        internalType: "uint256",
        name: "wad",
        type: "uint256",
      },
    ],
    name: "pull",
    outputs: [],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "usr",
        type: "address",
      },
      {
        internalType: "uint256",
        name: "wad",
        type: "uint256",
      },
    ],
    name: "push",
    outputs: [],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "guy",
        type: "address",
      },
    ],
    name: "rely",
    outputs: [],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: true,
    inputs: [],
    name: "symbol",
    outputs: [
      {
        internalType: "string",
        name: "",
        type: "string",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: true,
    inputs: [],
    name: "totalSupply",
    outputs: [
      {
        internalType: "uint256",
        name: "",
        type: "uint256",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "dst",
        type: "address",
      },
      {
        internalType: "uint256",
        name: "wad",
        type: "uint256",
      },
    ],
    name: "transfer",
    outputs: [
      {
        internalType: "bool",
        name: "",
        type: "bool",
      },
    ],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: false,
    inputs: [
      {
        internalType: "address",
        name: "src",
        type: "address",
      },
      {
        internalType: "address",
        name: "dst",
        type: "address",
      },
      {
        internalType: "uint256",
        name: "wad",
        type: "uint256",
      },
    ],
    name: "transferFrom",
    outputs: [
      {
        internalType: "bool",
        name: "",
        type: "bool",
      },
    ],
    payable: false,
    stateMutability: "nonpayable",
    type: "function",
  },
  {
    constant: true,
    inputs: [],
    name: "version",
    outputs: [
      {
        internalType: "string",
        name: "",
        type: "string",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
  {
    constant: true,
    inputs: [
      {
        internalType: "address",
        name: "",
        type: "address",
      },
    ],
    name: "wards",
    outputs: [
      {
        internalType: "uint256",
        name: "",
        type: "uint256",
      },
    ],
    payable: false,
    stateMutability: "view",
    type: "function",
  },
];

// Mainnet DAI, Optimism and Arbitrium Rollup Contracts with local addresses
module.exports = {
  1: {
    contracts: {
      DAI: {
        address: "0x6B175474E89094C44Da98b954EedeAC495271d0F",
        abi: DAIABI,
      },
      UNI: {
        address: "0x1f9840a85d5aF5bf1D1762F925BDADdC4201F984",
        abi: ERC20ABI,
      },
      zoraTransferHelperMAINNET: {
        address: "0x909e9efE4D87d1a6018C2065aE642b6D0447bc91",
        abi: ZORATRANSFERHELPERABI,
      },
      zoraModuleManagerMAINNET: {
        address: "0x850A7c6fE2CF48eea1393554C8A3bA23f20CC401",
        abi: ZORAMODULEMANAGERABI,
      },
      zoraAsksV1_1ModuleMAINNET: {
        address: "0x6170B3C3A54C3d8c854934cBC314eD479b2B29A3",
        abi: ZORAASKSV1_1MODULEABI,
      },
      lostandfoundContractMAINNET: {
        address: "0x6C0845540C0b7B868C3a1739246fC99aDEDC8036",
        abi: LOSTANDFOUNDCONTRACT4ABI,
      },                        
    },
  },
  4: {
    contracts: {
      zoraTransferHelper: {
        address: "0x029AA5a949C9C90916729D50537062cb73b5Ac92",
        abi: ZORATRANSFERHELPERABI,
      },
      zoraModuleManager: {
        address: "0xa248736d3b73A231D95A5F99965857ebbBD42D85",
        abi: ZORAMODULEMANAGERABI,
      },
      zoraAsksV1_1Module: {
        address: "0xA98D3729265C88c5b3f861a0c501622750fF4806",
        abi: ZORAASKSV1_1MODULEABI,
      },
      lostandFoundContract: {
        address: "0xcdEC1f89eE5755C3b519A2F66851711da84BF876",
        abi: LOSTANDFOUNDCONTRACTABI,
      },
      lostandFoundContract2: {
        address: "0x7b2DE8719120F21Ac8A95f9115bc8D9779EC44d4",
        abi: LOSTANDFOUNDCONTRACT2ABI,
      },
      lostandFoundContract3: {
        address: "0x288FC01ACcf7E053cD594AA18eff3e2D549600b7",
        abi: LOSTANDFOUNDCONTRACT3ABI,
      },
      lostandFoundContract4: {
          address: "0x60bf8601de15c9fFC689314E7E76C701a1b01645",
          abi: LOSTANDFOUNDCONTRACT4ABI,
      },      
    },
  },
  3: {
    contracts: {
        zoraTransferHelperROPSTEN: {
            address: "0x0afB6A47C303f85c5A6e0DC6c9b4c2001E6987ED",
            abi: ZORATRANSFERHELPERABI,
        },
        zoraModuleManagerROPSTEN: {
            address: "0x3120f8A161bf8ae8C4287A66920E7Fd875b41805",
            abi: ZORAMODULEMANAGERABI,
        },
        zoraAsksV1_1ModuleROPSTEN: {
            address: "0x03abaccb86f7bc2990181b71c2ef85927bf41044",
            abi: ZORAASKSV1_1MODULEABI,
        },
        zeroExErc721OrdersFeatureROPSTEN: {
            address: "0x72657b338391c6a55120eb786a2b4feced7d3be2",
            abi: ZEROEXERC721ORDERSFEATUREABI,
        },      
        lostandFoundContract4: {
            address: "0x9fd7ad2ecf7510eddcf0e6a345188d9df23805ac",
            abi: LOSTANDFOUNDCONTRACT4ABI,
        }      
    },
  },  
    10: {
        contracts: {
            zeroExErc721StatusOPTIMISM: {
                address: "0x0C58C1170f1DEd633862A1166f52107490a9C594",
                abi: ZEROEXERC721ORDERSFEATUREABI,
            },
            lostandFoundOptimism: {
                address: "0xa4248aC1a4Fc557134802f39cddF830Fde6DdA06",
                abi: LOSTANDFOUNDCONTRACT4ABI,
            }                  
        }        
    },  
};
