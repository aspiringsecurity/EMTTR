module.exports = {
  'Notes': {
    'address': '0x2Bd63177f61D0Dc67534d48406c1849005dfE04B',
    'abi': [
      {
        'inputs': [],
        'payable': false,
        'stateMutability': 'nonpayable',
        'type': 'constructor',
        'signature': 'constructor'
      },
      {
        'anonymous': false,
        'inputs': [
          {
            'indexed': false,
            'name': 'id',
            'type': 'uint256'
          }
        ],
        'name': 'NoteAdded',
        'type': 'event',
        'signature': '0x47845bae1c19c06fc781e2d5e8c917f9d4d91f4d7cfceda2946276046a0aa8ec'
      },
      {
        'constant': false,
        'inputs': [
          {
            'name': 'tag',
            'type': 'string'
          },
          {
            'name': 'encrypted',
            'type': 'string'
          },
          {
            'name': 'author',
            'type': 'address'
          },
          {
            'name': 'sharedWith',
            'type': 'string[]'
          },
          {
            'name': 'encryptedKeys',
            'type': 'string[]'
          }
        ],
        'name': 'addNote',
        'outputs': [],
        'payable': false,
        'stateMutability': 'nonpayable',
        'type': 'function',
        'signature': '0x8a8d2ebd'
      },
      {
        'constant': true,
        'inputs': [],
        'name': 'getNoteCount',
        'outputs': [
          {
            'name': '_count',
            'type': 'uint256'
          }
        ],
        'payable': false,
        'stateMutability': 'view',
        'type': 'function',
        'signature': '0x41fb87fb'
      },
      {
        'constant': true,
        'inputs': [
          {
            'name': 'id',
            'type': 'uint256'
          }
        ],
        'name': 'getNote',
        'outputs': [
          {
            'name': 'tag',
            'type': 'string'
          },
          {
            'name': 'encrypted',
            'type': 'string'
          },
          {
            'name': 'author',
            'type': 'address'
          },
          {
            'name': 'sharedWith',
            'type': 'string[]'
          },
          {
            'name': 'encryptedKeys',
            'type': 'string[]'
          }
        ],
        'payable': false,
        'stateMutability': 'view',
        'type': 'function',
        'signature': '0xa965a941'
      }
    ]
  },
  'Users': {
    'address': '0xfFb383e52D4006690a5215a4a978220863147aD2',
    'abi': [
      {
        'inputs': [],
        'payable': false,
        'stateMutability': 'nonpayable',
        'type': 'constructor',
        'signature': 'constructor'
      },
      {
        'anonymous': false,
        'inputs': [
          {
            'indexed': false,
            'name': 'id',
            'type': 'uint256'
          }
        ],
        'name': 'UserAdded',
        'type': 'event',
        'signature': '0x27412bb19840b11057dee7c7019767312555c4497d8d4c2383fa6b7791bf014b'
      },
      {
        'constant': false,
        'inputs': [
          {
            'name': '_addr',
            'type': 'address'
          },
          {
            'name': '_pubKey',
            'type': 'bytes'
          },
          {
            'name': '_name',
            'type': 'string'
          }
        ],
        'name': 'addUser',
        'outputs': [],
        'payable': false,
        'stateMutability': 'nonpayable',
        'type': 'function',
        'signature': '0xc3325402'
      },
      {
        'constant': true,
        'inputs': [],
        'name': 'getUserCount',
        'outputs': [
          {
            'name': '_count',
            'type': 'uint256'
          }
        ],
        'payable': false,
        'stateMutability': 'view',
        'type': 'function',
        'signature': '0xb5cb15f7'
      },
      {
        'constant': true,
        'inputs': [
          {
            'name': 'id',
            'type': 'uint256'
          }
        ],
        'name': 'getUser',
        'outputs': [
          {
            'name': 'addr',
            'type': 'address'
          },
          {
            'name': 'pubKey',
            'type': 'bytes'
          },
          {
            'name': 'name',
            'type': 'string'
          }
        ],
        'payable': false,
        'stateMutability': 'view',
        'type': 'function',
        'signature': '0xb0467deb'
      }
    ]
  }
}
