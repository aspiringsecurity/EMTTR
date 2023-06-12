pragma solidity ^0.4.23;
pragma experimental ABIEncoderV2;

contract Notes {
  uint count;

  struct Note {
    string tag;
    string encrypted;
    address author;
    string[] sharedWith; // public keys
    string[] encryptedKeys; // encrypted symmetric key
  }

  mapping (uint => Note) notes;

  event NoteAdded(uint id);

  constructor() public {
    count = 0;
  }

  function addNote(string memory tag, string memory encrypted, address author, string[] memory sharedWith, string[] memory encryptedKeys) public {
    Note memory note = Note({
      tag: tag,
      encrypted: encrypted,
      author: author,
      sharedWith: sharedWith,
      encryptedKeys: encryptedKeys
    });

    notes[count] = note;
    emit NoteAdded(count);
    count++;
  }

  function getNoteCount() public view returns (uint _count) {
    return count;
  }

  function getNote(uint id) public view returns (string memory tag, string memory encrypted, address author, string[] memory sharedWith, string[] memory encryptedKeys) {
    Note memory note = notes[id];
    return (note.tag, note.encrypted, note.author, note.sharedWith, note.encryptedKeys);
  }
}