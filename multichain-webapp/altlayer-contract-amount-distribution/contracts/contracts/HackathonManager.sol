// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.9;

import "@openzeppelin/contracts/access/Ownable.sol";

contract HackathonManager is Ownable {

    enum HackathonState { UPCOMING, OPEN, CLOSED, CONCLUDED } 
    
    uint256 public _hackathonFundBalance;
    uint256 public _currentTrackTotal;
    uint256 public _totalPrizePool;
    address public _contractFactory;
    address public _hackOwner;
    string public _hackathonName;
    string public _cid;
    string[] public _tracks;
    Participant[] public _participants;
    HackathonState public _state;
    bool public isFinished;


    struct Participant {
        string _teamName;
        string _project;
        string _projectLink;
        bool _submitted;
        bool _isSubmissionValid;
        bool _isWinner;
        address _participantAddress;
    }

    struct Prize {
        string _name;
        Participant _winner;
        uint _amount;
        bool _paid;
    }

    struct Track {
        string _trackName;
        uint256 _trackPoolAmount;
        uint256 _currentPrizeTotal;
        mapping(string => Prize) _prizes;
    }

    mapping (string => Track) public _hackathonTracks;
    mapping (string => Participant) public _hackathonParticipants;
    mapping (address => bool) public _hackathonCommitteeMembers;
    mapping (string => bool) public _prizeNameTaken;
    
    event HackathonCreated(string _hack, address _creator);
    event HackathonFunded(uint _amountFunded, uint _currentHackatonFundBalance);
    event HackathonCIDAdded(address _addedBy, string CID);
    event TrackCreated(string _trackName, uint _totalPrizePool);
    event PrizeWinnerCaptured(string _track, string _prize, string _team);
    event PrizePaid(string _team, address _toAddress, uint256 _amount);
    event PrizeAddedToTrack(string _track, string _prize, uint256 _prizeAmount);
    event ParticipantRegistered(string _teamName);
    event ProjectSubmitted(string _teamName, string _project);
    event ProjectApproved(string _teamName, string _project, address _approver);
    event ProjectRejected(string _teamName, string _project, address _rejector);
    event CommitteeMemberAdded(address _newMember);
    event CommitteeMemberRemoved(address _oldMember);

    modifier onlyCommitteeMembers(address _member){
        require(_hackathonCommitteeMembers[_member] == true, "Only committee members!");
        _;
    }

    modifier cannotExceedFundedAmount(uint256 _amount){
        require(_currentTrackTotal + _amount <= _hackathonFundBalance, "Contract funds low!");
        _;
    }

    modifier trackPrizeTotalCannotExceedTrackPoolAmount(string memory _trackName, uint256 _amount){
        require(_hackathonTracks[_trackName]._currentPrizeTotal + _amount <= _hackathonTracks[_trackName]._trackPoolAmount, "Total greater than pool!");
        _;
    }

    modifier onlyParticipant(string memory _teamName){
        address _participant = _hackathonParticipants[_teamName]._participantAddress;
        require(_participant == msg.sender, "Not main participant");
        _;
    }

    modifier prizeNameShouldBeUnique(string memory _name){
        require(_prizeNameTaken[_name] != true, "Should be unique!");
        _;
    }

    modifier shouldHaveTracks(){
        require(_tracks.length > 0, "No tracks!");
        _;
    }

    constructor (address _hackathonOwner, string memory _name) Ownable(){
        _contractFactory = msg.sender;
        _hackathonName = _name;
        Ownable.transferOwnership(_hackathonOwner);
        _hackOwner = _hackathonOwner;
        _hackathonCommitteeMembers[_hackathonOwner] = true;
        _state = HackathonState.UPCOMING;
        emit HackathonCreated(_name, _hackathonOwner);
    }

    function addCID(string memory cid) external onlyCommitteeMembers(msg.sender){
        _cid = cid;
        emit HackathonCIDAdded(msg.sender, _cid);
    }

    function getCID() external view returns(string memory cid){
        cid = _cid;
    }

    function ParticipantsLength()public view returns(uint256){
        return _participants.length;
    }

    function getCurrentMaxIndexOfTracks()public view returns(uint256){
        return _tracks.length;
    }
    
    function getAllTracks() public view returns (string[] memory) {
        return _tracks;
    }

    function getTrackByIndex(uint256 index)public view shouldHaveTracks returns(string memory trackName){
        require(bytes(_tracks[index]).length > 0, "Track doesn't exist!");
        trackName = _tracks[index];
    }

    function addCommitteeMember(address _newMember) public onlyOwner {
        _hackathonCommitteeMembers[_newMember] = true;
        emit CommitteeMemberAdded(_newMember);
    }

    function removeCommitteeMember(address _oldMember) public onlyOwner {
        _hackathonCommitteeMembers[_oldMember] = false;
        emit CommitteeMemberRemoved(_oldMember);
    }

    function getHackathonState() external view returns(string memory state){
        if(_state == HackathonState.UPCOMING) state = "upcoming";
        if(_state == HackathonState.OPEN) state = "open";
        if(_state == HackathonState.CONCLUDED) state = "concluded";
        if(_state == HackathonState.CLOSED) state = "closed";
    }

    function fundHackathon() external payable returns(uint256 _balance){
        _hackathonFundBalance += msg.value;
        _balance = _hackathonFundBalance;
        emit HackathonFunded(msg.value, _hackathonFundBalance);
    }

    function registerParticipant(string memory _teamName, string memory _projectName, string memory _projectLink) external {
        require(isFinished == false,"Sorry, hackaton already over!");
        require(bytes(_hackathonParticipants[_teamName]._teamName).length == 0, "Team name exists");
        require(bytes(_teamName).length > 0, "Team required!");
        _hackathonParticipants[_teamName] = Participant(_teamName, _projectName, _projectLink, false, false, false, msg.sender);
        _participants.push(Participant(_teamName, _projectName, _projectLink, false, false, false, msg.sender));
        emit ParticipantRegistered(_teamName);
    }

    function submitProject(string memory _team)external onlyParticipant(_team){
        _hackathonParticipants[_team]._submitted = true;
        emit ProjectSubmitted(_team, _hackathonParticipants[_team]._projectLink);
    }

    function trackExists(string memory _trackName) internal view returns(bool){
        if (bytes(_hackathonTracks[_trackName]._trackName).length != 0){
            return true;
        }
        return false;
    }

    function createTrack(string memory _newTrackName, uint256 _newTrackPoolAmount) external cannotExceedFundedAmount(_newTrackPoolAmount) onlyCommitteeMembers(msg.sender) {
        require(!trackExists(_newTrackName), "Track exists");
        require(bytes(_newTrackName).length != 0, "Can't be empty name");
        Track storage newTrack = _hackathonTracks[_newTrackName];
        newTrack._currentPrizeTotal = 0;
        newTrack._trackName = _newTrackName;
        newTrack._trackPoolAmount = _newTrackPoolAmount;
        _currentTrackTotal += _newTrackPoolAmount;
        _tracks.push(_newTrackName);
        emit TrackCreated(_newTrackName, _newTrackPoolAmount);
    }

    function addPrizeToTrack(string memory _trackName, string memory _prizeName, uint256 _amount) external trackPrizeTotalCannotExceedTrackPoolAmount(_trackName, _amount) onlyCommitteeMembers(msg.sender) prizeNameShouldBeUnique(_prizeName){
        require(trackExists(_trackName), "Track doesn't exist");
        require(_amount > 0, "Amount can't be zero!");
        Participant memory initParticipant = Participant("", "", "", false, false, false, address(0));
        _hackathonTracks[_trackName]._prizes[_prizeName] = (Prize(_prizeName, initParticipant, _amount, false));
        _prizeNameTaken[_prizeName] = true;
        emit PrizeAddedToTrack(_trackName, _prizeName, _amount);
    }

    function captureWinner(string memory _track, string memory _prize, string memory _team) external onlyCommitteeMembers(msg.sender) {
        require(trackExists(_track), "Track doesn't exist");
        require(_prizeNameTaken[_prize] == true, "Prize doesn't exist!");
        Participant memory winner = _hackathonParticipants[_team];
        winner._isWinner = true;
        require(_hackathonTracks[_track]._prizes[_prize]._paid != true, "Already paid!");
        _hackathonTracks[_track]._prizes[_prize]._winner = winner;
        emit PrizeWinnerCaptured(_track, _prize, _team);
        _hackathonTracks[_track]._prizes[_prize]._paid = true;
        
        isFinished = true;
        payable(winner._participantAddress).transfer(_hackathonTracks[_track]._prizes[_prize]._amount);
        emit PrizePaid(_team, winner._participantAddress, _hackathonTracks[_track]._prizes[_prize]._amount);
    }
}