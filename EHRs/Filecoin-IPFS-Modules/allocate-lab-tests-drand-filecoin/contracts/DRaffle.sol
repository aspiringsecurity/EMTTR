// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.18;

contract DRaffle {

    event Winner(uint blockHeight, address winner, uint256 amount);
    event NoWinner(uint blockHeight);
    event Scheduled(uint blockHeight);

    uint256 costPerDraw;
    uint256 drawCutoff;
    uint256 triggerReward;
    uint256 nextDrawBlockHeight;

    address[] candidates;

    constructor(uint256 roundCutoff, uint256 cost, uint256 reward) {
        costPerDraw = cost;
        triggerReward = reward;
        drawCutoff = roundCutoff;

        scheduleNext();
    }

    function enter() external payable {
        require(msg.value == costPerDraw, "you have passed too much or too little money to enter the lotto");
        require(block.number < nextDrawBlockHeight - drawCutoff, "It's too close to the next draw to participate");
        candidates.push(msg.sender);
    }

    function draw() external payable {
        require(block.number >= nextDrawBlockHeight, "it's too early to trigger the draw!");
        uint numberOfEntries = candidates.length;
        if (numberOfEntries == 0) {
            emit NoWinner(block.number);
        } else {
            address winner = candidates[block.prevrandao % numberOfEntries];
            uint256 amount = numberOfEntries * costPerDraw - triggerReward;

            payable(winner).transfer(amount);
            payable(msg.sender).transfer(triggerReward);

            emit Winner(block.number, winner, amount);
        }

        scheduleNext();
    }

    function nextDraw() public view returns (uint) {
        return nextDrawBlockHeight;
    }

    function scheduleNext() internal {
        candidates = new address[](0);
        nextDrawBlockHeight = block.number + 2880; // a new draw every 24hours
        emit Scheduled(nextDrawBlockHeight);
    }
}
