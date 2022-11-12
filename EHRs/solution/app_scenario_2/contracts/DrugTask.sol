/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

import "./Legitimation.sol";
import "./LoyaltyToken.sol";
import "./DataAccess.sol";
import "./TaskManage.sol";

contract DrugTask is Legitimation, LoyaltyToken, DataAccess, TaskManage {
    function submitTaskRequest(address dataOwner,
                               string encryptCode)
        public returns (uint dataId,
                        uint playerId) {
        dataId = sendDataRequest(dataOwner, encryptCode);
        playerId = submitPlayerRequest(dataId);
    }

    function approveTaskRequest(uint playerId,
                                string encryptCode,
                                string dataURL,
                                string dataHash)
        public onlyOwner validTask {
        uint dataId = playerPool[playerId].dataInputId;
        approveDataRequest(dataId, dataURL, dataHash);
        dataId = sendDataRequest(playerPool[playerId].addr, encryptCode);
        approvePlayerRequest(playerId, dataId);
    }

    function submitTaskResult(uint playerId,
                              string dataURL,
                              string dataHash,
                              int result)
        public validTask {
        uint dataId = playerPool[playerId].dataOutputId;
        approveDataRequest(dataId, dataURL, dataHash);
        submitPlayerResult(playerId, result);
    }

    function approveTaskResult(uint playerId) public {
        approvePlayerResult(playerId);
        if (task.isFinished) {
            uint bestPlayerId;
            if (task.isCrossValidate)
                bestPlayerId = crossValidate();
            else
                bestPlayerId = playerId;
            transferBonus(playerPool[bestPlayerId].addr, bonus);
        }
    }

    function rejectTaskResult(uint playerId) public {
        rejectPlayerResult(playerId);
    }
}
