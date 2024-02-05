import { newMockEvent } from "matchstick-as"
import { ethereum, BigInt, Address } from "@graphprotocol/graph-ts"
import {
  ConcludeFatalError,
  HandlerProgress,
  OwnershipTransferred,
  PredictionConcluded,
  PredictionCreated
} from "../generated/PredictionMarket/PredictionMarket"

export function createConcludeFatalErrorEvent(
  predictionId: BigInt,
  timestamp: BigInt,
  isAbove: boolean,
  priceReading: BigInt,
  priceTarget: BigInt
): ConcludeFatalError {
  let concludeFatalErrorEvent = changetype<ConcludeFatalError>(newMockEvent())

  concludeFatalErrorEvent.parameters = new Array()

  concludeFatalErrorEvent.parameters.push(
    new ethereum.EventParam(
      "predictionId",
      ethereum.Value.fromUnsignedBigInt(predictionId)
    )
  )
  concludeFatalErrorEvent.parameters.push(
    new ethereum.EventParam(
      "timestamp",
      ethereum.Value.fromUnsignedBigInt(timestamp)
    )
  )
  concludeFatalErrorEvent.parameters.push(
    new ethereum.EventParam("isAbove", ethereum.Value.fromBoolean(isAbove))
  )
  concludeFatalErrorEvent.parameters.push(
    new ethereum.EventParam(
      "priceReading",
      ethereum.Value.fromSignedBigInt(priceReading)
    )
  )
  concludeFatalErrorEvent.parameters.push(
    new ethereum.EventParam(
      "priceTarget",
      ethereum.Value.fromSignedBigInt(priceTarget)
    )
  )

  return concludeFatalErrorEvent
}

export function createHandlerProgressEvent(
  predictionId: BigInt,
  marketHandler: Address,
  trader: Address,
  amountYes: BigInt,
  amountNo: BigInt
): HandlerProgress {
  let handlerProgressEvent = changetype<HandlerProgress>(newMockEvent())

  handlerProgressEvent.parameters = new Array()

  handlerProgressEvent.parameters.push(
    new ethereum.EventParam(
      "predictionId",
      ethereum.Value.fromUnsignedBigInt(predictionId)
    )
  )
  handlerProgressEvent.parameters.push(
    new ethereum.EventParam(
      "marketHandler",
      ethereum.Value.fromAddress(marketHandler)
    )
  )
  handlerProgressEvent.parameters.push(
    new ethereum.EventParam("trader", ethereum.Value.fromAddress(trader))
  )
  handlerProgressEvent.parameters.push(
    new ethereum.EventParam(
      "amountYes",
      ethereum.Value.fromSignedBigInt(amountYes)
    )
  )
  handlerProgressEvent.parameters.push(
    new ethereum.EventParam(
      "amountNo",
      ethereum.Value.fromSignedBigInt(amountNo)
    )
  )

  return handlerProgressEvent
}

export function createOwnershipTransferredEvent(
  previousOwner: Address,
  newOwner: Address
): OwnershipTransferred {
  let ownershipTransferredEvent = changetype<OwnershipTransferred>(
    newMockEvent()
  )

  ownershipTransferredEvent.parameters = new Array()

  ownershipTransferredEvent.parameters.push(
    new ethereum.EventParam(
      "previousOwner",
      ethereum.Value.fromAddress(previousOwner)
    )
  )
  ownershipTransferredEvent.parameters.push(
    new ethereum.EventParam("newOwner", ethereum.Value.fromAddress(newOwner))
  )

  return ownershipTransferredEvent
}

export function createPredictionConcludedEvent(
  predictionId: BigInt,
  timestamp: BigInt
): PredictionConcluded {
  let predictionConcludedEvent = changetype<PredictionConcluded>(newMockEvent())

  predictionConcludedEvent.parameters = new Array()

  predictionConcludedEvent.parameters.push(
    new ethereum.EventParam(
      "predictionId",
      ethereum.Value.fromUnsignedBigInt(predictionId)
    )
  )
  predictionConcludedEvent.parameters.push(
    new ethereum.EventParam(
      "timestamp",
      ethereum.Value.fromUnsignedBigInt(timestamp)
    )
  )

  return predictionConcludedEvent
}

export function createPredictionCreatedEvent(
  predictionId: BigInt,
  marketHandler: Address,
  creator: Address,
  timestamp: BigInt,
  deadline: BigInt
): PredictionCreated {
  let predictionCreatedEvent = changetype<PredictionCreated>(newMockEvent())

  predictionCreatedEvent.parameters = new Array()

  predictionCreatedEvent.parameters.push(
    new ethereum.EventParam(
      "predictionId",
      ethereum.Value.fromUnsignedBigInt(predictionId)
    )
  )
  predictionCreatedEvent.parameters.push(
    new ethereum.EventParam(
      "marketHandler",
      ethereum.Value.fromAddress(marketHandler)
    )
  )
  predictionCreatedEvent.parameters.push(
    new ethereum.EventParam("creator", ethereum.Value.fromAddress(creator))
  )
  predictionCreatedEvent.parameters.push(
    new ethereum.EventParam(
      "timestamp",
      ethereum.Value.fromUnsignedBigInt(timestamp)
    )
  )
  predictionCreatedEvent.parameters.push(
    new ethereum.EventParam(
      "deadline",
      ethereum.Value.fromUnsignedBigInt(deadline)
    )
  )

  return predictionCreatedEvent
}
