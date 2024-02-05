import {
  ConcludeFatalError as ConcludeFatalErrorEvent,
  HandlerProgress as HandlerProgressEvent,
  OwnershipTransferred as OwnershipTransferredEvent,
  PredictionConcluded as PredictionConcludedEvent,
  PredictionCreated as PredictionCreatedEvent
} from "../generated/PredictionMarket/PredictionMarket"
import {
  ConcludeFatalError,
  HandlerProgress,
  OwnershipTransferred,
  PredictionConcluded,
  PredictionCreated
} from "../generated/schema"

export function handleConcludeFatalError(event: ConcludeFatalErrorEvent): void {
  let entity = new ConcludeFatalError(
    event.transaction.hash.concatI32(event.logIndex.toI32())
  )
  entity.predictionId = event.params.predictionId
  entity.timestamp = event.params.timestamp
  entity.isAbove = event.params.isAbove
  entity.priceReading = event.params.priceReading
  entity.priceTarget = event.params.priceTarget

  entity.blockNumber = event.block.number
  entity.blockTimestamp = event.block.timestamp
  entity.transactionHash = event.transaction.hash

  entity.save()
}

export function handleHandlerProgress(event: HandlerProgressEvent): void {
  let entity = new HandlerProgress(
    event.transaction.hash.concatI32(event.logIndex.toI32())
  )
  entity.predictionId = event.params.predictionId
  entity.marketHandler = event.params.marketHandler
  entity.trader = event.params.trader
  entity.amountYes = event.params.amountYes
  entity.amountNo = event.params.amountNo

  entity.blockNumber = event.block.number
  entity.blockTimestamp = event.block.timestamp
  entity.transactionHash = event.transaction.hash

  entity.save()
}

export function handleOwnershipTransferred(
  event: OwnershipTransferredEvent
): void {
  let entity = new OwnershipTransferred(
    event.transaction.hash.concatI32(event.logIndex.toI32())
  )
  entity.previousOwner = event.params.previousOwner
  entity.newOwner = event.params.newOwner

  entity.blockNumber = event.block.number
  entity.blockTimestamp = event.block.timestamp
  entity.transactionHash = event.transaction.hash

  entity.save()
}

export function handlePredictionConcluded(
  event: PredictionConcludedEvent
): void {
  let entity = new PredictionConcluded(
    event.transaction.hash.concatI32(event.logIndex.toI32())
  )
  entity.predictionId = event.params.predictionId
  entity.timestamp = event.params.timestamp

  entity.blockNumber = event.block.number
  entity.blockTimestamp = event.block.timestamp
  entity.transactionHash = event.transaction.hash

  entity.save()
}

export function handlePredictionCreated(event: PredictionCreatedEvent): void {
  let entity = new PredictionCreated(
    event.transaction.hash.concatI32(event.logIndex.toI32())
  )
  entity.predictionId = event.params.predictionId
  entity.marketHandler = event.params.marketHandler
  entity.creator = event.params.creator
  entity.timestamp = event.params.timestamp
  entity.deadline = event.params.deadline

  entity.blockNumber = event.block.number
  entity.blockTimestamp = event.block.timestamp
  entity.transactionHash = event.transaction.hash

  entity.save()
}
