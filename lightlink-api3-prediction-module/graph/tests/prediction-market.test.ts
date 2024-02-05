import {
  assert,
  describe,
  test,
  clearStore,
  beforeAll,
  afterAll
} from "matchstick-as/assembly/index"
import { BigInt, Address } from "@graphprotocol/graph-ts"
import { ConcludeFatalError } from "../generated/schema"
import { ConcludeFatalError as ConcludeFatalErrorEvent } from "../generated/PredictionMarket/PredictionMarket"
import { handleConcludeFatalError } from "../src/prediction-market"
import { createConcludeFatalErrorEvent } from "./prediction-market-utils"

// Tests structure (matchstick-as >=0.5.0)
// https://thegraph.com/docs/en/developer/matchstick/#tests-structure-0-5-0

describe("Describe entity assertions", () => {
  beforeAll(() => {
    let predictionId = BigInt.fromI32(234)
    let timestamp = BigInt.fromI32(234)
    let isAbove = "boolean Not implemented"
    let priceReading = BigInt.fromI32(234)
    let priceTarget = BigInt.fromI32(234)
    let newConcludeFatalErrorEvent = createConcludeFatalErrorEvent(
      predictionId,
      timestamp,
      isAbove,
      priceReading,
      priceTarget
    )
    handleConcludeFatalError(newConcludeFatalErrorEvent)
  })

  afterAll(() => {
    clearStore()
  })

  // For more test scenarios, see:
  // https://thegraph.com/docs/en/developer/matchstick/#write-a-unit-test

  test("ConcludeFatalError created and stored", () => {
    assert.entityCount("ConcludeFatalError", 1)

    // 0xa16081f360e3847006db660bae1c6d1b2e17ec2a is the default address used in newMockEvent() function
    assert.fieldEquals(
      "ConcludeFatalError",
      "0xa16081f360e3847006db660bae1c6d1b2e17ec2a-1",
      "predictionId",
      "234"
    )
    assert.fieldEquals(
      "ConcludeFatalError",
      "0xa16081f360e3847006db660bae1c6d1b2e17ec2a-1",
      "timestamp",
      "234"
    )
    assert.fieldEquals(
      "ConcludeFatalError",
      "0xa16081f360e3847006db660bae1c6d1b2e17ec2a-1",
      "isAbove",
      "boolean Not implemented"
    )
    assert.fieldEquals(
      "ConcludeFatalError",
      "0xa16081f360e3847006db660bae1c6d1b2e17ec2a-1",
      "priceReading",
      "234"
    )
    assert.fieldEquals(
      "ConcludeFatalError",
      "0xa16081f360e3847006db660bae1c6d1b2e17ec2a-1",
      "priceTarget",
      "234"
    )

    // More assert options:
    // https://thegraph.com/docs/en/developer/matchstick/#asserts
  })
})
