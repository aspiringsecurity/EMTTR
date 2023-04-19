import {
  ERC1155OrderCancelled,
  ERC1155OrderPreSigned,
  ERC721OrderCancelled,
  ERC721OrderPreSigned,
} from '../generated/ZeroEx/ZeroEx';
import { Erc1155Order, Erc721Order, Erc1155Cancellation, Erc721Cancellation, Fee, Property } from '../generated/schema';
import { Entity, store } from '@graphprotocol/graph-ts'


export function handleErc721OrderPreSigned(event: ERC721OrderPreSigned): void {
  let order = new Erc721Order(event.params.maker.toHex() + '-' + event.params.nonce.toString());
  order.direction = event.params.direction;
  order.maker = event.params.maker;
  order.taker = event.params.taker;
  order.expiry = event.params.expiry;
  order.nonce = event.params.nonce;
  order.erc20Token = event.params.erc20Token;
  order.erc20TokenAmount = event.params.erc20TokenAmount;
  let feeEntities: string[] = [];
  let fees = event.params.fees;
  for (let i = 0; i < fees.length; i++) {
      let feeStruct = fees[i];
      let feeEntity = new Fee(order.id + '-' + i.toString());
      feeEntity.recipient = feeStruct.recipient;
      feeEntity.amount = feeStruct.amount;
      feeEntity.feeData = feeStruct.feeData;
      feeEntity.save();
      feeEntities.push(feeEntity.id);
  }
  order.fees = feeEntities;
  order.erc721Token = event.params.erc721Token;
  order.erc721TokenId = event.params.erc721TokenId;
  let propertyEntities: string[] = [];
  let properties = event.params.erc721TokenProperties;
  for (let i = 0; i < properties.length; i++) {
      let propertyStruct = properties[i];
      let propertyEntity = new Property(order.id + '-' + i.toString());
      propertyEntity.propertyValidator = propertyStruct.propertyValidator;
      propertyEntity.propertyData = propertyStruct.propertyData;
      propertyEntity.save();
      propertyEntities.push(propertyEntity.id);
  }
  order.logIndex = event.logIndex;
  order.timestamp = event.block.timestamp;
  order.blockNumber = event.block.number

  order.save();
}

export function handleErc1155OrderPreSigned(event: ERC1155OrderPreSigned): void {
  let order = new Erc1155Order(event.params.maker.toHex() + '-' + event.params.nonce.toString());
  order.direction = event.params.direction;
  order.maker = event.params.maker;
  order.taker = event.params.taker;
  order.expiry = event.params.expiry;
  order.nonce = event.params.nonce;
  order.erc20Token = event.params.erc20Token;
  order.erc20TokenAmount = event.params.erc20TokenAmount;
  let feeEntities: string[] = [];
  let fees = event.params.fees;
  for (let i = 0; i < fees.length; i++) {
      let feeStruct = fees[i];
      let feeEntity = new Fee(order.id + '-' + i.toString());
      feeEntity.recipient = feeStruct.recipient;
      feeEntity.amount = feeStruct.amount;
      feeEntity.feeData = feeStruct.feeData;
      feeEntity.save();
      feeEntities.push(feeEntity.id);
  }
  order.fees = feeEntities;
  order.erc1155Token = event.params.erc1155Token;
  order.erc1155TokenId = event.params.erc1155TokenId;
  let propertyEntities: string[] = [];
  let properties = event.params.erc1155TokenProperties;
  for (let i = 0; i < properties.length; i++) {
      let propertyStruct = properties[i];
      let propertyEntity = new Property(order.id + '-' + i.toString());
      propertyEntity.propertyValidator = propertyStruct.propertyValidator;
      propertyEntity.propertyData = propertyStruct.propertyData;
      propertyEntity.save();
      propertyEntities.push(propertyEntity.id);
  }
  order.erc1155TokenProperties = propertyEntities;
  order.erc1155TokenAmount = event.params.erc1155TokenAmount;

  order.save();
}

export function handleErc721OrderCancelled(event: ERC721OrderCancelled): void {
  let id = event.params.maker.toHex() + '-' + event.params.nonce.toString();
  let order = Erc721Order.load(id);
  if (order != null) {
      store.remove('Erc721Order', id);
  }
}

export function handleErc1155OrderCancelled(event: ERC1155OrderCancelled): void {
  let id = event.params.maker.toHex() + '-' + event.params.nonce.toString();
  let order = Erc1155Order.load(id);
  if (order != null) {
      store.remove('Erc1155Order', id);
  }
}