/**
 * Limits the length of a string for display purposes, replacing the removed text
 * with the replacement entity specified
 * 
 * @param {String} strToShorten - string to shorten.
 * @param {Number} maxLength - maximum length of string before appending the replacement entity.
 * @param {String} replacement - string to replace the removed text with, defaults to '…'
 * @param {Boolean} trimMiddle - if true, maxLength chars form the beginning and end will be 
 * shown, and the middle of the string will be trimmed, ie '123…789' (maxLength = 3, delimiter = '…')
 * @example 
 * const fullLength = '1234567890';
 * limitLength(fullLength, 3);
 * // returns '123…'
 * @example
 * const fullLength = '1234567890';
 * limitLength(fullLength, 3, '-', true);
 * // returns '123-890'
 * @returns {String} the shortened string
 */
function limitLength (strToShorten, maxLength, replacement, trimMiddle){
    if(!strToShorten) return '';

    const fullStringLength = strToShorten.length;
    const ellips = replacement || '…';

    if(trimMiddle && fullStringLength > maxLength * 2){
      return  [strToShorten.substring(0, maxLength), ellips, strToShorten.substring(fullStringLength - maxLength)].join('');
    }

    if(fullStringLength > maxLength){
      return strToShorten.substring(0, maxLength) + ellips;
    }
    return strToShorten;
  }

/**
 * Limits the length of an address for display purposes, replacing the removed hex
 * chars with the replacement entity specified
 * 
 * @param {String} address - address to shorten.
 * @param {Number} maxLength - maximum hex chars to show before appending the replacement.
 * @param {String} replacement - string to replace the removed hex chars with, defaults to '…'
 * @example 
 * const fullLength = '0x3901F05c5e296E97c8Dc2ebEdCCa5F010f895552';
 * limitAddressLength(fullLength, 4);
 * // returns '0x3901…5552'
 * @example
 * const fullLength = '0x3901F05c5e296E97c8Dc2ebEdCCa5F010f895552';
 * limitAddressLength(fullLength, 3, '-');
 * // returns '0x390-552'
 * @returns {String} the shortened string
 */
function limitAddressLength (address, maxLength, replacement){
  if(!address) return '';
  let prepend0x = false;

  if(address.startsWith('0x')){
    address = address.substring(2);
    prepend0x = true;
  }

  return `${prepend0x ? '0x': ''}${limitLength(address, maxLength, replacement, true)}`;
}

/**
 * Formats an ethereum amount using fixed-point notation.
 * 
 * @param {any} eth - amount of ethereum to display.
 * @param {Number} decimals - number of decimal places to display.
 * @example 
 * const eth = 123.12345678901234567890;
 * limitAddressLength(eth, 4);
 * // returns 123.1234
 * @returns {Number} the ethereum amount in fixed-point notation
 */
function formatEth(eth, decimals){
  return Number.parseFloat(eth).toFixed(decimals);
}
  
  module.exports = {
    limitLength,
    limitAddressLength,
    formatEth
  }