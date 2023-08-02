#!/usr/bin/env python3

'''
generate base58 tests

really checking to see if my code matches the python base58 package which has
159 stars on github
'''

import base58 as b
import random as r

###########################
### simple cases (single bytes, two bytes, 10 bytes of same byte)
###########################

## 1 byte cases

def single_byte_case(byte):
    '''
    return the encode/decode pair corresponding to a single byte
    '''
    decoded_bytes = bytes([byte])
    encoded_bytes = b.b58encode(decoded_bytes)
    return {'db': decoded_bytes, 'eb': encoded_bytes}


def single_byte_cases():
    '''
    return the encode/decode pairs corresponding to every single byte case for
    byte between 0 and 255
    '''
    # range(0, 256) = [0, 256) intersect Z
    return [single_byte_case(n) for n in range(0, 256)]

## too many of these

### 2 byte cases
#
#def two_bytes(n):
#    '''
#    return the two bytes corresponding to <<n // 256, n % 256>>
#    '''
#    return bytes([n // 256, n % 256])
#
#
#def two_byte_case(n):
#    '''
#    return the encode/decode pair corresponding to <<n // 256, n % 256>>
#    '''
#    decoded_bytes = two_bytes(n)
#    encoded_bytes = b.b58encode(decoded_bytes)
#    return {'db': decoded_bytes, 'eb': encoded_bytes}
#
#
#def two_byte_cases():
#    '''
#    return the encode/decode pairs for each two bytes <<n:8, m:8>>
#    '''
#    return [two_byte_case(n) for n in range(0, 256*256)]


## 10 byte cases

def ten_bytes(byte):
    '''
    return the ten byte string which is byte repeated 10 times
    '''
    return bytes([byte for _ in range(10)])


def ten_byte_case(n):
    '''
    return the encode/decode pair corresponding to <<n>> * 10
    '''
    decoded_bytes = ten_bytes(n)
    encoded_bytes = b.b58encode(decoded_bytes)
    return {'db': decoded_bytes, 'eb': encoded_bytes}


def ten_byte_cases():
    '''
    return the encode/decode pairs for each string <<n:8>> * 10
    '''
    return [ten_byte_case(n) for n in range(0, 256)]



## all simple cases in one function

def simple_cases():
    return single_byte_cases() + ten_byte_cases()


###########################
### case generation
###########################

def random_bytes():
    '''
    return between 0 and 999 random bytes
    '''
    # randint is between [left, right]
    len = r.randint(0, 999)
    #len = 10
    acc = []
    for _ in range(len):
        # randint is between [left, right]
        randbyte = r.randint(0, 255)
        acc.append(randbyte)
    return bytes(acc)


def case():
    '''
    return a random encode/decode pair
    '''
    decoded_bytes = random_bytes()
    encoded_bytes = b.b58encode(decoded_bytes)
    return {'db': decoded_bytes, 'eb': encoded_bytes}


def cases(n):
    '''
    return n randomly generated encode/decode pairs
    '''
    return [case() for _ in range(n)]


###########################
### js formatting
###########################

def format_decoded_js(decoded_bytes):
    '''
    format the bytes as a js Uint8Array
    '''
    return f'new Uint8Array({list(decoded_bytes)})'

def format_encoded_js(encoded_bytes):
    s = str(encoded_bytes)[2:-1]
    return '"' + s + '"'

def format_case_js(case):
    decoded_bytes = case['db']
    encoded_bytes = case['eb']
    db_js = format_decoded_js(decoded_bytes)
    eb_js = format_encoded_js(encoded_bytes)
    return '{encoded: ' + eb_js + ',\n  ' + 'decoded: ' + db_js + '},\n '


def format_cases_js(cases):
    s = '['
    for case in cases:
        s += format_case_js(case)
    # each case adds ",\n " so remove that
    # >>> "12345678"[:-3]
    # '12345'
    s = s[:-3]
    s += ']'
    return s


###########################
### erlang formatting
###########################

def format_decoded_erl(decoded_bytes):
    '''
    format the bytes as a erl Uint8Array
    '''
    bytes_list = list(decoded_bytes)
    byteslstr = f'{bytes_list}'
    return '<<' + byteslstr[1:-1] + '>>'

def format_encoded_erl(encoded_bytes):
    s = str(encoded_bytes)[2:-1]
    return '"' + s + '"'

def format_case_erl(case):
    decoded_bytes = case['db']
    encoded_bytes = case['eb']
    db_erl = format_decoded_erl(decoded_bytes)
    eb_erl = format_encoded_erl(encoded_bytes)
    return '{{encoded, ' + eb_erl + '},\n ' + '{decoded, ' + db_erl + '}}.\n'

def format_cases_erl(cases):
    s = ''
    for case in cases:
        s += format_case_erl(case)
    return s


###########################
### main
###########################

def main():
    c = simple_cases() + cases(50)
    print(format_cases_js(c))
    #print(format_cases_erl(c))

if __name__ == '__main__':
    main()
