#!/usr/bin/env python3.9

'''
generate rlp tests

really checking to see if my code matches the ethereum python rlp package which
has 85 stars on github

requires rlp package, which requires python 3.7 or greater

this checks to see if my Erlang implementation matches python. It works for
59,638 randomly generated test cases
'''

# need more cases
# single bytes
# empty list
# list with variable number of elements
# element has equal chance of becoming a list or a binary

import rlp
import random as r

from rlpcases import cases

def main():
    all_encode_work = True
    all_decode_work = True
    for case in cases:
        decoded_data       = case['decoded']
        encoded_bytes      = case['encoded']
        real_encoded_bytes = rlp.encode(decoded_data)
        real_decoded_data  = rlp.decode(encoded_bytes)
        # check if encode/decode works
        encode_works = (real_encoded_bytes == encoded_bytes)
        decode_works = (real_decoded_data == decoded_data)
        print('encode works: %s' % (encode_works))
        print('decode works: %s' % (decode_works))
        # update globals
        all_encode_work = all_encode_work and encode_works
        all_decode_work = all_decode_work and decode_works
    # print globals
    print('all encode work: %s' % (all_encode_work))
    print('all decode work: %s' % (all_decode_work))
    #print(format_cases_erl(c))

if __name__ == '__main__':
    main()

