# Vanillae RLP library

This implements [Ethereum's RLP codec](https://zxq9.com/archives/2749)

The decoded data ("decoded" from the perspective of the program using the data)
is arbitrary-depth arbitrary-length lists-of-lists-of-...-binaries (lists and
binaries are allowed to be empty). The encoded data is of course a single flat
binary string.

It has been tested against Ethereum's official Python implementation.

You can see the methodology in [`tests/vrlp_testgen.erl`](tests/vrlp_testgen.erl):

- 10,000 decoded data structures are generated at random
- the test generator calls `vrlp:encode/1` to generate an encode/decode pair
- assert that encode -> decode round trips properly in the `vrlp` module
- print out an encode-decode pair as a python term to stdout
- python program tests the encode-decode pairs against Ethereum's library

The heart of the methodology is here:

```erl
% input: decoded_data
% format a case as
% {'decoded_data': <python term for rlist>,
%  'encoded_bytes': bytes([B1, B2, B3, ...])}
format_case_py(DecodedData_rlist) ->
    % EncodedData_bytes = rlp:encode(
    % DD_js = format_data_js(DecodedData_rlist),
    EncodedData_bytes          = vrlp:encode(DecodedData_rlist),
    % assert encode -> decode round trips
    {DecodedData_rlist, <<>>}  = vrlp:decode(EncodedData_bytes),
    EncodedBytes_py   = format_bytes_py(EncodedData_bytes),
    DecodedData_py    = format_data_py(DecodedData_rlist),
    ["    {'decoded': ", DecodedData_py, ",\n",
     "     'encoded': ", EncodedBytes_py, "}"].
```

The Python side from [`tests/rlptests.py`](tests/rlptests.py):

```python
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
```
