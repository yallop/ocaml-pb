#!/usr/bin/env python

import test_pb2

def from_string(msg, filename):
    with open(filename) as fd:
        return msg.FromString(fd.read())

def main():
    small = from_string(test_pb2.Small,
                        'small.ocaml.serialized')
    assert small.small_s == 'abc'
    assert small.small_i == 17

    two = from_string(test_pb2.TwoString,
                      'twostring.ocaml.serialized')
    assert two.two_s == 'abc'
    assert two.two_b == 'def'

    c = from_string(test_pb2.Comprehensive,
                    'comprehensive.ocaml.serialized')
    assert list(c.repeated_uint32) == [1,2]
    assert c.required_int32 == 3
    s1 = c.required_Small
    assert s1.small_s == 'abc'
    assert s1.small_i == 17
    assert c.required_double == 4.1
    assert c.optional_sfixed32 == 5
    assert c.optional_fixed32 == 6
    assert list(c.repeated_bytes) == ['def', 'gh']
    assert list(c.repeated_bool) == [False, True]
    assert list(c.repeated_sfixed64) == [7,8,9]
    assert c.optional_bool == True
    assert c.required_uint32 == 10
    assert c.optional_double == 11.2
    assert c.required_int64 == 12
    assert c.required_uint64 == 13
    assert c.required_string == 'rstuvw'
    assert c.required_bytes == 'lmnopq'
    assert c.optional_bytes == 'rstuv'
    assert c.optional_sint64 == 14
    assert list(c.repeated_sint64) == [-15,16,17]
    assert list(c.repeated_fixed32) == [18,19,20,21]
    s1 = c.optional_Small
    assert s1.small_s == 'abc'
    assert s1.small_i == 17
    assert c.optional_int32 == 22
    assert c.optional_fixed64 == 23
    e1 = c.optional_enum
    assert e1 == test_pb2.Enum.Value('one')
    assert c.required_float == 24.5
    assert c.optional_sfixed64 == 25
    assert c.required_sfixed32 == 26
    assert c.required_bool == True
    assert list(c.repeated_fixed64) == [27,28,29,30,31]
    assert c.optional_sint32 == 32
    assert list(c.repeated_int64) == [33,34,35,36,37,38,100000000039]
    assert c.required_fixed64 == 40
    e2 = test_pb2.Enum.Value('two')
    assert list(c.repeated_enum) == [e1, e2]
    assert c.optional_int64 == 41
    assert list(c.repeated_float) == [42.0]
    assert list(c.repeated_sint32) == [44, 45, 46, 47, 48, 49]
    assert list(c.repeated_uint64) == [50, 51, 52, 53, 54, 55]
    [s1, s2] = c.repeated_Small
    assert s1.small_s == 'abc'
    assert s1.small_i == 17
    assert s2.small_s == ''
    assert s2.small_i == 100
    assert list(c.repeated_double) == [56.3, 57.4, 58.0, 59.1]
    assert list(c.repeated_string) == ['w', '', 'yz']
    assert c.required_sfixed64 == 60
    assert c.required_sint64 == 61
    assert c.optional_string == 'A3'
    assert c.optional_uint32 == 62
    assert list(c.repeated_sfixed32) == [63, 64, 65, 66, 67, 68]
    assert c.optional_float == 69.0
    assert c.optional_uint64 == 70
    assert c.required_enum == e2
    assert c.required_sint32 == 71
    assert c.required_fixed32 == 72
    assert list(c.repeated_int32) == [73, 74, 75, 76, 77, 78]


if __name__ == '__main__':
    main()
