#!/usr/bin/env python

import comprehensive_pb2
test_pb2 = comprehensive_pb2

e1 = test_pb2.Enum.Value('one')
e2 = test_pb2.Enum.Value('two')

s1 = test_pb2.Small()
s1.small_s = 'abc'
s1.small_i = 17

s2 = test_pb2.Small()
s2.small_i = 100

c = test_pb2.Comprehensive()
c.repeated_uint32.extend([1,2])
c.required_int32 = 3
c.required_Small.CopyFrom(s1)
c.required_double = 4.1
c.optional_sfixed32 = 5
c.optional_fixed32 = 6
c.repeated_bytes.extend([b'def', b'gh'])
c.repeated_bool.extend([False, True])
c.repeated_sfixed64.extend([7,8,9])
c.optional_bool = True
c.required_uint32 = 10
c.optional_double = 11.2
c.required_int64 = 12
c.required_uint64 = 13
c.required_string = 'rstuvw'
c.required_bytes = b'lmnopq'
c.optional_bytes = b'rstuv'
c.optional_sint64 = 14
c.repeated_sint64.extend([-15,16,17])
c.repeated_fixed32.extend([18,19,20,21])
c.optional_Small.CopyFrom(s1)
c.optional_int32 = 22
c.optional_fixed64 = 23
c.optional_enum = e1
c.required_float = 24.5
c.optional_sfixed64 = 25
c.required_sfixed32 = 26
c.required_bool = True
c.repeated_fixed64.extend([27,28,29,30,31])
c.optional_sint32 = 32
c.repeated_int64.extend([33,34,35,36,37,38,39])
c.required_fixed64 = 40
c.repeated_enum.extend([e1, e2])
c.optional_int64 = 41
c.repeated_float.extend([42.0])
c.repeated_sint32.extend([44, 45, 46, 47, 48, 49])
c.repeated_uint64.extend([50, 51, 52, 53, 54, 55])
c.repeated_Small.extend([s1, s2])
c.repeated_double.extend([56.3, 57.4, 58.0, 59.1])
c.repeated_string.extend(['w', '', 'yz'])
c.required_sfixed64 = 60
c.required_sint64 = 61
c.optional_string = 'A3'
c.optional_uint32 = 62
c.repeated_sfixed32.extend([63, 64, 65, 66, 67, 68])
c.optional_float = 69.0
c.optional_uint64 = 70
c.required_enum = e2
c.required_sint32 = 71
c.required_fixed32 = 72
c.repeated_int32.extend([73, 74, 75, 76, 77, 78])

def main():
    print('generating comprehensive.serialized')
    with open('comprehensive.python.serialized', 'wb') as fd:
        fd.write(c.SerializeToString())

    print('generating small.serialized')
    with open('small.python.serialized', 'wb') as fd:
        fd.write(s1.SerializeToString())

if __name__ == '__main__':
    main()
