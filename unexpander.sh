#!/bin/sh
# Remove expanded library, implementation and test files
for at in u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 c64 c128; do
    rm -f srfi/160/$at.sld
    rm -f srfi/160/$at-impl.scm
    rm -f srfi/160/base/$at-vector2list.scm
    rm -f srfi.160.$at.scm
done

