#!/bin/sh
# Expand library, implementation and test files
for at in u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 c64 c128; do
    sed "s/@/$at/g" srfi/160/at.sld >srfi/160/$at.sld
    sed "s/@/$at/g" srfi/160/at-impl.scm >srfi/160/$at-impl.scm
    sed "s/@/$at/g" srfi/160/base/at-vector2list.scm >srfi/160/base/$at-vector2list.scm
    sed "s/@/$at/g" srfi/160/base/at-vector2list.scm >srfi/160/base/$at-vector2list.scm
    sed "s/@/$at/g" srfi.160.at.scm >srfi.160.$at.scm
done

