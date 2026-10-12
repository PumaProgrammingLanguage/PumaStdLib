# Feature Progress

- [x] Fix `String::ToUTF8()` to return String-managed storage instead of a stack buffer.
- [x] Preserve pointer validity across repeated calls on an unchanged string while its storage remains alive.
- [x] Provide `Size()` readable UTF-8 bytes without NUL termination; bytes beyond the size are unspecified.
- [x] Preserve the 16-byte String layout and full 15-byte inline capacity; long storage allocates exactly the byte count.
- [x] Validate corrected semantics with a temporary MSVC runtime harness: 0/1/14/15/16/64-byte strings, inline capacity, repeated-call stability, borrowing, embedded NUL, and multibyte UTF-8, reading only Size() bytes.
- [x] Rebuild the complete solution successfully; remove temporary validation artifacts.
- [x] Fix `Text::ReadLn()` to consume one line and exclude its newline without replacing bytes with NUL.
- [x] Preserve exact UTF-8 byte lengths, empty-line/EOF/unopened behavior, long lines, and final unterminated lines.
- [x] Validate ReadLn with a temporary MSVC runtime harness: successive hello/world reads, empty lines/files, EOF, unopened files, 1023/1024/1025/8192-byte lines, final unterminated lines, CRLF text mode, multibyte UTF-8, and embedded NUL; full solution build passed.
- [x] Fix default String copy ownership: long copies are borrowers; explicit transfer from an owner clears source ownership and makes the destination owner; short strings retain value-copy semantics.
- [x] Validate String copy ownership with a temporary MSVC runtime harness: default/explicit borrower copies, copies from borrowers, ownership transfer, borrower/source destruction order, ToString, 0/1/15/16/8192-byte inputs, and unchanged 16-byte layout; full solution build passed.

## General Unit Test TODO

1. [ ] Add compiler-module regression coverage for generated `ToUTF8()`/`Size()` calls when the compiler test workspace is available, including explicit expected C/C++ output. This runtime solution has no registered unit tests.
2. [ ] Add compiler-module regression coverage for successive `Text::ReadLn()` calls and exact `Size()` results, including explicit expected C/C++ output, when the compiler test workspace is available.
3. [ ] Add compiler-module regression coverage for String borrower copies and explicit ownership transfer, including explicit expected C/C++ output, when the compiler test workspace is available.

## Postponed / Special-Feature Unit Test TODO

1. [ ] Readonly semantics.
2. [ ] Optional properties and variables.
3. [ ] fix32 conversions, pending custom C++ fixed-point classes.
4. [ ] fix64 conversions, pending custom C++ fixed-point classes.
5. [ ] forall and container-focused scenarios.
6. [ ] Override behavior.
7. [ ] Range operator `..`.
8. [ ] Boxing/unboxing.
