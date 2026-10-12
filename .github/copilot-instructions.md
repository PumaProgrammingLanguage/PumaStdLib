# Copilot Instructions

## Project Guidelines
- Puma String short storage codeUnits supports up to 15 bytes and is not NUL-terminated. Bytes beyond Size() are unspecified leftover garbage. ToUTF8 callers must use Size(); do not reserve a terminator byte or promise NUL termination.