#pragma once

#include "../PumaTypes/String.hpp"

namespace Puma {
namespace Console
{
    // Initializes console IO (UTF-8, unsynced stdio)
    void Initialize() noexcept;

    // Restores console IO state changed during Initialize
    void Finalize() noexcept;

    // Writes a Puma String to standard output
    void Write(const Types::String& str) noexcept;

    // Writes a Puma String to standard output followed by a newline
    void WriteLn(const Types::String& str) noexcept;

    // Flushes the standard output buffer
    void Flush() noexcept;

    // Reads the next whitespace-delimited token from standard input
    Types::String Read() noexcept;

    // Reads the next line from standard input (newline excluded)
    Types::String ReadLn() noexcept;

} // namespace Console
} // namespace Puma