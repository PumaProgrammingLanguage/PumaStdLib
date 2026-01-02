#pragma once

#include "../PumaTypes/String.hpp"

namespace Puma {
namespace Console
{
    // Writes a Puma String to standard output
    void Write(const Types::String& str) noexcept;

    // Writes a Puma String to standard output followed by a newline
    void WriteLn(const Types::String& str) noexcept;

    // Reads the next whitespace-delimited token from standard input
    Types::String Read() noexcept;

    // Reads the next line from standard input (newline excluded)
    Types::String ReadLn() noexcept;

} // namespace Console
} // namespace Puma