#pragma once

#include "String.hpp"

namespace Puma {
namespace Console
{
    // Writes a Puma String to standard output
    void Write(const Types::String& str) noexcept;

    // Writes a Puma String to standard output followed by a newline
    void WriteLn(const Types::String& str) noexcept;

} // namespace Console
} // namespace Puma