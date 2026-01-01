#include "pch.h"
#include "framework.h"
#include "Console.hpp"
#include <iostream>

namespace Puma {
namespace Console
{
    // Writes a Puma String to standard output
    void Write(const Types::String& str) noexcept
    {
        const std::uint32_t strSize = str.StrSize();
        
        if (strSize == 0)
        {
            // Empty string, nothing to output
            return;
        }

        // Get pointer to string data using public accessor
        const char* ptr = str.Data();

        // Write the string bytes to stdout (no null terminator needed)
        std::cout.write(ptr, strSize);
    }

    // Writes a Puma String to standard output followed by a newline
    void WriteLn(const Types::String& str) noexcept
    {
        Write(str);
        std::cout << std::endl;
    }

} // namespace Console
} // namespace Puma