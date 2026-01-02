#include "pch.h"
#include "framework.h"
#include "Console.hpp"
#include <iostream>
#include <string>

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

    // Reads the next whitespace-delimited token from standard input
    Types::String Read() noexcept
    {
        std::string buffer;
        if (!(std::cin >> buffer))
        {
            std::cin.clear();
            return Types::String();
        }

        return Types::String(buffer.c_str());
    }

    // Reads the next line from standard input (newline excluded)
    Types::String ReadLn() noexcept
    {
        std::string buffer;
        if (!std::getline(std::cin, buffer))
        {
            std::cin.clear();
            return Types::String();
        }

        return Types::String(buffer.c_str());
    }

} // namespace Console
} // namespace Puma