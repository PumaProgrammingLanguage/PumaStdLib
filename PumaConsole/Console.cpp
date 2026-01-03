#include "pch.h"
#include "framework.h"
#include "Console.hpp"
#include <iostream>
#include <string>
#if defined(_WIN32)
#include <windows.h>

namespace
{
    UINT g_originalOutputCodePage = 0;
    UINT g_originalInputCodePage = 0;
    bool g_codePageSaved = false;
}
#endif

namespace Puma {
namespace Console
{
    void Initialize() noexcept
    {
    #if defined(_WIN32)
        if (!g_codePageSaved)
        {
            g_originalOutputCodePage = GetConsoleOutputCP();
            g_originalInputCodePage = GetConsoleCP();
            g_codePageSaved = true;
        }
        SetConsoleOutputCP(CP_UTF8);
        SetConsoleCP(CP_UTF8);
    #endif
        std::ios::sync_with_stdio(false);
    }

    void Finalize() noexcept
    {
    #if defined(_WIN32)
        if (g_codePageSaved)
        {
            SetConsoleOutputCP(g_originalOutputCodePage);
            SetConsoleCP(g_originalInputCodePage);
            g_codePageSaved = false;
        }
    #endif
    }

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
		// Add newline, does not flush
        std::cout << '\n';
    }

    void Flush() noexcept
    {
        std::cout.flush();
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