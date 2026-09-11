#ifndef PUMA_CONSOLE_CONSOLE_HPP
#define PUMA_CONSOLE_CONSOLE_HPP

#pragma once

#include "../PumaType/String.hpp"
#include "../PumaType/Character.hpp"
#include <cstdint>

namespace PumaConsole
{
    // Initializes console IO (UTF-8, unsynced stdio)
    void Initialize() noexcept;

    // Restores console IO state changed during Initialize
    void Finalize() noexcept;

    // Writes a Puma String to standard output
    void Write(const PumaType::String& str) noexcept;

    // Writes a uint8_t string to standard output
    void Write(const uint8_t* str, const uint32_t size) noexcept;

    // Writes a C-string to standard output
    void Write(const char* cstr) noexcept;

    // Writes a single Puma Character to standard output
    void Write(const PumaType::Character& ch) noexcept;

    // Writes a Puma String to standard output followed by a newline
    void WriteLn(const PumaType::String& str) noexcept;

    // Writes a C-string to standard output followed by a newline
    void WriteLn(const char* cstr) noexcept;

    // Writes a single Puma Character followed by a newline
    void WriteLn(const PumaType::Character& ch) noexcept;

    // Flushes the standard output buffer
    void Flush() noexcept;

    // Reads the next whitespace-delimited token from standard input
    PumaType::String Read() noexcept;

    // Reads the next line from standard input (newline excluded)
    PumaType::String ReadLn() noexcept;

    class CommandPrompt
    {
    public:
        CommandPrompt() noexcept;
        ~CommandPrompt() noexcept;

    private:
        // Internal state
        bool m_visible;
    };
} // namespace PumaConsole

#endif // PUMA_CONSOLE_CONSOLE_HPP