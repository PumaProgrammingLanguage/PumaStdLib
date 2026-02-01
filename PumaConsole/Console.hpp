#pragma once

#include "../PumaTypes/String.hpp"
#include "../PumaTypes/Charactor.hpp"

namespace Puma {
namespace Console
{
	// Initializes console IO (UTF-8, unsynced stdio)
    void Initialize() noexcept;

	// Restores console IO state changed during Initialize
    void Finalize() noexcept;

    // Writes a Puma String to standard output
    void Write(const Types::String& str) noexcept;

    // Writes a uint8_t string to standard output
    void Write(const uint8_t* cstr) noexcept;

    // Writes a C-string to standard output
	void Write(const char* cstr) noexcept;

    // Writes a single Puma Charactor to standard output
    void Write(const Types::Charactor& ch) noexcept;

    // Writes a Puma String to standard output followed by a newline
    void WriteLn(const Types::String& str) noexcept;

	// Writes a C-string to standard output followed by a newline
	void WriteLn(const char* cstr) noexcept;

    // Writes a single Puma Charactor followed by a newline
    void WriteLn(const Types::Charactor& ch) noexcept;

    // Flushes the standard output buffer
    void Flush() noexcept;

    // Reads the next whitespace-delimited token from standard input
    Types::String Read() noexcept;

    // Reads the next line from standard input (newline excluded)
    Types::String ReadLn() noexcept;

	class CommandPrompt
    {
    public:
        CommandPrompt() noexcept;
        ~CommandPrompt() noexcept;

    private:
        // Internal state
        bool m_visible;
    };
} // namespace Console
} // namespace Puma