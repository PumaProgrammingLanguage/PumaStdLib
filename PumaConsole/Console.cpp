#include "pch.h"
#include "framework.h"
#include "Console.hpp"
#include <iostream>
#include <string>
#include <cstdio>
#if defined(_WIN32)
#include <windows.h>

namespace
{
    UINT g_originalOutputCodePage = 0;
    UINT g_originalInputCodePage = 0;
    bool g_codePageSaved = false;
}
#endif

using namespace std;

namespace PumaConsole
{
    CommandPrompt* commandPrompt = nullptr;

	// Initializes console IO (UTF-8, unsynced stdio)
	void Initialize() noexcept
    {
		// Already initialized
        if (commandPrompt != nullptr)
			return;

        commandPrompt = new CommandPrompt();
	}

	// Restores console IO state changed during Initialize
	void Finalize() noexcept
    {
		// Already finalized
		if (commandPrompt == nullptr)
			return;

        delete commandPrompt;
        commandPrompt = nullptr;
    }

    // Writes a Puma String to standard output
    void Write(const PumaType::String& str) noexcept
    {
        const uint32_t strSize = str.Size();

        if (strSize == 0)
        {
            // Empty string, nothing to output
            return;
        }

        // Add Ptr() method to StringIterator if not present
        cout.write(reinterpret_cast<const char*>(str.ToUTF8()), strSize);
    }

    // Writes a uint8_t string to standard output
    void Write(const uint8_t* str, const uint32_t size) noexcept
    {
        if (str == nullptr || size == 0)
        {
            return;
        }

        // Write the string bytes to stdout (no null terminator needed)
        cout.write(reinterpret_cast<const char*>(str), size);
    }

    // Writes a C-string to standard output
    void Write(const char* cstr) noexcept
    {
        if (cstr == nullptr)
        {
            return;
        }

        // Write the string bytes to stdout (no null terminator needed)
        cout.write(cstr, strlen(cstr));
    }

    // Writes a single Puma Character to standard output
    void Write(const PumaType::Character& ch) noexcept
    {
        const PumaType::String tmp = ch.ToString();
        Write(tmp);
    }

    // Writes a Puma String to standard output followed by a newline
    void WriteLn(const PumaType::String& str) noexcept
    {
        Write(str);
		// Add newline, does not flush
        Write("\n");
    }

    // Writes a uint8_t string to standard output
    void WriteLn(const uint8_t* str, const uint32_t size) noexcept
    {
        if (str && size > 0)
        {
            std::fwrite(reinterpret_cast<const char*>(str), sizeof(uint8_t), size, stdout);
        }
        Write("\n");
    }

    // Writes a C-string to standard output followed by a newline
    void WriteLn(const char* cstr) noexcept
    {
        if (cstr == nullptr)
        {
            return;
        }

        WriteLn(PumaType::String(cstr, strlen(cstr)));
    }

    // Writes a single Puma Character followed by a newline
    void WriteLn(const PumaType::Character& ch) noexcept
    {
        Write(ch);
        Write("\n");
    }

    void Flush() noexcept
    {
        cout.flush();
    }

    // Reads the next whitespace-delimited token from standard input
    PumaType::String Read() noexcept
    {
        string buffer;
        if (!(cin >> buffer))
        {
            cin.clear();
            return PumaType::String();
        }

        return PumaType::String(buffer.c_str(), buffer.size());
    }

    // Reads the next line from standard input (newline excluded)
    PumaType::String ReadLn() noexcept
    {
        string buffer;
        if (!getline(cin, buffer))
        {
            cin.clear();
            return PumaType::String();
        }

        return PumaType::String(buffer.c_str(), buffer.size());
    }

    // Pauses execution until the user presses any key
    void Pause() noexcept
    {
        cin.get();
    }


    // Initializes console IO (UTF-8, unsynced stdio)
    CommandPrompt::CommandPrompt() noexcept
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
        ios::sync_with_stdio(false);

        // Show command prompt
		m_visible = true;
    }

    // Restores console IO state changed during Initialize
	CommandPrompt::~CommandPrompt() noexcept
    {
		if (!m_visible)
            return;

#if defined(_WIN32)
        if (g_codePageSaved)
        {
            SetConsoleOutputCP(g_originalOutputCodePage);
            SetConsoleCP(g_originalInputCodePage);
            g_codePageSaved = false;
        }
#endif

        // Hide command prompt
		m_visible = false;
	}
} // namespace PumaConsole