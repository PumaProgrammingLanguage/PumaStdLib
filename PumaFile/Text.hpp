#ifndef PUMA_FILE_TEXT_HPP
#define PUMA_FILE_TEXT_HPP

#pragma once

#include "../PumaType/String.hpp"
#include "../PumaType/Character.hpp"
#include <cstdio>
#include <cstdint>

namespace PumaFile
{
    class Text final
    {
    public:
        enum class OpenMode : std::uint8_t
        {
            // Open for reading
            READ,
            // Open for writing at end of file (appending or creating)
            WRITE,
            // Open for writing (overwriting existing file or creating)
            WRITE_NEW,
            // Open for reading and writing at end of file (appending or creating)
            READ_WRITE,
            // Open for reading and writing at beginning of file (replacing existing file)
            READ_WRITE_NEW
        };

        // Default constructor
        Text() noexcept;
        // Open file at path with mode (default is READ_WRITE)
        Text(const PumaType::String& path, OpenMode mode = OpenMode::READ_WRITE) noexcept;
        ~Text() noexcept;
        // Assign from another Text
        Text& operator=(Text&& other) noexcept;
        // Returns true if the file is open
        bool IsOpen() const noexcept;

        // Reads the next whitespace-delimited word from the file
        PumaType::String Read() noexcept;
        // Reads the next line from the file (newline excluded)
        PumaType::String ReadLn() noexcept;
        // Writes text to the file
        bool Write(const PumaType::String& text) noexcept;
        // Writes a single Character to the file
        bool Write(const PumaType::Character& ch) noexcept;
        // Writes text followed by a newline to the file
        bool WriteLn(const PumaType::String& text) noexcept;
        // Writes a single Character followed by a newline to the file
        bool WriteLn(const PumaType::Character& ch) noexcept;

        // Opens the file at path with mode (default is READ_WRITE)
        bool Open(const PumaType::String& path, OpenMode mode) noexcept;
        // Closes the file if open
        void Close() noexcept;

        std::FILE* handle;
        bool adjustPosition;
    };
} // namespace PumaFile

#endif // PUMA_FILE_TEXT_HPP
