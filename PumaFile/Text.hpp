#ifndef PUMA_FILE_TEXT_HPP
#define PUMA_FILE_TEXT_HPP

#pragma once

#include "../PumaTypes/String.hpp"
#include "../PumaTypes/Charactor.hpp"
#include <cstdio>
#include <cstdint>

namespace Puma {
namespace File
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
        Text(const Type::String& path, OpenMode mode = OpenMode::READ_WRITE) noexcept;
        ~Text() noexcept;
        // Assign from another Text
        Text& operator=(Text&& other) noexcept;
        // Returns true if the file is open
        bool IsOpen() const noexcept;

        // Reads the next whitespace-delimited word from the file
        Type::String Read() noexcept;
        // Reads the next line from the file (newline excluded)
        Type::String ReadLn() noexcept;
        // Writes text to the file
        bool Write(const Type::String& text) noexcept;
        // Writes a single Charactor to the file
        bool Write(const Type::Charactor& ch) noexcept;
        // Writes text followed by a newline to the file
        bool WriteLn(const Type::String& text) noexcept;
        // Writes a single Charactor followed by a newline to the file
        bool WriteLn(const Type::Charactor& ch) noexcept;

    private:
        // Opens the file at path with mode (default is READ_WRITE)
        bool open(const Type::String& path, OpenMode mode) noexcept;
        // Closes the file if open
        void close() noexcept;

        std::FILE* handle;
        bool adjustPosition;
    };
} // namespace File
} // namespace Puma

#endif // PUMA_FILE_TEXT_HPP
