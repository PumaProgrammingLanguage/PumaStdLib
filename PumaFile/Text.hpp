#pragma once

#include "../PumaTypes/String.hpp"
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
			WRITE_REPLACE,
			// Open for reading and writing at end of file (appending or creating)
			READ_WRITE
		};

		// Default constructor
		Text() noexcept;
		// Open file at path with mode (default is READ_WRITE)
		Text(const Types::String& path, OpenMode mode = OpenMode::READ_WRITE) noexcept;
		~Text() noexcept;
		// Assign from another Text
		Text& operator=(Text&& other) noexcept;
		// Opens the file at path with mode (default is READ_WRITE)
		bool Open(const Types::String& path, OpenMode mode) noexcept;
		// Returns true if the file is open
		bool IsOpen() const noexcept;

		// Reads the next whitespace-delimited word from the file
		Types::String Read() noexcept;
		// Reads the next line from the file (newline excluded)
		Types::String ReadLn() noexcept;
		// Writes text to the file
		bool Write(const Types::String& text) noexcept;
		// Writes text followed by a newline to the file
		bool WriteLn(const Types::String& text) noexcept;

	private:
		// Closes the file if open
		void Close() noexcept;

		std::FILE* handle;
	};
} // namespace File
} // namespace Puma
