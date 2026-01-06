#include "pch.h"
#include "framework.h"
#include "Text.hpp"
#include <algorithm>
#include <string>

namespace Puma {
namespace File
{
	Text::Text() noexcept
		: handle(nullptr)
	{
	}

	// Open file at path with mode (default is READ_WRITE)
	Text::Text(const Types::String& path, OpenMode mode) noexcept
		: handle(nullptr)
	{
		open(path, mode);
	}

	// Closes the file if open
	Text::~Text() noexcept
	{
		close();
	}

	// Assign from another Text
	Text& Text::operator=(Text&& other) noexcept
	{
		if (this != &other)
		{
			close();
			handle = other.handle;
		}

		return *this;
	}

	// Opens the file at path with mode (default is READ_WRITE)
	bool Text::open(const Types::String& path, OpenMode mode) noexcept
	{
		if (path.StrSize() == 0)
		{
			return false;
		}

		close();

		std::string native(path.BeginConst(), path.EndConst());
		std::replace(native.begin(), native.end(), '/', '\\');

		const char* modeString = nullptr;
		switch (mode)
		{
		case OpenMode::READ:
			// Open for reading
			modeString = "r";
			break;
		case OpenMode::WRITE:
			// Open for writing at end of file (appending or creating)
			modeString = "a";
			break;
		case OpenMode::WRITE_REPLACE:
			// Open for writing (overwriting existing file)
			modeString = "w";
			break;
		case OpenMode::READ_WRITE:
			// Open for reading and writing at end of file (appending or creating)
			modeString = "a+";
			break;
		default:
			return false;
		}

#if defined(_MSC_VER)
		// Use fopen_s on MSVC
		FILE* tempHandle = nullptr;
		if (fopen_s(&tempHandle, native.c_str(), modeString) != 0) {
			handle = nullptr;
		} else {
			handle = tempHandle;
		}
#else
		// Use fopen on other compilers
		handle = std::fopen(native.c_str(), modeString);
#endif

		return handle != nullptr;
	}

	// Closes the file if open
	bool Text::IsOpen() const noexcept
	{
		return handle != nullptr;
	}

	// Reads the next whitespace-delimited word from the file
	Types::String Text::Read() noexcept
	{
		if (handle != nullptr)
		{
			std::string result;
			char buffer[1024];
#if defined(_MSC_VER)
			if (fscanf_s(handle, "%1023s", buffer, static_cast<unsigned int>(sizeof(buffer))) == 1)
#else
			if (fscanf(handle, "%1023s", buffer) == 1)
#endif
			{
				buffer[1023] = '\0';
				result = buffer;
			}
			return Types::String(result.c_str());
		}
		else
		{
			// File not open
			return Types::String();
		}
	}

	// Read until end of line or end of file
	Types::String Text::ReadLn() noexcept
	{
		if (handle != nullptr)
		{
			std::string result;
			char buffer[1024];
			while (std::fgets(buffer, sizeof(buffer), handle) != nullptr)
			{
				result += buffer;
			}

			return Types::String(result.c_str());
		}
		else
		{
			// File not open
			return Types::String();
		}
	}

	// Writes text to the file
	bool Text::Write(const Types::String& text) noexcept
	{
		if (handle == nullptr)
		{
			return false;
		}

		const std::uint32_t size = text.StrSize();
		if (size == 0)
		{
			return true;
		}

		const std::size_t written = std::fwrite(text.BeginConst(), sizeof(char), size, handle);
		return written == size;
	}

	// Writes text followed by a newline to the file
	bool Text::WriteLn(const Types::String& text) noexcept
	{
		if (!Write(text))
		{
			return false;
		}

		return std::fputc('\n', handle) != EOF;
	}

} // namespace File
} // namespace Puma