#include "pch.h"
#include "framework.h"
#include "Text.hpp"
#include <algorithm>
#include <string>

namespace PumaFile
{
	errno_t errorCode = 0;

	// Default constructor
	Text::Text() noexcept
		: handle(nullptr), adjustPosition(false)
	{
		return;
	}

	// Open file at path with mode (default is READ_WRITE)
	Text::Text(const PumaType::String& path, OpenMode mode) noexcept
		: handle(nullptr), adjustPosition(false)
	{
		Open(path, mode);
	}

	// Closes the file if open
	Text::~Text() noexcept
	{
		Close();
	}

	// Assign from another Text
	Text& Text::operator=(Text&& other) noexcept
	{
		if (this != &other)
		{
			Close();
			handle = other.handle;
		}

		return *this;
	}

	// Opens the file at path with mode (default is READ_WRITE)
  bool Text::Open(const PumaType::String& path, OpenMode mode) noexcept
	{
		if (path.Size() == 0)
		{
			return false;
		}

		Close();

		string native(path.ToUTF8(), path.ToUTF8() + path.Size());
		replace(native.begin(), native.end(), '/', '\\');

		const char* modeString = nullptr;
		switch (mode)
		{
		case OpenMode::READ:
			// Open for reading (starting at beginning of file)
			modeString = "r";
			break;
		case OpenMode::WRITE:
			// Open for writing at end of file (appending or creating)
			modeString = "a";
			break;
		case OpenMode::WRITE_NEW:
			// Open for writing at beginning of file (replacing existing file)
			modeString = "w";
			break;
		case OpenMode::READ_WRITE:
			// Open for reading and writing at end of file (appending or creating)
			modeString = "a+";
			adjustPosition = true;
			break;
		case OpenMode::READ_WRITE_NEW:
			// Open for reading and writing at beginning of file (replacing existing file)
			modeString = "w+";
			break;
		default:
			return false;
		}

#if defined(_MSC_VER)
		// Use fopen_s on MSVC
		FILE* tempHandle = nullptr;
		if (errorCode = fopen_s(&tempHandle, native.c_str(), modeString) == 0) {
			handle = tempHandle;
		} else {
			handle = nullptr;
		}
#else
		// Use fopen on other compilers
		handle = fopen(native.c_str(), modeString);
#endif

		return handle != nullptr;
	}

	// Closes the file if open
	bool Text::IsOpen() const noexcept
	{
		return handle != nullptr;
	}

	// Closes the file if open
	void Text::Close() noexcept
	{
		if (handle != nullptr)
		{
			fclose(handle);
			handle = nullptr;
		}
	}


	// Reads the next whitespace-delimited word from the file
 PumaType::String Text::Read() noexcept
	{
		if (handle != nullptr)
		{
			string result;
			int c;
			while ((c = fgetc(handle)) != EOF)
			{
				// end of word on whitespace
				if (isspace(c))
					break;
				result += static_cast<char>(c);
			}
			return PumaType::String(result.data(), result.size());
		}
		else
		{
			// File not open
			return PumaType::String();
		}
	}

	// Read until end of line or end of file
	// Returns the line read (newline excluded)
	PumaType::String Text::ReadLn() noexcept
	{
		if (handle != nullptr)
		{
			string result;
			int c;
			while ((c = fgetc(handle)) != EOF && c != '\n')
			{
				result += static_cast<char>(c);
			}

			return PumaType::String(result.data(), result.size());
		}
		else
		{
			// File not open return an empty String
			return PumaType::String();
		}
	}

	// Writes text to the file
    bool Text::Write(const PumaType::String& text) noexcept
	{
		if (handle == nullptr)
		{
			return false;
		}

		const size_t size = text.Size();
		if (size == 0)
		{
			return true;
		}

		const size_t written = fwrite(text.ToUTF8(), 1, size, handle);
		return written == size;
	}

	// Writes a single Character to the file
   bool Text::Write(const PumaType::Character& ch) noexcept
	{
		// Convert Character to a one-character String and reuse existing Write
		return Write(ch.ToString());
	}

	// Writes text followed by a newline to the file
  bool Text::WriteLn(const PumaType::String& text) noexcept
	{
		if (!Write(text))
		{
			return false;
		}

		return fputc('\n', handle) != EOF;
	}

	// Writes a single Character followed by a newline to the file
 bool Text::WriteLn(const PumaType::Character& ch) noexcept
	{
		if (!Write(ch))
		{
			return false;
		}

		return fputc('\n', handle) != EOF;
	}

} // namespace PumaFile