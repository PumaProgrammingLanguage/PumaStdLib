// PumaTypes.cpp : Defines the functions for the static library.
//
#include "pch.h"
#include "framework.h"
#include "String.hpp"
#include <memory.h>
#include <new>

namespace Puma {
namespace Types
{

	String::String() noexcept
	{
		str.firstHalf = 0;
		str.secondHalf = 0;
	}

	String::String(const char* cstr) noexcept
		: String()
	{
		// handle null input
		if (!cstr)
		{
			return;
		}

		const std::size_t strSize = std::strlen(cstr);

		// decide short vs long string
		if (strSize <= sizeof(shortStr.data))
		{
			// short string
			shortStr.tag = static_cast<std::uint8_t>(strSize & LENGTH_MASK);
			if (strSize > 0)
			{
				memcpy_s(shortStr.data, sizeof(shortStr.data), cstr, strSize);
			}
		}
		else
		{
			// long string
			longStr.tag = LONG_MASK;
			longStr.strSize = static_cast<std::uint32_t>(strSize);

			char* buf = new (std::nothrow) char[strSize];
			if (buf != nullptr)
			{
				// allocation succeeded, copy string data
				memcpy_s(buf, strSize, cstr, strSize);
				longStr.ptr = buf;
			}
			else
			{
				// allocation failed, set to empty string
				str.firstHalf = 0;
				str.secondHalf = 0;
			}
		}
	}

	String::String(const String& source) noexcept
		: String()
	{
		copyFrom(source);
	}

	String::~String() noexcept
	{
		release();
	}

	String& String::operator=(const String& source) noexcept
	{
		// if same object, just return the current object
		if (this == &source)
		{
			return *this;
		}

		copyFrom(source);
		return *this;
	}

	// test for short string
    bool String::isShort() const noexcept
    {
        return (shortStr.tag & SHORT_MASK) == 0;
    }

	// test for long string
    bool String::isLong() const noexcept
    {
        return (longStr.tag & LONG_MASK) != 0;
    }


    // get str length - number of characters (code points)
    std::uint32_t String::Length() const noexcept
    {
		uint32_t charCount = 0;
        const char* ptr;
        if (isShort())
        {
            ptr = shortStr.data;
        }
        else
        {
            ptr = longStr.ptr;
		}

		const std::uint32_t strSize = StrSize();
		for (std::uint32_t i = 0; i < strSize; )
		{
            // Determine the number of bytes in the current UTF-8 character
			// and advance the index accordingly
			unsigned char c = static_cast<unsigned char>(ptr[i]);
			if ((c & 0x80) == 0)
			{
				// 1-byte character (ASCII)
				i += 1;
			}
			else if ((c & 0xE0) == 0xC0)
			{
				// 2-byte character
				i += 2;
			}
			else if ((c & 0xF0) == 0xE0)
			{
				// 3-byte character
				i += 3;
			}
			else if ((c & 0xF8) == 0xF0)
			{
				// 4-byte character
				i += 4;
			}
			else
			{
				// Invalid UTF-8 byte sequence counts as a single character
				i += 1;
			}
            // Count one character
            charCount++;

			// Check for more characters
        }
		// return number of valid character found
        return charCount;
	}

    // get str size - number of bytes used to store the string buffer
    std::uint32_t String::StrSize() const noexcept
    {
        return isShort()
            ? static_cast<std::uint32_t>(shortStr.tag & LENGTH_MASK)
            : static_cast<std::uint32_t>(longStr.strSize);
    }

    // get variable size - number of bytes used to store the variable
    std::uint32_t String::VarSize() const noexcept
    {
        return sizeof(String);
	}

	// get pointer to string data
	const char* String::Data() const noexcept
	{
		return isShort() ? shortStr.data : longStr.ptr;
	}

	void String::release() noexcept
	{
		if (isLong() && longStr.ptr != nullptr)
		{
			delete[] longStr.ptr;
		}

		str.firstHalf = 0;
		str.secondHalf = 0;
	}

	void String::copyFrom(const String& source) noexcept
	{
		// check for short string
		if (source.isShort())
		{
			// copy short string directly
			str.firstHalf = source.str.firstHalf;
			str.secondHalf = source.str.secondHalf;
			return;
		}
		// is long string, check for valid pointer
		if (source.longStr.ptr != nullptr)
		{
			// allocate buffer for long string
			const std::size_t bufferSize = static_cast<std::size_t>(source.longStr.strSize);
			if (bufferSize > 0)
			{
				char* buffer = new (std::nothrow) char[bufferSize];
				// check if allocation succeeded
				if (buffer != nullptr)
				{
					// copy string data
					memcpy_s(buffer, bufferSize, source.longStr.ptr, bufferSize);
					longStr.tag = source.longStr.tag;
					longStr.strSize = static_cast<std::uint32_t>(bufferSize);
					longStr.ptr = buffer;
					return;
				}
			}
		}
		// if we reach here, the copy failed, set to empty string
		str.firstHalf = 0;
		str.secondHalf = 0;
	}
} // namespace Types
} // namespace Puma