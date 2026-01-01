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

    // String factory, initializes from a C-style string and returns the initialized string.
    // if cstr is null, return empty string
    String String::initialize(const char* cstr) noexcept
    {
		String newStr;
        // zero the string - this creates an empty string
        newStr.str.firstHalf = 0;
        newStr.str.secondHalf = 0;

        if (!cstr)
        {
			// return empty string
            return newStr;
        }
 
        const std::size_t strSize = std::strlen(cstr);

        if (strSize <= 15)
        {
            // Short string
            // MSB stays 0
            newStr.shortStr.tag = static_cast<std::uint8_t>(strSize & LENGTH_MASK);
            if (strSize > 0)
            {
                memcpy_s(newStr.shortStr.data, sizeof(newStr.shortStr.data), cstr, strSize);
            }
			// else empty string
        }
        else
        {
            // Long string
            newStr.longStr.tag = LONG_MASK; // set MSB=1
            newStr.longStr.strSize = static_cast<std::uint32_t>(strSize);

            char* buf = new (std::nothrow) char[strSize]; // no terminator stored
            if (buf)
            {
                memcpy_s(buf, sizeof(buf), cstr, strSize);
                // Store pointer directly; width matches platform.
                newStr.longStr.ptr = buf;
            }
            else
            {
                // Allocation failed: fall back to empty string
                newStr.str.firstHalf = 0;
                newStr.str.secondHalf = 0;
            }
        }

		return newStr;
    }

    // String factory overload, initializes from another Puma string and returns a copy.
    String String::initialize(String source) noexcept
    {
        String newStr;
        newStr.str.firstHalf = source.str.firstHalf;
        newStr.str.secondHalf = source.str.secondHalf;
        return newStr;
    }

    // delete the long string buffer if any
	// initialize to empty string
    void String::Finalize(bool isOwner) noexcept
    {
		if (isOwner && isLong() && longStr.ptr != nullptr)
        {
            delete[] longStr.ptr;
        }
		// Set to empty string
        str.firstHalf = 0;
		str.secondHalf = 0;
    }

    // Assignment
    // if lvalue is owner, call finalize before the assignment
    void String::operator=(String source) noexcept
    {
        str.firstHalf = source.str.firstHalf;
		str.secondHalf = source.str.secondHalf;
		return;
    }
} // namespace Types
} // namespace Puma