// PumaType.cpp : Defines the functions for the static library.
//
#include "pch.h"
#include "framework.h"
#include "String.hpp"
#include <memory.h>
#include <new>

namespace PumaType
{

	// Default-constructs an empty String (no heap allocation).
	String::String() noexcept
	{
		// Initialize the union to represent an empty string (short string with length 0).
		packedValues = {};
	}

	// Constructs a String from raw UTF‑8 bytes and explicit length in bytes.
	String::String(const char* data, size_t dataSize) noexcept
		: String(reinterpret_cast<const uint8_t*>(data), dataSize)
	{
	}

	// Constructs a String from raw UTF‑8 bytes and explicit length in bytes.
	String::String(const uint8_t* data, size_t dataSize) noexcept
		: String()
	{
		if (data == nullptr || dataSize == 0)
		{
			// Empty string: no heap allocation needed, just leave the default empty state.
			return;
		}
		// Check if the string can be stored as a short string (fits within the codeUnits array).
		if (dataSize <= sizeof(shortStr.codeUnits))
		{
			// Short string: store directly in the union (no heap allocation).
			// Long flag and owner flags are already 0. 
			
			// Length is stored in the lower 4 bits of the tag.
			shortStr.tag = static_cast<uint8_t>(dataSize & LENGTH_MASK);
			if (dataSize > 0)
			{
				// Copy the string data into the codeUnits array.
				memcpy(shortStr.codeUnits, data, dataSize);
			}
		}
		else
		{
			uint8_t* buf = new (nothrow) uint8_t[dataSize];
			if (buf != nullptr)
			{
				// Copy the string data into the newly allocated buffer.
				memcpy(buf, data, dataSize);
				longStr.ptr = buf;
				// Set as long string with ownership.
				longStr.tag = LONG_OWNER_FLAG;
				// Store the string size in bytes.
				longStr.strSize = static_cast<uint32_t>(dataSize);
			}
			else
			{
				// Allocation failed: fallback to empty string (no heap allocation).
				packedValues = {};
			}
		}
	}

	String::String(String& source, bool moveOwner) noexcept
		: String()
	{
		// Copy the source string's data into this string.
		packedValues = source.packedValues;
		SetBorrower();

		// check if we should transfer ownership
		if(moveOwner && source.isLongOwner())
		{
			// transfer ownership of the string data from the source to this string.
			// Set the source string as a borrower (non-owner) since ownership is being transferred.
			source.SetBorrower();
			// Set this string as the new owner of the data.
			SetOwner();
		}
	}

	// Constructs a String from a PumaType::Character.
	String::String(const PumaType::Character& ch) noexcept
		: String()
	{
		packedValues = ch.ToString().packedValues;
		// If the temporary String was a long owner, we need to set this String as an owner as well.
		SetOwner();
	}

	String::~String() noexcept
	{
		release();
	}

	String& String::operator=(const String& source) noexcept
	{
		if (this == &source)
		{
			return *this;
		}

		// Release current resources before copying new data.
		release();
		// Copy the source string's data into this string.
		packedValues = source.packedValues;
		// String assignments doesn't transfer ownership.
		if (isLongOwner())
		{
			SetBorrower();
		}
		return *this;
	}

	// Append a string to this string (concatenation). Returns a new String with the combined content.
	String& String::operator+=(const String& other) noexcept
	{
		// Implementation of operator+=
		size_t stringSize = Size() + other.Size();

		// Check if the string can be stored as a short string (fits within the codeUnits array).
		if (stringSize <= sizeof(shortStr.codeUnits))
		{
			// Short string: store directly in the union (no heap allocation).
			// Long flag and owner flags are already 0. 

			if (stringSize > 0)
			{
				// append the other string data into the codeUnits array starting at one beyond the current size.
				memcpy(shortStr.codeUnits + Size(), other.shortStr.codeUnits, other.Size());
			}
			else
			{
				// If the resulting string is empty, just set the tag to 0 which sets the length to 0.
				shortStr.tag = 0;
			}
			// Update the length after copying so Size() still gives the original append offset.
			shortStr.tag = static_cast<uint8_t>(stringSize & LENGTH_MASK);
		}
		else
		{
			if (isLongOwner())
			{
				// If this string is a long owner, we can reallocate the existing buffer to accommodate the new size.
				uint8_t* newBuf = new (nothrow) uint8_t[stringSize];
				if (newBuf != nullptr)
				{
					// Copy the existing string data into the new buffer.
					memcpy(newBuf, longStr.ptr, Size());
					// Append the other string data into the new buffer.
					memcpy(newBuf + Size(), other.ToUTF8(), other.Size());
					// Free the old buffer.
					delete[] longStr.ptr;
					// Update the pointer and size.
					longStr.ptr = newBuf;
					longStr.strSize = static_cast<uint32_t>(stringSize);
				}
				// else allocation failed: keep the original string.
			}
			else
			{
				// If this string is not a long owner, we need to allocate a new buffer for the concatenated result.
				uint8_t* buf = new (nothrow) uint8_t[stringSize];
				if (buf != nullptr)
				{
					// Copy the existing string data into the new buffer.
					memcpy(buf, ToUTF8(), Size());
					// Append the other string data into the new buffer.
					memcpy(buf + Size(), other.ToUTF8(), other.Size());
					longStr.ptr = buf;
					longStr.tag = LONG_OWNER_FLAG;
					longStr.strSize = static_cast<uint32_t>(stringSize);
				}
				// else allocation failed: keep the original string.
			}
		}
		
		return *this;
	}

	// Concatenation operator - returns a new String that is the concatenation of two strings.
	String String::operator+(const String& other) const noexcept
	{
		String temp(ToUTF8(), Size()); // Create an independent copy of the current string
		temp += other; // Use operator+= to append the other string
		return String(temp, true); // Transfer ownership before the temporary is destroyed
	}


	bool String::isShort() const noexcept
	{
		// A short string is identified by the LONG_FLAG bit not set (SHORT_FLAG) in the tag.
		return (shortStr.tag & SHORT_MASK) == SHORT_FLAG;
	}

	bool String::isLong() const noexcept
	{
		// A long string is identified by the LONG_FLAG bit set in the tag.
		return (longStr.tag & LONG_MASK) == LONG_FLAG;
	}

	bool String::isLongOwner() const noexcept
	{
		// A long string is an owner if both the LONG_FLAG and OWNER_FLAG bits are set in the tag.
		return (longStr.tag & LONG_OWNER_MASK) == LONG_OWNER_FLAG;
	}

	void String::SetOwner() noexcept
	{
		// Only applicable for long strings; short strings are value types (stored directly in the union).
		if (isLong())
		{
			// Set the owner flag for long strings.
			longStr.tag |= OWNER_FLAG;
		}
		// Short strings are value types and do not have an owner flag, so we do nothing for short strings.
	}

	void String::SetBorrower() noexcept
	{
		// Only applicable for long strings; short strings are value types (stored directly in the union).
		if (isLong())
		{
			// Clear the owner flag for long strings.
			longStr.tag &= ~OWNER_FLAG;
		}
		// Short strings are value types and do not have an owner flag, so we do nothing for short strings.
	}

	bool String::IsOwner() const noexcept
	{
		// Only applicable for long strings; short strings are value types (stored directly in the union).
		return isLongOwner();
	}

	uint32_t String::Length() const noexcept
	{
		uint32_t charCount = 0;
		const uint8_t* ptr = isShort() ? shortStr.codeUnits : longStr.ptr;
		const size_t strSize = Size();

		for (size_t i = 0; i < strSize; )
		{
			const uint8_t c = ptr[i];
			const uint8_t charSize = Character::GetCharSize(c);
			i += charSize;
			++charCount;
		}

		return charCount;
	}

	size_t String::Size() const noexcept
	{
		return isShort()
			? static_cast<size_t>(shortStr.tag & LENGTH_MASK)
			: static_cast<size_t>(longStr.strSize);
	}

	size_t String::SizeVar() const noexcept
	{
		return sizeof(String);
	}

	// Returns a pointer to the UTF-8 bytes of the string.
	// Do not delete the returned pointer; it is managed by the String class.
	// The returned pointer is valid as long as the String object is alive and not modified.
	// Return string that is not NUL-terminated; use Size() to determine the length in bytes.
	const uint8_t* String::ToUTF8() const noexcept
	{
		return isShort() ? shortStr.codeUnits : longStr.ptr;
	}

	void String::release() noexcept
	{
		if (isLongOwner() && longStr.ptr != nullptr)
		{
			delete[] longStr.ptr;
		}

		packedValues = {};
	}

	String String::ToString() noexcept
	{
		// Create a new String with the same content as this one (copy constructor semantics).
		return String(*this);
	}

	// Iterator support: first code unit.
	StringIterator String::First() const noexcept
	{
		return StringIterator(*this);
	}

	// Iterator support: iterator to first byte of last UTF‑8 character (or invalid if empty/malformed).
	StringIterator String::Last() const noexcept
	{
	    const uint8_t* first = (const uint8_t*)ToUTF8();
	    const size_t size  = Size();

	    if (size == 0 || first == nullptr)
	    {
	        return StringIterator();
	    }

	    const uint8_t* p = first + size;

	    while (p > first)
	    {
	        --p;
	        const uint8_t byte = *p;

	        if ((byte & 0xC0u) != 0x80u)
	        {
	            return StringIterator(p, first);
	        }
	    }

	    // malformed; no valid leading byte found
	    return StringIterator();
	}

	// Iterator support: advance to the next UTF‑8 code point.
	StringIterator String::Next(const StringIterator& current) const noexcept
	{
		if (!current.IsValid())
		{
			return StringIterator();
		}

		// Copy, then advance one UTF‑8 character.
		StringIterator nextIt = current;
		++nextIt;

		return nextIt;
	}

	// Iterator support: move to the previous UTF‑8 code point.
	StringIterator String::Previous(const StringIterator& current) const noexcept
	{
		if (!current.IsValid())
		{
			return StringIterator();
		}

		// Copy and move back one UTF‑8 character using the iterator's operator--().
		StringIterator prevIt = current;
		--prevIt;

		return prevIt;
	}

} // namespace PumaType