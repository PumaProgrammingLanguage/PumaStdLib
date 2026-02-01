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

	// Default-constructs an empty String (no heap allocation).
	String::String() noexcept
	{
		// Zero the packed storage so that all views (short/long) are in a valid empty state.
		packedValues.firstHalf = 0;
		packedValues.secondHalf = 0;
	}

	// Constructs a String from raw UTF‑8 bytes and explicit length in bytes.
	String::String(const char* data, size_t dataSize) noexcept
		: String(reinterpret_cast<const uint8_t*>(data), static_cast<uint32_t>(dataSize))
	{
		// delegation is handled in the initializer list.
	}

	// Constructs a String from raw UTF‑8 bytes and explicit length in bytes.
	String::String(const uint8_t* data, uint32_t dataSize) noexcept
		: String()
	{
		// Treat null data or zero length as empty string.
		if (data == nullptr || dataSize == 0)
		{
			return;
		}

		// Decide between short and long representation.
		if (dataSize <= sizeof(shortStr.codeUnits))
		{
			// Short string: store bytes inline in the union.
			shortStr.tag = static_cast<uint8_t>(dataSize & LENGTH_MASK);
			if (dataSize > 0)
			{
				memcpy(shortStr.codeUnits, data, dataSize);
			}
			// else dataSize is zero, already handled by default constructor.
		}
		else
		{
			// Long string: allocate buffer on the heap.
			longStr.tag = LONG_MASK;
			longStr.strSize = static_cast<uint32_t>(dataSize);

			char* buf = new (nothrow) char[dataSize];
			if (buf != nullptr)
			{
				memcpy(buf, data, dataSize);
				longStr.ptr = reinterpret_cast<const uint8_t*>(buf);
			}
			else
			{
				// Allocation failed, revert to empty string.
				packedValues.firstHalf = 0;
				packedValues.secondHalf = 0;
			}
		}
	}

	// Copy constructor: deep-copies heap storage if necessary.
	String::String(const String& source) noexcept
		: String()
	{
		FromString(source);
	}

	// Destructor: releases heap storage for long strings.
	String::~String() noexcept
	{
		release();
	}

	// Copy assignment: strong self-assignment check, then deep-copy.
	String& String::operator=(const String& source) noexcept
	{
		// If same object, just return.
		if (this == &source)
		{
			return *this;
		}

		FromString(source);
		return *this;
	}

	// Returns true if the string uses the short in-place representation.
    bool String::isShort() const noexcept
    {
        return (shortStr.tag & SHORT_MASK) == 0;
    }

	// Returns true if the string uses the heap-allocated long representation.
    bool String::isLong() const noexcept
    {
        return (longStr.tag & LONG_MASK) != 0;
    }

    // Returns the number of Unicode code points in the string (UTF-8 aware).
    uint32_t String::Length() const noexcept
    {
		uint32_t charCount = 0;
        const uint8_t* ptr;

		// Select the underlying UTF-8 buffer.
        if (isShort())
        {
            ptr = shortStr.codeUnits;
        }
        else
        {
            ptr = longStr.ptr;
		}

		const uint32_t strSize = Size();

		// Walk the UTF-8 sequence using Charactor's length helper.
		for (uint32_t i = 0; i < strSize; )
		{
			const uint8_t c = static_cast<uint8_t>(ptr[i]);
			const uint8_t charSize = Charactor::GetCharSize(c);

			// Advance by one full code point (1–4 bytes).
			i += charSize;
            ++charCount;
        }

        return charCount;
	}

	// Returns the number of bytes occupied by the UTF-8 sequence (not including any terminator).
    uint32_t String::Size() const noexcept
    {
        return isShort()
            ? static_cast<uint32_t>(shortStr.tag & LENGTH_MASK)
            : static_cast<uint32_t>(longStr.strSize);
    }

	// Returns the size of the String object itself.
    uint32_t String::SizeVar() const noexcept
    {
        return sizeof(String);
	}

	// Returns a pointer to the underlying UTF-8 bytes.
	const uint8_t* String::codeUnits() const noexcept
	{
		return isShort() ? shortStr.codeUnits : longStr.ptr;
	}

	// Releases any heap-allocated storage and resets to empty.
	void String::release() noexcept
	{
		if (isLong() && longStr.ptr != nullptr)
		{
			delete[] longStr.ptr;
		}

		packedValues.firstHalf = 0;
		packedValues.secondHalf = 0;
	}

	// Deep-copy from another String instance.
	void String::FromString(const String& source) noexcept
	{
		// Short representation: copy the entire packed union.
		if (source.isShort())
		{
			packedValues.firstHalf  = source.packedValues.firstHalf;
			packedValues.secondHalf = source.packedValues.secondHalf;
			return;
		}

		// Long representation: copy heap storage if present.
		if (source.longStr.ptr != nullptr)
		{
			const size_t bufferSize = static_cast<size_t>(source.longStr.strSize);
			if (bufferSize > 0)
			{
				uint8_t* buffer = new (nothrow) uint8_t[bufferSize];
				if (buffer != nullptr)
				{
					memcpy_s(buffer, bufferSize, source.longStr.ptr, bufferSize);
					longStr.tag     = source.longStr.tag;
					longStr.strSize = static_cast<uint32_t>(bufferSize);
					longStr.ptr     = buffer;
					return;
				}
			}
			// else bufferSize is zero, treat as empty.
		}
		// else source.longStr.ptr is null, treat as empty.

		// If we reach here, copy failed: reset to empty.
		packedValues.firstHalf = 0;
		packedValues.secondHalf = 0;
	}

	// Returns a copy of this String (convenience method).
	String String::ToString() const noexcept
	{
		return String(*this);
	}

	// Iterator support: Set to first code point.
	const uint8_t* String::First() const noexcept
	{
		return codeUnits();
	}

	// Iterator support: one‑past‑the‑last code unit pointer.
	const uint8_t* String::Last() const noexcept
	{
	    const uint8_t* first = First();
	    const uint32_t size = Size();

	    // check for empty
	    if (size == 0)
	    {
	        return nullptr;
	    }

	    const uint8_t* p = first + size; // one beyond last byte of the string

	    // Walk backwards until we find the leading byte of the last UTF‑8 code point.
	    while (p > first)
	    {
	        --p;
	        const uint8_t byte = static_cast<uint8_t>(*p);

	        // UTF‑8 continuation bytes have the form 10xxxxxx (0x80–0xBF).
	        // Leading bytes are anything that is NOT a continuation byte.
	        if ((byte & 0xC0u) != 0x80u)
	        {
	            return p; // start of last character
	        }
	    }

	    // If we fall through, data is malformed).
	    return nullptr;
	}

	// Iterator support: advance to the next code unit (not code point).
	const uint8_t* String::Next(const uint8_t* current) const noexcept
	{
		// Not started yet
		if (current == nullptr)
		{
			return nullptr;
		}

		const uint8_t* first = First();
		const uint8_t* last  = Last();

		// check for empty
		if (last == nullptr)
		{
			return nullptr;
		}

		// Move one UTF-8 code point forward.
		const uint8_t c = static_cast<uint8_t>(*current);
		const uint8_t charSize = Charactor::GetCharSize(c);
		const uint8_t* next = current + charSize;
		if (next > last)
		{
			return nullptr;
		}

		return next;
	}

	// Iterator support: move to the previous code unit (not code point).
	const uint8_t* String::Previous(const uint8_t* current) const noexcept
	{
		// Not started yet
		if (current == nullptr)
		{
			return nullptr;
		}

		const uint8_t* first = First();

		const uint8_t* p = current; // scanning pointer
		// Walk backwards until we find the leading byte of the previous UTF‑8 code point.
		while (p > first)
		{
			--p;
			const uint8_t byte = static_cast<uint8_t>(*p);

			// UTF‑8 continuation bytes have the form 10xxxxxx (0x80–0xBF).
			// Leading bytes are anything that is NOT a continuation byte.
			if ((byte & 0xC0u) != 0x80u)
			{
				return p; // start of previous character
			}
		}
		 
		// If we fall through, the first byte of the previous code point is malformed).
		return nullptr;
	}
} // namespace Types
} // namespace Puma