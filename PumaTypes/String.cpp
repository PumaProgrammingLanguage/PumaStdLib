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
		: m_currentConst(nullptr)
	{
		// Zero the packed storage so that all views (short/long) are in a valid empty state.
		packedValues.firstHalf = 0;
		packedValues.secondHalf = 0;
	}

	// Constructs a String from raw UTF‑8 bytes and explicit length in bytes.
	String::String(const char* data, std::size_t dataSize) noexcept
		: String(reinterpret_cast<const std::uint8_t*>(data), static_cast<std::uint32_t>(dataSize))
	{
		// delegation is handled in the initializer list.
	}

	// Constructs a String from raw UTF‑8 bytes and explicit length in bytes.
	String::String(const std::uint8_t* data, std::uint32_t dataSize) noexcept
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
			shortStr.tag = static_cast<std::uint8_t>(dataSize & LENGTH_MASK);
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
			longStr.strSize = static_cast<std::uint32_t>(dataSize);

			char* buf = new (std::nothrow) char[dataSize];
			if (buf != nullptr)
			{
				memcpy(buf, data, dataSize);
				longStr.ptr = buf;
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
		fromString(source);
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

		fromString(source);
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
    std::uint32_t String::Length() const noexcept
    {
		std::uint32_t charCount = 0;
        const char* ptr;

		// Select the underlying UTF-8 buffer.
        if (isShort())
        {
            ptr = shortStr.codeUnits;
        }
        else
        {
            ptr = longStr.ptr;
		}

		const std::uint32_t strSize = Size();

		// Walk the UTF-8 sequence using Charactor's length helper.
		for (std::uint32_t i = 0; i < strSize; )
		{
			const std::uint8_t c = static_cast<std::uint8_t>(ptr[i]);
			const std::uint8_t charSize = Charactor::GetCharSize(c);

			// Advance by one full code point (1–4 bytes).
			i += charSize;
            ++charCount;
        }

        return charCount;
	}

	// Returns the number of bytes occupied by the UTF-8 sequence (not including any terminator).
    std::uint32_t String::Size() const noexcept
    {
        return isShort()
            ? static_cast<std::uint32_t>(shortStr.tag & LENGTH_MASK)
            : static_cast<std::uint32_t>(longStr.strSize);
    }

	// Returns the size of the String object itself.
    std::uint32_t String::SizeVar() const noexcept
    {
        return sizeof(String);
	}

	// Returns a pointer to the underlying UTF-8 bytes.
	const char* String::codeUnits() const noexcept
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
		m_currentConst = nullptr;
	}

	// Deep-copy from another String instance.
	void String::fromString(const String& source) noexcept
	{
		// Short representation: copy the entire packed union.
		if (source.isShort())
		{
			packedValues.firstHalf  = source.packedValues.firstHalf;
			packedValues.secondHalf = source.packedValues.secondHalf;
			m_currentConst = nullptr;
			return;
		}

		// Long representation: copy heap storage if present.
		if (source.longStr.ptr != nullptr)
		{
			const std::size_t bufferSize = static_cast<std::size_t>(source.longStr.strSize);
			if (bufferSize > 0)
			{
				char* buffer = new (std::nothrow) char[bufferSize];
				if (buffer != nullptr)
				{
					memcpy_s(buffer, bufferSize, source.longStr.ptr, bufferSize);
					longStr.tag     = source.longStr.tag;
					longStr.strSize = static_cast<std::uint32_t>(bufferSize);
					longStr.ptr     = buffer;
					m_currentConst  = nullptr;
					return;
				}
			}
			// else bufferSize is zero, treat as empty.
		}
		// else source.longStr.ptr is null, treat as empty.

		// If we reach here, copy failed: reset to empty.
		packedValues.firstHalf = 0;
		packedValues.secondHalf = 0;
		m_currentConst = nullptr;
	}

	// Returns a copy of this String (convenience method).
	String String::ToString() const noexcept
	{
		return String(*this);
	}

	// Iterator support: Set to first code point.
	const char* String::First() const noexcept
	{
		m_currentConst = codeUnits();
		return m_currentConst;
	}

	// Iterator support: one‑past‑the‑last code unit pointer.
	const char* String::Last() const noexcept
	{
	    const char* first = First();
	    const std::uint32_t size = Size();

	    // Empty or invalid buffer.
	    if (first == nullptr || size == 0)
	    {
	        return nullptr;
	    }

	    const char* p = first + size; // one beyond last byte of the string

	    // Walk backwards until we find the leading byte of the last UTF‑8 code point.
	    while (p > first)
	    {
	        --p;
	        const std::uint8_t byte = static_cast<std::uint8_t>(*p);

	        // UTF‑8 continuation bytes have the form 10xxxxxx (0x80–0xBF).
	        // Leading bytes are anything that is NOT a continuation byte.
	        if ((byte & 0xC0u) != 0x80u)
	        {
	            return p; // start of last character
	        }
	    }

	    // If we fall through, the first byte is the start (or data is malformed).
	    return nullptr;
	}

	// Iterator support: advance to the next code unit (not code point).
	const char* String::Next() const noexcept
	{
		// Not started yet
		if (m_currentConst == nullptr)
		{
			return nullptr;
		}

		const char* first = First();
		const char* last   = Last();

		// Empty or invalid buffer.
		if (first == nullptr || last == nullptr)
		{
			m_currentConst = nullptr;
			return nullptr;
		}

		// Out‑of‑range iterator state.
		if (m_currentConst < first || m_currentConst > last)
		{
			m_currentConst = nullptr;
			return nullptr;
		}

		// Move one UTF-8 code point forward.
		const std::uint8_t c = static_cast<std::uint8_t>(*m_currentConst);
		const std::uint8_t charSize = Charactor::GetCharSize(c);
		const char* next = m_currentConst + charSize;
		if (next > last)
		{
			m_currentConst = nullptr;
			return nullptr;
		}

		m_currentConst = next;
		return m_currentConst;
	}

	// Iterator support: move to the previous code unit (not code point).
	const char* String::Previous() const noexcept
	{
		// Not started yet: position at the last character.
		if (m_currentConst == nullptr)
		{
			return nullptr;
		}

		const char* first = First();
		const char* last  = Last();

		// Empty or invalid buffer.
		if (first == nullptr || last == nullptr)
		{
			m_currentConst = nullptr;
			return nullptr;
		}

		// Out‑of‑range iterator state.
		if (m_currentConst < first || m_currentConst > last)
		{
			m_currentConst = nullptr;
			return nullptr;
		}

		const char* p = m_currentConst; // scanning pointer
		// Walk backwards until we find the leading byte of the previous UTF‑8 code point.
		while (p > first)
		{
			--p;
			const std::uint8_t byte = static_cast<std::uint8_t>(*p);

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