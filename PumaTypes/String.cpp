// PumaTypes.cpp : Defines the functions for the static library.
//
#include "pch.h"
#include "framework.h"
#include "String.hpp"
#include "StringIterator.hpp"
#include <memory.h>
#include <new>

namespace Puma {
namespace Types
{

	// Default-constructs an empty String (no heap allocation).
	String::String() noexcept
	{
		packedValues.firstHalf = 0;
		packedValues.secondHalf = 0;
	}

	// Constructs a String from raw UTF‑8 bytes and explicit length in bytes.
	String::String(const char* data, size_t dataSize) noexcept
		: String(reinterpret_cast<const uint8_t*>(data), static_cast<uint32_t>(dataSize))
	{
	}

	// Constructs a String from raw UTF‑8 bytes and explicit length in bytes.
	String::String(const uint8_t* data, uint32_t dataSize) noexcept
		: String()
	{
		if (data == nullptr || dataSize == 0)
		{
			return;
		}

		if (dataSize <= sizeof(shortStr.codeUnits))
		{
			shortStr.tag = static_cast<uint8_t>(dataSize & LENGTH_MASK);
			if (dataSize > 0)
			{
				memcpy(shortStr.codeUnits, data, dataSize);
			}
		}
		else
		{
			longStr.tag = LONG_MASK;
			longStr.strSize = static_cast<uint32_t>(dataSize);

			uint8_t* buf = new (nothrow) uint8_t[dataSize];
			if (buf != nullptr)
			{
				memcpy(buf, data, dataSize);
				longStr.ptr = buf;
			}
			else
			{
				packedValues.firstHalf = 0;
				packedValues.secondHalf = 0;
			}
		}
	}

	String::String(const String& source) noexcept
		: String()
	{
		FromString(source);
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

		FromString(source);
		return *this;
	}

	bool String::isShort() const noexcept
	{
		return (shortStr.tag & SHORT_MASK) == 0;
	}

	bool String::isLong() const noexcept
	{
		return (longStr.tag & LONG_MASK) != 0;
	}

	uint32_t String::Length() const noexcept
	{
		uint32_t charCount = 0;
		const uint8_t* ptr = isShort() ? shortStr.codeUnits : longStr.ptr;
		const uint32_t strSize = Size();

		for (uint32_t i = 0; i < strSize; )
		{
			const uint8_t c = ptr[i];
			const uint8_t charSize = Charactor::GetCharSize(c);
			i += charSize;
			++charCount;
		}

		return charCount;
	}

	uint32_t String::Size() const noexcept
	{
		return isShort()
			? static_cast<uint32_t>(shortStr.tag & LENGTH_MASK)
			: static_cast<uint32_t>(longStr.strSize);
	}

	uint32_t String::SizeVar() const noexcept
	{
		return sizeof(String);
	}

	const uint8_t* String::codeUnits() const noexcept
	{
		return isShort() ? shortStr.codeUnits : longStr.ptr;
	}

	void String::release() noexcept
	{
		if (isLong() && longStr.ptr != nullptr)
		{
			delete[] longStr.ptr;
		}

		packedValues.firstHalf = 0;
		packedValues.secondHalf = 0;
	}

	void String::FromString(const String& source) noexcept
	{
		if (source.isShort())
		{
			packedValues.firstHalf  = source.packedValues.firstHalf;
			packedValues.secondHalf = source.packedValues.secondHalf;
			return;
		}

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
		}

		packedValues.firstHalf = 0;
		packedValues.secondHalf = 0;
	}

	String String::ToString() const noexcept
	{
		return String(*this);
	}

	// Iterator support: first code unit.
	StringIterator String::First() const noexcept
	{
		return StringIterator(codeUnits(), codeUnits() + Size());
	}

	// Iterator support: iterator to first byte of last UTF‑8 character (or invalid if empty/malformed).
	StringIterator String::Last() const noexcept
	{
	    const uint8_t* first = codeUnits();
	    const uint32_t size  = Size();

	    if (size == 0 || first == nullptr)
	    {
	        return StringIterator(nullptr, nullptr);
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
	    return StringIterator(nullptr, nullptr);
	}

	// Iterator support: advance to the next UTF‑8 code point.
	StringIterator String::Next(const StringIterator& current) const noexcept
	{
		if (!current.IsValid())
		{
			return StringIterator(nullptr, nullptr);
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
			return StringIterator(nullptr, nullptr);
		}

		// Copy and move back one UTF‑8 character using the iterator's operator--().
		StringIterator prevIt = current;
		--prevIt;

		return prevIt;
	}

} // namespace Types
} // namespace Puma