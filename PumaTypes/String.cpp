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
		: m_currentConst(nullptr)
	{
		packedValues.firstHalf = 0;
		packedValues.secondHalf = 0;
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
		if (strSize <= sizeof(shortStr.codeUnits))
		{
			// short string
			shortStr.tag = static_cast<std::uint8_t>(strSize & LENGTH_MASK);
			if (strSize > 0)
			{
				memcpy_s(shortStr.codeUnits, sizeof(shortStr.codeUnits), cstr, strSize);
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
				// allocation succeeded, copy string codeUnits
				memcpy_s(buf, strSize, cstr, strSize);
				longStr.ptr = buf;
			}
			else
			{
				// allocation failed, set to empty string
				packedValues.firstHalf = 0;
				packedValues.secondHalf = 0;
			}
		}
	}

	String::String(const String& source) noexcept
		: String()
	{
		fromString(source);
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

		fromString(source);
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

    // get str length - number of characters (code points)
    std::uint32_t String::Length() const noexcept
    {
		std::uint32_t charCount = 0;
        const char* ptr;
        if (isShort())
        {
            ptr = shortStr.codeUnits;
        }
        else
        {
            ptr = longStr.ptr;
		}

		const std::uint32_t strSize = StrSize();
		for (std::uint32_t i = 0; i < strSize; )
		{
			const std::uint8_t c = static_cast<std::uint8_t>(ptr[i]);
			const std::uint8_t len = Charactor::GetCharLength(c);

			i += len;
            charCount++;
        }

        return charCount;
	}

    std::uint32_t String::StrSize() const noexcept
    {
        return isShort()
            ? static_cast<std::uint32_t>(shortStr.tag & LENGTH_MASK)
            : static_cast<std::uint32_t>(longStr.strSize);
    }

    std::uint32_t String::VarSize() const noexcept
    {
        return sizeof(String);
	}

	const char* String::codeUnits() const noexcept
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
		m_currentConst = nullptr;
	}

	void String::fromString(const String& source) noexcept
	{
		if (source.isShort())
		{
			packedValues.firstHalf = source.packedValues.firstHalf;
			packedValues.secondHalf = source.packedValues.secondHalf;
			m_currentConst = nullptr;
			return;
		}

		if (source.longStr.ptr != nullptr)
		{
			const std::size_t bufferSize = static_cast<std::size_t>(source.longStr.strSize);
			if (bufferSize > 0)
			{
				char* buffer = new (std::nothrow) char[bufferSize];
				if (buffer != nullptr)
				{
					memcpy_s(buffer, bufferSize, source.longStr.ptr, bufferSize);
					longStr.tag = source.longStr.tag;
					longStr.strSize = static_cast<std::uint32_t>(bufferSize);
					longStr.ptr = buffer;
					m_currentConst = nullptr;
					return;
				}
			}
		}

		packedValues.firstHalf = 0;
		packedValues.secondHalf = 0;
		m_currentConst = nullptr;
	}

	String String::ToString() const noexcept
	{
		return String(*this);
	}

	const char* String::BeginConst() const noexcept
	{
		m_currentConst = codeUnits();
		return m_currentConst;
	}

	const char* String::EndConst() const noexcept
	{
		return codeUnits() + StrSize();
	}

	const char* String::NextConst() const noexcept
	{
		const char* begin = codeUnits();
		const char* end = EndConst();

		if (begin == nullptr || begin == end)
		{
			m_currentConst = nullptr;
			return nullptr;
		}

		if (m_currentConst == nullptr)
		{
			m_currentConst = begin;
			return m_currentConst;
		}

		if (m_currentConst < begin || m_currentConst >= end)
		{
			m_currentConst = nullptr;
			return nullptr;
		}

		const char* next = m_currentConst + 1;
		if (next >= end)
		{
			m_currentConst = nullptr;
			return nullptr;
		}

		m_currentConst = next;
		return m_currentConst;
	}

	const char* String::PreviousConst() const noexcept
	{
		const char* begin = codeUnits();
		const char* end = EndConst();

		if (begin == nullptr || begin == end)
		{
			m_currentConst = nullptr;
			return nullptr;
		}

		if (m_currentConst == nullptr)
		{
			m_currentConst = end - 1;
			return m_currentConst;
		}

		if (m_currentConst <= begin || m_currentConst > end)
		{
			m_currentConst = nullptr;
			return nullptr;
		}

		m_currentConst = m_currentConst - 1;
		return m_currentConst;
	}

} // namespace Types
} // namespace Puma