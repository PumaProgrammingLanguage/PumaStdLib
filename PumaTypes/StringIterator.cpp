// PumaTypes/StringIterator.cpp
#include "pch.h"
#include "framework.h"
#include "StringIterator.hpp"
#include "String.hpp"

namespace Puma {
namespace Types
{

    // Default constructor - creates an invalid iterator
    StringIterator::StringIterator() noexcept
        : _current(nullptr)
        , _limit(nullptr)
    {
    }

    // Constructor with position (no limit known yet)
    StringIterator::StringIterator(const uint8_t* current, const uint8_t* limit) noexcept
        : _current(current)
        , _limit(limit)
    {
    }

    // Copy constructor
    StringIterator::StringIterator(const StringIterator& other) noexcept
        : _current(other._current)
        , _limit(other._limit)
    {
    }

    // Copy assignment
    StringIterator& StringIterator::operator=(const StringIterator& other) noexcept
    {
        if (this != &other)
        {
            _current = other._current;
            _limit   = other._limit;
        }
        return *this;
    }

    // Assign from pointer (does not change limit)
    StringIterator& StringIterator::operator=(const uint8_t* ptr) noexcept
    {
        _current = ptr;
        return *this;
    }

    // Dereference operator - returns current character
    const Charactor StringIterator::operator*() const noexcept
    {
        return Charactor(_current);
    }

    // Add raw byte offset (no UTF-8 awareness)
    StringIterator StringIterator::operator+(std::uint32_t offset) const noexcept
    {
        if (!_current)
        {
            return StringIterator(nullptr);
        }

        StringIterator result(*this);
        result._current = _current + offset;
        return result;
    }

    // Prefix increment - move forward one UTF-8 character
    StringIterator& StringIterator::operator++() noexcept
    {
        if (_current != nullptr)
        {
            const std::uint8_t firstByte = *_current;
            const std::uint8_t charSize  = Charactor::GetCharSize(firstByte);
            _current += charSize;

            // Respect optional limit if set
            if (_limit && _current >= _limit)
            {
                _current = nullptr;
            }
        }
        return *this;
    }

    // Prefix decrement - move backward one UTF-8 character
    StringIterator& StringIterator::operator--() noexcept
    {
        if (_current == nullptr)
        {
            return *this;
        }

        const uint8_t* p = _current;

        // Walk backwards until we find the leading byte of the previous UTF‑8 code point.
        // NOTE: _limit is an end pointer; we don't know the begin explicitly here.
        // Caller must ensure _current stays within a valid buffer.
        while (p > _limit)
        {
            --p;
            const std::uint8_t byte = *p;

            // UTF‑8 continuation bytes have the form 10xxxxxx (0x80–0xBF).
            // Leading bytes are anything that is NOT a continuation byte.
            if ((byte & 0xC0u) != 0x80u)
            {
                _current = p;
                return *this;
            }
        }

        // If we fall through, treat as invalid.
        _current = nullptr;
        return *this;
    }

    // Equality comparison
    bool StringIterator::operator==(const StringIterator& other) const noexcept
    {
        return _current == other._current;
    }

    // Inequality comparison
    bool StringIterator::operator!=(const StringIterator& other) const noexcept
    {
        return !(*this == other);
    }

    // Check if iterator is valid
    bool StringIterator::IsValid() const noexcept
    {
        return _current != nullptr;
    }

} // namespace Types
} // namespace Puma