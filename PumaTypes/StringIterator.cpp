#include "pch.h"
#include "framework.h"
#include "StringIterator.hpp"
#include "String.hpp"

namespace Puma {
namespace Types
{

    // Default constructor - creates an invalid iterator
    StringIterator::StringIterator() noexcept
        : _string(nullptr), _current(nullptr)
    {
    }

    // Constructor with String and position
    StringIterator::StringIterator(const String* str, const uint8_t* ptr) noexcept
        : _string(str), _current(ptr)
    {
    }

    // Copy constructor
    StringIterator::StringIterator(const StringIterator& other) noexcept
        : _string(other._string), _current(other._current)
    {
    }

    // Copy assignment
    StringIterator& StringIterator::operator=(const StringIterator& other) noexcept
    {
        if (this != &other)
        {
            _string = other._string;
            _current = other._current;
        }
        return *this;
    }

    // Assign from pointer
    StringIterator& StringIterator::operator=(const uint8_t* ptr) noexcept
    {
        _current = ptr;
        return *this;
    }

    // Dereference operator - returns current position
    const uint8_t* StringIterator::operator*() const noexcept
    {
        return _current;
    }

    // Arrow operator - returns current position
    const uint8_t* StringIterator::operator->() const noexcept
    {
        return _current;
    }

    // Prefix increment - move to next code point
    StringIterator& StringIterator::operator++() noexcept
    {
        if (_string != nullptr && _current != nullptr)
        {
            _current = _string->Next(_current);
        }
        return *this;
    }

    // Postfix increment - move to next code point
    StringIterator StringIterator::operator++(int) noexcept
    {
        StringIterator temp(*this);
        ++(*this);
        return temp;
    }

    // Prefix decrement - move to previous code point
    StringIterator& StringIterator::operator--() noexcept
    {
        if (_string != nullptr && _current != nullptr)
        {
            _current = _string->Previous(_current);
        }
        return *this;
    }

    // Postfix decrement - move to previous code point
    StringIterator StringIterator::operator--(int) noexcept
    {
        StringIterator temp(*this);
        --(*this);
        return temp;
    }

    // Equality comparison
    bool StringIterator::operator==(const StringIterator& other) const noexcept
    {
        return _string == other._string && _current == other._current;
    }

    // Inequality comparison
    bool StringIterator::operator!=(const StringIterator& other) const noexcept
    {
        return !(*this == other);
    }

    // Check if iterator is valid
    bool StringIterator::IsValid() const noexcept
    {
        return _string != nullptr && _current != nullptr;
    }

} // namespace Types
} // namespace Puma