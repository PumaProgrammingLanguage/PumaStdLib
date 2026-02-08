#include "pch.h"
#include "framework.h"
#include "StringIterator.hpp"
#include "String.hpp"

namespace Puma {
namespace Types
{

    // Default constructor - creates an invalid iterator
    StringIterator::StringIterator() noexcept
        : utf8(nullptr)
    {
    }

    // Constructor with String
    StringIterator::StringIterator(const uint8_t* ptr) noexcept
        : utf8(ptr)
    {
    }

    // Copy constructor
    StringIterator::StringIterator(const StringIterator& other) noexcept
        : utf8(other.utf8)
    {
    }

    // Copy assignment
    StringIterator& StringIterator::operator=(const StringIterator& other) noexcept
    {
        if (this != &other)
        {
            utf8 = other.utf8;
        }
        return *this;
    }

    // Assign from pointer
    StringIterator& StringIterator::operator=(const uint8_t* ptr) noexcept
    {
        utf8 = ptr;
        return *this;
    }

    // Dereference operator - returns current position
    const Charactor StringIterator::operator*() const noexcept
    {
		Charactor charactor(utf8);
        return charactor;
    }

    // Equality comparison
    bool StringIterator::operator==(const StringIterator& other) const noexcept
    {
        return utf8 == other.utf8;
    }

    // Inequality comparison
    bool StringIterator::operator!=(const StringIterator& other) const noexcept
    {
		// Leverage equality operator above
        return !(*this == other);
    }

    // Check if iterator is valid
    bool StringIterator::IsValid() const noexcept
    {
        return utf8 != nullptr;
    }

} // namespace Types
} // namespace Puma