#pragma once

#include <cstdint>

using namespace std;

namespace Puma {
namespace Types
{
    // Forward declaration
    union String;
        
    class StringIterator
    {
    public:
        // Constructors
        StringIterator() noexcept;
        StringIterator(const String* str, const uint8_t* ptr) noexcept;
        StringIterator(const StringIterator& other) noexcept;

        // Assignment
        StringIterator& operator=(const StringIterator& other) noexcept;
        StringIterator& operator=(const uint8_t* ptr) noexcept;

        // Dereference - returns current UTF-8 code unit pointer
        const uint8_t* operator*() const noexcept;
        const uint8_t* operator->() const noexcept;

        // Increment operators - move to next UTF-8 code point
        StringIterator& operator++() noexcept;      // prefix
        StringIterator operator++(int) noexcept;    // postfix

        // Decrement operators - move to previous UTF-8 code point
        StringIterator& operator--() noexcept;      // prefix
        StringIterator operator--(int) noexcept;    // postfix

        // Comparison operators
        bool operator==(const StringIterator& other) const noexcept;
        bool operator!=(const StringIterator& other) const noexcept;

        // Check validity
        bool IsValid() const noexcept;

    private:
        const String* _string;
        const uint8_t* _current;
    };

} // namespace Types
} // namespace Puma