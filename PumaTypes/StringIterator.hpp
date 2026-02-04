#pragma once

#include "Charactor.hpp"
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
        StringIterator(const uint8_t* ptr) noexcept;
        StringIterator(const StringIterator& other) noexcept;

        // Assignment
        StringIterator& operator=(const StringIterator& other) noexcept;
        StringIterator& operator=(const uint8_t* ptr) noexcept;
        StringIterator& operator=(const Charactor& charactor) noexcept;

        // Dereference - returns current UTF-8 code unit pointer
        const Charactor operator*() const noexcept;

        // Comparison operators
        bool operator==(const StringIterator& other) const noexcept;
        bool operator!=(const StringIterator& other) const noexcept;

        // Check validity
        bool IsValid() const noexcept;

    private:
        const uint8_t* utf8;
    };

} // namespace Types
} // namespace Puma