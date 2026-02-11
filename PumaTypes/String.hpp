#ifndef PUMA_TYPES_STRING_HPP
#define PUMA_TYPES_STRING_HPP

#pragma once

#include "Charactor.hpp"
#include "StringIterator.hpp"
#include <cstdint>
#include <cstddef>
#include <cstring>
#include <new>

using namespace std;

namespace Puma {
namespace Types
{
#pragma pack(push, 1)
    union String
    {
    public:
        // Lifetime
        String() noexcept;
        String(const String& source) noexcept;
        String(const uint8_t* data, uint32_t dataSize) noexcept;
        String(const char* data, size_t dataSize) noexcept;
        ~String() noexcept;

        String& operator=(const String& source) noexcept;

        String ToString() const noexcept;
        void FromString(const String& source) noexcept;
        // get raw UTF-8 bytes pointer
        const uint8_t* ToUTF8() const noexcept;

        // Public API
        // get str length - length in characters (code points)
        uint32_t Length() const noexcept;
        // get str size - length in bytes
        uint32_t Size() const noexcept;
        // get variable size - number of bytes used to store the variable
        uint32_t SizeVar() const noexcept;

        // iterator range support - now returns StringIterator
        StringIterator First() const noexcept;
        StringIterator Last() const noexcept;
        StringIterator Next(const StringIterator& current) const noexcept;
        StringIterator Previous(const StringIterator& current) const noexcept;

    private:
        // Layout (private)
        struct { uint8_t tag; uint8_t codeUnits[15]; } shortStr;

    #if INTPTR_MAX == INT64_MAX
        struct { uint8_t tag; uint8_t reserved[3]; uint32_t strSize; const uint8_t* ptr; } longStr;
    #elif INTPTR_MAX == INT32_MAX
        struct { uint8_t tag; uint8_t reserved[3]; uint32_t strSize; uint32_t reserved2; const uint8_t* ptr; } longStr;
    #else
    #error Unsupported pointer size
    #endif
        // copy or zero the union
        struct { uint64_t firstHalf; uint64_t secondHalf; } packedValues;

        // Masks (private) - UPPER_CASE
        static constexpr uint8_t SHORT_MASK  = 0x80;
        static constexpr uint8_t LENGTH_MASK = 0x0F;
        static constexpr uint8_t LONG_MASK   = 0x80;

        // Helpers (private) - lowerCamelCase
        bool isShort() const noexcept;
        bool isLong()  const noexcept;

        void release() noexcept;
    };
#pragma pack(pop)

} // namespace Types
} // namespace Puma

#endif // PUMA_TYPES_STRING_HPP